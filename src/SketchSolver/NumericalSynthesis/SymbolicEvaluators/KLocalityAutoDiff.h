#pragma once

#ifndef _NOGSL
#include <gsl/gsl_vector.h>
#else
#include "CustomSolver.h"
#endif
#include "ValueGrad.h"
#include "BooleanNodes.h"
#include "BooleanDAG.h"
#include "NodeVisitor.h"
#include "VarStore.h"
#include <map>
#include <set>
#include "SymbolicEvaluator.h"
#include "Interface.h"

#include <iostream>


using namespace std;

class Path {
public:
	map<int, int> nodeToVal;

	Path() { }
	void add(map<int, int>& initMap) {
		nodeToVal.insert(initMap.begin(), initMap.end());
	}
	void addCond(int nodeid, int val) {
		Assert(nodeToVal.find(nodeid) == nodeToVal.end(), "Node already has value");
		nodeToVal[nodeid] = val;
	}

	string toString() {
		stringstream s;
		for (auto it = nodeToVal.begin(); it != nodeToVal.end(); it++) {
			s << "(" << it->first << "," << it->second << ");";
		}
		return s.str();
	}

	int getVal(int nodeid) {
		if (nodeToVal.find(nodeid) == nodeToVal.end()) {
			return -1;
		} else {
			return nodeToVal[nodeid];
		}
	}

	void empty() {
		nodeToVal.clear();
	}

	int length() {
		return nodeToVal.size();
	}

	int lowestId() {
		if (nodeToVal.size() == 0) {
			return -1;
		}
		return nodeToVal.begin()->first;
	}
	
	static bool isCompatible(Path* p1, Path* p2) {
		int size1 = p1->nodeToVal.size();
		int size2 = p2->nodeToVal.size();
		if (size1 == 0 || size2 == 0) {
			return true;
		}
		Path* pl;
		Path* ps;
		if (size1 > size2) {
			pl = p1;
			ps = p2;
		} else {
			pl = p2;
			ps = p1;
		}
		for (auto it = pl->nodeToVal.begin(); it != pl->nodeToVal.end(); it++) {
			int nodeid = it->first;
			int val = it->second;
			int val1 = ps->getVal(nodeid);
			if (val1 != -1 && val != val1) {
				return false;
			}
		}
		return true;
	}

	static void combinePaths(Path* p1, Path* p2, Path* p) {
		// assumes that paths are compatible
		p->nodeToVal.clear();
		p->add(p1->nodeToVal);
		p->add(p2->nodeToVal);
	}

	static void combinePath(Path* p1, Path* p) {
		p->nodeToVal.clear();
		p->add(p1->nodeToVal);
	}

	static void combinePaths(vector<Path*>& paths, Path* p2, Path* p) {
		p->nodeToVal.clear();
		p->add(p2->nodeToVal);
		// TODO: need to add common conditions in paths to p
	}

	static Path* pIntersect(Path* p1, Path* p2) {
		// assumes that the paths are compatible
		Path* p = new Path();
		p->add(p1->nodeToVal);
		p->add(p2->nodeToVal);
		return p;
	}

	// only one cond  should differ
	static int isConjugate(Path* p1, Path* p2) {
		//cout << "Conjugate check: " << p1->toString() << " | " << p2->toString() << endl;
		if (p1->length() != p2->length()) {
			return -1;
		}
		int conjIdx = -1;
		int ctr = 0;
		for (auto it = p1->nodeToVal.begin(); it != p1->nodeToVal.end(); it++) {
			if (p2->getVal(it->first) == -1) {
				return -1;
			}
			if (p2->getVal(it->first) != it->second) {
				if (conjIdx == -1) {
					conjIdx = ctr;
				} else {
					return -1;
				}
			}
			ctr++;
		}
		//cout << conjIdx << endl;
		return conjIdx;
	}

	static Path* pUnion(Path* p1, Path* p2) {
		// assumes that the paths are conjugates
		Path* p = new Path();
		for (auto it = p1->nodeToVal.begin(); it != p1->nodeToVal.end(); it++) {
			if (p2->getVal(it->first) == it->second) {
				p->addCond(it->first, it->second);
			}
			
		}
		return p;

	}

	static bool isSubset(Path* p1, Path* p2) {
		if (p1->length() == 0) return true;
		for (auto it = p1->nodeToVal.begin(); it != p1->nodeToVal.end(); it++) {
			if (p2->getVal(it->first) != it->second) {
				return false;
			}
		}
		return true;

	}

	// no common condition
	static bool isDisjoint(Path* p1, Path* p2) {
		if (p1->length() == 0 || p2->length() == 0) return false;
		for (auto it = p1->nodeToVal.begin(); it != p1->nodeToVal.end(); it++) {
			if (p2->getVal(it->first) != -1) {
				return false;
			}
		}
		return true;
	}
};

class MergeNodesList {
	vector<tuple<int, int, int, int>> nodesToMerge;

public:
	void add(int node1, int idx1, int node2, int idx2) {
		nodesToMerge.push_back(make_tuple(node1, idx1, node2, idx2));
	}

	string print() {
		stringstream s;
		for (int i = 0; i < nodesToMerge.size(); i++) {
			auto& t = nodesToMerge[i];
			s << "(" << get<0>(t) << "," << get<1>(t) << "," << get<2>(t) << "," << get<3>(t) << ") ";
		}
		return s.str();
	}

	int size() {
		return nodesToMerge.size();
	}

	tuple<int, int, int, int>& getTuple(int idx) {
		return nodesToMerge[idx];
	}
};


class KLocalityAutoDiff: public NodeVisitor, public SymbolicEvaluator
{
	BooleanDAG& bdag;
	map<string, int>& floatCtrls; // Maps float ctrl names to indices within grad vector
	int nctrls; // number of float ctrls
	gsl_vector* ctrls; // ctrl values
	vector<vector<ValueGrad*>> values; // Keeps track of values along with gradients for each node
	vector<vector<DistanceGrad*>> distances; // Keeps track of distance metric for boolean nodes
	vector<vector<Path*>> paths;
	vector<int> sizes;
    Interface* inputValues;

    set<string> pathStrings;


    int MAX_REGIONS = 16 + 1; // 0th idx is for final merging

    vector<vector<MergeNodesList*>> mergeNodes;

public:
    int DEFAULT_INP = -32;
    KLocalityAutoDiff(BooleanDAG& bdag_p, map<string, int>& floatCtrls_p);
    
	~KLocalityAutoDiff(void);
	
	virtual void visit( SRC_node& node );
	virtual void visit( DST_node& node );
	virtual void visit( CTRL_node& node );
	virtual void visit( PLUS_node& node );
	virtual void visit( TIMES_node& node );
	virtual void visit( ARRACC_node& node );
	virtual void visit( DIV_node& node );
	virtual void visit( MOD_node& node );
	virtual void visit( NEG_node& node );
	virtual void visit( CONST_node& node );
	virtual void visit( LT_node& node );
	virtual void visit( EQ_node& node );
	virtual void visit( AND_node& node );
	virtual void visit( OR_node& node );
	virtual void visit( NOT_node& node );
	virtual void visit( ARRASS_node& node );
	virtual void visit( UFUN_node& node );
	virtual void visit( TUPLE_R_node& node );
	virtual void visit( ASSERT_node& node );
	
    virtual void setInputs(Interface* inputValues_p);

	virtual void run(const gsl_vector* ctrls_p, const set<int>& nodesSubset) {
		Assert(false, "Not yet supported");
	}
    virtual void run(const gsl_vector* ctrls_p);
    
    virtual double getErrorOnConstraint(int nodeid, gsl_vector* grad);
    double getSqrtError(bool_node* node, gsl_vector* grad);
    double getAssertError(bool_node* node, gsl_vector* grad);
    double getBoolCtrlError(bool_node* node, gsl_vector* grad);
    double getBoolExprError(bool_node* node, gsl_vector* grad);
    
    virtual double getErrorOnConstraint(int nodeid);
    double getSqrtError(bool_node* node);
    double getAssertError(bool_node* node);
    double getBoolCtrlError(bool_node* node);
    double getBoolExprError(bool_node* node);

    virtual double getErrorForAsserts(const set<int>& assertIds, gsl_vector* grad) {
    	Assert(false, "TODO");
    }

	virtual double getErrorForAssert(int assertId, gsl_vector* grad) {
		Assert(false, "TODO");
	}
    

	void setvalue(bool_node& bn, int idx, ValueGrad* v) {
		values[bn.id][idx] = v;
	}
	
	ValueGrad* v(bool_node& bn, int idx) {
		ValueGrad* val = values[bn.id][idx];
		if (val == NULL) {
			gsl_vector* g = gsl_vector_alloc(nctrls);
			GradUtil::default_grad(g);
			val = new ValueGrad(0, g);
			setvalue(bn, idx, val);
		}
		return val;
	}
	
	ValueGrad* v(bool_node* bn, int idx) {
		return v(*bn, idx);
	}
	
	void setdistance(bool_node& bn, int idx, DistanceGrad* d) {
		distances[bn.id][idx] = d;
	}
	
	DistanceGrad* d(bool_node& bn, int idx) {
		DistanceGrad* dist = distances[bn.id][idx];
		if (dist == NULL) {
			gsl_vector* g = gsl_vector_alloc(nctrls);
			GradUtil::default_grad(g);
			dist = new DistanceGrad(0, g);
			setdistance(bn, idx, dist);
		}
		return dist;
	}
	
	DistanceGrad* d(bool_node* bn, int idx) {
		return d(*bn, idx);
	}

	int size(bool_node& bn) {
		return sizes[bn.id];
	}

	int size(bool_node* bn) {
		return size(*bn);
	}

	void setsize(bool_node& bn, int sz) {
		sizes[bn.id] = sz;
	}

	void setsize(bool_node* n, int sz) {
		setsize(*n, sz);
	}

	void setpath(bool_node& bn, int idx, Path* p) {
		paths[bn.id][idx] = p;
	}

	Path* path(bool_node& bn, int idx) {
		Path* p = paths[bn.id][idx];
		if (p == NULL) {
			p = new Path();
			setpath(bn, idx, p);
		}
		return p;
	}

	Path* path(bool_node* bn, int idx) {
		return path(*bn, idx);
	}

	
	virtual double dist(int nid) {
		ValueGrad* val = v(bdag[nid], 0);
		return val->getVal();
	}

	virtual double dist(int nid, gsl_vector* grad) {
		ValueGrad* val = v(bdag[nid], 0);
		gsl_vector_memcpy(grad, val->getGrad());
		return val->getVal();
	}

	virtual void print() {
		/*for (int i = 0; i < bdag.size(); i++) {
			if (bdag[i]->type == bool_node::ASSERT) {
				DistanceGrad* dist = d(bdag[i]->mother);
				if (dist->set) {
				double gmag = gsl_blas_dnrm2(dist->grad);
				if (dist->dist < 0.1) {
					cout << bdag[i]->mother->lprint() << endl;
					cout << dist->printFull() << endl;
				}
				}
			}
		}
		return;*/
		for (int i = 0; i < bdag.size(); i++) {
			cout << bdag[i]->lprint() << " ";
			if (bdag[i]->getOtype() == OutType::FLOAT) {
				if (v(bdag[i], 0)->set) {
					cout << v(bdag[i], 0)->print() << endl;
				} else {
					cout << "UNSET" << endl;
				}
			} else {
				if (d(bdag[i], 0)->set) {
					cout << d(bdag[i], 0)->print() << endl;
				} else {
					cout << "UNSET" << endl;
				}
			}
		}
	}
	virtual void printFull() {
		for (int i = 0; i < bdag.size(); i++) {
			cout << bdag[i]->lprint() << endl;
			if (bdag[i]->getOtype() == OutType::FLOAT) {
				if (v(bdag[i],0)->set) {
					cout << v(bdag[i],0)->printFull() << endl;
				} else {
					cout << "UNSET" <<endl;
				}
			} else {
				if(d(bdag[i],0)->set) {
					cout << d(bdag[i],0)->printFull() << endl;
				} else {
					cout << "UNSET" << endl;
				}
			}
			if (bdag[i]->type == bool_node::PLUS) {
				cout << v(bdag[i]->mother(),0)->getVal() << " + " << v(bdag[i]->father(), 0)->getVal() << endl;
			}
		}
	}
	
	bool isFloat(bool_node& bn) {
		return (bn.getOtype() == OutType::FLOAT);
	}
	
	bool isFloat(bool_node* bn) {
		return (bn->getOtype() == OutType::FLOAT);
	}
	
	int getInputValue(bool_node& bn) {
		if (inputValues->hasValue(bn.id)) {
			int val = inputValues->getValue(bn.id);
			return val;
		} else {
			return DEFAULT_INP;
		}
	}
	
	int getInputValue(bool_node* bn) {
		return getInputValue(*bn);
	}


	void doUfun(UFUN_node& node, ValueGrad* mval, ValueGrad* val);
	
	void copyNodes(bool_node& node, bool_node* m);
	double merge(bool_node* n, gsl_vector* grad);

	void mergePaths(vector<tuple<double, vector<tuple<int, int, int>>, Path*>>& mergedPaths);
	void getMergeNodesBinop(bool_node* n);
	void getMergeNodesUnop(bool_node* n, bool_node* m);
	void getMergeNodesArracc(bool_node* node);
	void getMergeNodes();

	void combineDistance(bool_node& node, int nodeid1, int idx1, int nodeid2, int idx2, double& dist, gsl_vector* dg_grad);
	void doPair(bool_node& node, int nodeid1, int idx1, int nodeid2, int idx2, double& val, gsl_vector* val_grad);
	void pairNodes(bool_node& node);
	void normalize(bool_node* n);

};
