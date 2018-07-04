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

class SmoothAutoDiff: public NodeVisitor, public SymbolicEvaluator
{
	BooleanDAG& bdag;
	map<string, int>& floatCtrls; // Maps float ctrl names to indices within grad vector
	int nctrls; // number of float ctrls
	gsl_vector* ctrls; // ctrl values
	vector<vector<ValueGrad*>> values; // Keeps track of values along with gradients for each node
	vector<vector<DistanceGrad*>> distances; // Keeps track of distance metric for boolean nodes
    Interface* inputValues;
    int MAX_CONDS = 4;
    int MAX_REGIONS = pow(2, MAX_CONDS) + 1;
    int cur_idx = 0;
    vector<int> condNodes;
public:
    int DEFAULT_INP = -32;
    SmoothAutoDiff(BooleanDAG& bdag_p, map<string, int>& floatCtrls_p);
    
	~SmoothAutoDiff(void);
	
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
	
	double dist(int nid) {
		DistanceGrad* dg = d(bdag[nid], 0);
		return dg->dist;
	}

	double dist(int nid, gsl_vector* grad) {
		DistanceGrad* dg = d(bdag[nid], 0);
		gsl_vector_memcpy(grad, dg->grad);
		return dg->dist;
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
				cout << v(bdag[i]->mother,0)->getVal() << " + " << v(bdag[i]->father, 0)->getVal() << endl;
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

	bool getAssignment(int v, int idx) {
		return v >> idx & 1;
	}
	
	
};
