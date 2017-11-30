#pragma once
#include <gsl/gsl_vector.h>
#include "ValueGrad.h"
#include "BooleanNodes.h"
#include "BooleanDAG.h"
#include "NodeVisitor.h"
#include "VarStore.h"
#include <map>
#include "FloatSupport.h"
#include "SymbolicEvaluator.h"

#include <iostream>


using namespace std;

class BoolAutoDiff: public NodeVisitor, public SymbolicEvaluator
{
	FloatManager& floats;
	BooleanDAG& bdag;
	map<string, int> floatCtrls; // Maps float ctrl names to indices within grad vector
    map<string, int> boolCtrls;
	int nctrls; // number of float ctrls
	gsl_vector* ctrls; // ctrl values
	vector<ValueGrad*> values; // Keeps track of values along with gradients for each node
	vector<DistanceGrad*> distances; // Keeps track of distance metric for boolean nodes
	map<int, int> inputValues; // Maps node id to values set by the SAT solver
	double error = 0.0;
	gsl_vector* errorGrad;
	
	int DEFAULT_INP = -1;
	
public:
	int failedAssert;
	
	BoolAutoDiff(BooleanDAG& bdag_p, FloatManager& _floats, const map<string, int>& floatCtrls_p, const map<string, int>& boolCtrls_p);
	~BoolAutoDiff(void);
	
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
	
	virtual void run(const gsl_vector* ctrls_p, const map<int, int>& inputValues_p);
    virtual bool checkAll(const gsl_vector* ctrls_p, const map<int, int>& inputValues_p);
	virtual bool check(bool_node* n, int expected);
	virtual double computeError(bool_node* n, int expected, gsl_vector* errorGrad);
	virtual double computeDist(bool_node*, gsl_vector* distgrad);
	virtual bool hasDist(bool_node* n);
    virtual double computeVal(bool_node*, gsl_vector* distgrad);
    virtual bool hasVal(bool_node* n);
    virtual bool hasSqrtDist(bool_node* n);
    virtual double computeSqrtError(bool_node* n, gsl_vector* errorGrad);
    virtual double computeSqrtDist(bool_node* n, gsl_vector* errorGrad);
    virtual double getVal(bool_node* n);
    virtual gsl_vector* getGrad(bool_node* n);
    
	void setvalue(bool_node& bn, ValueGrad* v) {
		values[bn.id] = v;
	}
	
	ValueGrad* v(bool_node& bn) {
		ValueGrad* val = values[bn.id];
		if (val == NULL) {
			gsl_vector* g = gsl_vector_alloc(nctrls);
			val = new ValueGrad(0, g);
			setvalue(bn, val);
		}
		return val;
	}
	
	ValueGrad* v(bool_node* bn) {
		return v(*bn);
	}
	
	void setdistance(bool_node& bn, DistanceGrad* d) {
		distances[bn.id] = d;
	}
	
	DistanceGrad* d(bool_node& bn) {
		DistanceGrad* dist = distances[bn.id];
		if (dist == NULL) {
			gsl_vector* g = gsl_vector_alloc(nctrls);
			dist = new DistanceGrad(0, g);
			setdistance(bn, dist);
		}
		return dist;
	}
	
	DistanceGrad* d(bool_node* bn) {
		return d(*bn);
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
				if (v(bdag[i])->set) {
					cout << v(bdag[i])->print() << endl;
				} else {
					cout << "UNSET" << endl;
				}
			} else {
				if (d(bdag[i])->set) {
					cout << d(bdag[i])->print() << endl;
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
				if (v(bdag[i])->set) {
					cout << v(bdag[i])->printFull() << endl;
				} else {
					cout << "UNSET" <<endl;
				}
			} else {
				if(d(bdag[i])->set) {
					cout << d(bdag[i])->printFull() << endl;
				} else {
					cout << "UNSET" << endl;
				}
			}
			if (bdag[i]->type == bool_node::PLUS) {
				cout << v(bdag[i]->mother)->getVal() << " + " << v(bdag[i]->father)->getVal() << endl;
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
		if (inputValues.find(bn.id) != inputValues.end()) {
			int val = inputValues[bn.id];
			//Assert(val == 0 || val == 1, "NYI: Integer values");
			return val;
		} else {
			return DEFAULT_INP;
		}
	}
	
	int getInputValue(bool_node* bn) {
		return getInputValue(*bn);
	}
	
	
};
