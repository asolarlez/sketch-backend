#pragma once
#include <gsl/gsl_vector.h>
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

class BoolAutoDiff: public NodeVisitor, public SymbolicEvaluator
{
	BooleanDAG& bdag;
	map<string, int> floatCtrls; // Maps float ctrl names to indices within grad vector
    map<string, int> boolCtrls;
	int nctrls; // number of float ctrls
	gsl_vector* ctrls; // ctrl values
	vector<ValueGrad*> values; // Keeps track of values along with gradients for each node
	vector<DistanceGrad*> distances; // Keeps track of distance metric for boolean nodes
    Interface* inputValues;
	double error = 0.0;
	gsl_vector* errorGrad;
		
public:
    int DEFAULT_INP = -32;
    BoolAutoDiff(BooleanDAG& bdag_p, const map<string, int>& floatCtrls_p, const map<string, int>& boolCtrls_p);
    
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
	
    virtual void setInputs(const Interface* inputValues_p);

	virtual void run(const gsl_vector* ctrls_p, const set<int>& nodesSubset);
    virtual void run(const gsl_vector* ctrls_p);
    
    virtual double getErrorOnConstraint(int nodeid, gsl_vector* grad);
    double getSqrtError(bool_node* node, gsl_vector* grad);
    double getAssertError(bool_node* node, gsl_vector* grad);
    double getBoolCtrlError(bool_node* node, gsl_vector* grad);
    
    virtual double getErrorOnConstraint(int nodeid);
    double getSqrtError(bool_node* node);
    double getAssertError(bool_node* node);
    double getBoolCtrlError(bool_node* node);


    
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
	
	
};
