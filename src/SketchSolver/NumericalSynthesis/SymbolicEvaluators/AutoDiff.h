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

// AutoDiff through only the numerical structure - ignores all boolean structure
class AutoDiff: public NodeVisitor, public SymbolicEvaluator
{
	FloatManager& floats;
	BooleanDAG& bdag;
	map<string, int> floatCtrls; // Maps float ctrl names to indices within grad vector
	int nctrls; // number of float ctrls
	gsl_vector* ctrls; // ctrl values
	vector<ValueGrad*> values; // Keeps track of values along with gradients for each node
	map<int, int> inputValues; // Maps node id to values set by the SAT solver
	double error = 0.0;
	gsl_vector* errorGrad;
	
	int DEFAULT_INP = -1;
	
	
public:	
	AutoDiff(BooleanDAG& bdag_p, FloatManager& _floats, const map<string, int>& floatCtrls_p);
	~AutoDiff(void);
	
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
	
	virtual double errorGD(gsl_vector* errorGrad_p);
	
	virtual bool check(bool_node* n, int expected);
	//virtual set<int> getConflicts(int nid);
	
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
	
	virtual void print() {
		for (int i = 0; i < bdag.size(); i++) {
			cout << bdag[i]->lprint() << " ";
			ValueGrad* val = v(bdag[i]);
			if (val->set) {
				cout << val->print() << endl;
			} else {
				cout << "UNSET" << endl;
			}
		}
	}
	virtual void printFull() {
		for (int i = 0; i < bdag.size(); i++) {
			cout << bdag[i]->lprint() << endl;
			ValueGrad* val = v(bdag[i]);
			if (val->set) {
				cout << val->printFull() << endl;
			} else {
				cout << "UNSET" << endl;
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
	void computeError(float dist, int expected, gsl_vector* dg);
	
};
