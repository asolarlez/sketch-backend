#pragma once
#include "IntervalGrad.h"
#include <gsl/gsl_vector.h>
#include "BooleanNodes.h"
#include "BooleanDAG.h"
#include "NodeVisitor.h"
#include "VarStore.h"
#include <map>
#include "FloatSupport.h"

#include <iostream>

using namespace std;

#define FINE_GRAIN_RANGES 1

class RangeDiff: NodeVisitor
{
	FloatManager& floats;
	BooleanDAG& bdag;
  map<string, int> floatCtrls; // Maps float ctrl names to indices with grad vectors
  int nctrls; // number of float ctrls
	VarStore* ctrls; // Maps ctrl names to values (int abstraction for floating point values)
  vector<IntervalGrad*> ranges; // Keeps track of ranges for each node
	vector<DistanceGrad*> distances; // Keeps track of distance metric for boolean nodes
	map<int, int> inputValues; // Maps node id to values set by the SAT solver
	double error = 0.0;
	gsl_vector* errorGrad;
	
	int DEFAULT_INP = -1;
	int assertCtr;
	bool foundFailure;
	
	static constexpr float ASSERT_PENALTY = 1.0;
	
public:
	int failedAssert;

  RangeDiff(BooleanDAG& bdag_p, FloatManager& _floats, const map<string, int>& floatCtrls_p);
  ~RangeDiff(void);
  
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
  
  double run(VarStore& ctrls_p, map<int, int>& inputValues_p, gsl_vector* errorGrad_p);
	
	void setrange(bool_node& bn, IntervalGrad* r) {
		ranges[bn.id] = r;
	}
	
	IntervalGrad* r(bool_node& bn) {
		IntervalGrad* interval = ranges[bn.id];
		if (interval == NULL) {
			gsl_vector* l = gsl_vector_alloc(nctrls);
			gsl_vector* h = gsl_vector_alloc(nctrls);
			interval = new IntervalGrad(0, 0, l, h);
			setrange(bn, interval);
		}
		return interval;
	}
	
	IntervalGrad* r(bool_node* bn) {
		return r(*bn);
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
	
	void print() {
    for (int i = 0; i < bdag.size(); i++) {
      cout << bdag[i]->lprint() << endl;
			cout << r(bdag[i])->print() << endl;
			if (bdag[i]->getOtype() != OutType::FLOAT) {
				cout << d(bdag[i])->print() << endl;
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
			Assert(val == 0 || val == 1, "NYI: Integer values");
			return val;
		} else {
			return DEFAULT_INP;
		}
	}
	
	int getInputValue(bool_node* bn) {
		return getInputValue(*bn);
	}
	void computeError(float dist, int expected, gsl_vector* dg, bool_node& node, bool relax = false);
	
};
