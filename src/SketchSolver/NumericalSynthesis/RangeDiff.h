#pragma once
#include "NodeEvaluator.h"
#include "IntervalGrad.h"
#include <gsl/gsl_vector.h>


class RangeDiff :
public NodeEvaluator
{
  map<string, int> floatCtrls;
  int nctrls;
  vector<IntervalGrad*> ranges;
  
public:
  RangeDiff(map<string, BooleanDAG*>& functionMap_p, BooleanDAG& bdag_p, FloatManager& _floats, const map<string, int>& floatCtrls_p);
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
  virtual void visit( ARRASS_node& node );
  virtual void visit( UFUN_node& node );
  virtual void visit( TUPLE_R_node& node );
  
  bool run(VarStore& inputs_p);
	
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
	
	void print() {
    for (int i = 0; i < bdag.size(); i++) {
      cout << bdag[i]->lprint() << endl;
			cout << r(bdag[i])->print() << endl;
		}
  }
	
	bool isFloat(bool_node& bn) {
		return (bn.getOtype() == OutType::FLOAT);
	}
};
