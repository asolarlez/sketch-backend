#pragma once
#include <gsl/gsl_vector.h>
#include "NodeEvaluator.h"


class AutoDiff :
  public NodeEvaluator
{
  map<string, int> floatCtrls;
  vector<gsl_vector*> grads;
  gsl_vector* tmp;
  int nctrls;
  
public:
  AutoDiff(map<string, BooleanDAG*>& functionMap_p, BooleanDAG& bdag_p, FloatManager& _floats, const map<string, int>& floatCtrls_p);
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
  virtual void visit( ARRASS_node& node );
  virtual void visit( UFUN_node& node );
  virtual void visit( TUPLE_R_node& node );
  
  gsl_vector* getGrad(bool_node* bn) {
    return grads[bn->id];
  }
};