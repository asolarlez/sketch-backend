#pragma once
#include <gsl/gsl_vector.h>
#include "NodeEvaluator.h"


class RangeDiff :
public NodeEvaluator
{
  map<string, int> floatCtrls;
  vector<gsl_vector*> lgrads;
  vector<gsl_vector*> hgrads;
  gsl_vector* tmp;
  int nctrls;
  vector<pair<int, int>> ranges;
  
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
	
	void findMinMax(bool_node& node, vector<float>& vals, vector<gsl_vector*>& grads);
  
  gsl_vector* getLGrad(bool_node* bn) {
    return lgrads[bn->id];
  }
  
  gsl_vector* getHGrad(bool_node* bn) {
    return hgrads[bn->id];
  }
  
  void setrange(bool_node& bn, int i){
    ranges[bn.id] = make_pair(i,i);
  }
  
  void setrange(bool_node& bn, int i, int j) {
    ranges[bn.id] = make_pair(i,j);
  }
  
  pair<int, int> r(bool_node& bn) {
    return ranges[bn.id];
  }
  pair<int, int> r(bool_node* bn) {
    return ranges[bn->id];
  }
  void print() {
    for (int i = 0; i < bdag.size(); i++) {
      cout << bdag[i]->lprint() << endl;
			pair<int,int> mrange = r(bdag[i]);
			if ((bdag[i])->getOtype() == OutType::FLOAT) {
				cout << "Range: " << floats.getFloat(mrange.first) << " " << floats.getFloat(mrange.second) << endl;
			} else {
				cout << "Range: " << mrange.first << " " << mrange.second << endl;
			}
			gsl_vector* lgrads = getLGrad((bdag[i]));
			cout << "LGrads: ";
			for (int i = 0; i < lgrads->size; i++) {
				cout << gsl_vector_get(lgrads, i) << ", ";
			}
			cout << endl;
			gsl_vector* hgrads = getHGrad((bdag[i]));
			cout << "HGrads: " ;
			for (int i = 0; i < hgrads->size; i++) {
				cout << gsl_vector_get(hgrads, i) << ", ";
			}
			cout << endl;
		}
  }
};
