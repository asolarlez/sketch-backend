#pragma once
#include "BooleanNodes.h"
#include "BooleanDAG.h"
#include "NodeVisitor.h"
#include "VarStore.h"
#include <map>
#include "FloatSupport.h"
#include <tuple>
#include <iostream>

using namespace std;


class SimpleEvaluator: NodeVisitor
{
	FloatManager& floats;
	BooleanDAG& bdag;
	VarStore* ctrls; // Maps ctrl names to values (int abstraction for floating point values)
	vector<float> distances; // Keeps track of distance metric for boolean nodes
	float MIN_VALUE = 0.001;

public:
  SimpleEvaluator(BooleanDAG& bdag_p, FloatManager& _floats);
	
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
  
  vector<tuple<float, int, int>> run(VarStore& ctrls_p, map<int, int>& imap_p);
	
	void setvalue(bool_node& bn, float d) {
		distances[bn.id] = d;
	}
	
	float d(bool_node& bn) {
		return distances[bn.id];
	}
	
	float d(bool_node* bn) {
		return d(*bn);
	}
	
	void print() {
    for (int i = 0; i < bdag.size(); i++) {
      cout << bdag[i]->lprint() << endl;
			cout << d(bdag[i]) << endl;
		}
  }
	
	bool isFloat(bool_node& bn) {
		return (bn.getOtype() == OutType::FLOAT);
	}
	
	bool isFloat(bool_node* bn) {
		return (bn->getOtype() == OutType::FLOAT);
	}
	
	double run1(VarStore& ctrls_p, map<int, int>& inputValues_p);
	double computeError(float dist, int expected, bool_node* node);
};
