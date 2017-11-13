#pragma once
#include "BooleanNodes.h"
#include "BooleanDAG.h"
#include "NodeVisitor.h"
#include "VarStore.h"
#include <map>
#include "FloatSupport.h"
#include <tuple>
#include <iostream>
#include <gsl/gsl_vector.h>
#include "Util.h"

using namespace std;


class SimpleEvaluator: NodeVisitor
{
	FloatManager& floats;
	BooleanDAG& bdag;
	map<string, int> floatCtrls; // Maps float ctrl names to indices with grad vectors
	map<string, int> boolCtrls; // Maps bool ctrl names to indices with grad vectors
	gsl_vector* ctrls; // ctrl values
	vector<double> distances; // Keeps track of distance metric for boolean nodes
	double MIN_VALUE = 0.001;

public:
  SimpleEvaluator(BooleanDAG& bdag_p, FloatManager& _floats, const map<string, int>& floatCtrls_p, const map<string, int>& boolCtrls_p);
	
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
  
  vector<tuple<double, int, int>> run(const gsl_vector* ctrls_p, map<int, int>& imap_p);
	
	void setvalue(bool_node& bn, double d) {
		distances[bn.id] = d;
	}
	
	double d(bool_node& bn) {
		return distances[bn.id];
	}
	
	double d(bool_node* bn) {
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
	
	double run1(const gsl_vector* ctrls_p, map<int, int>& inputValues_p);
	double computeError(double dist, int expected, bool_node* node);
    
    bool check(const gsl_vector* ctrls_p);
};
