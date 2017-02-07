#include "RangeDiff.h"
#include <algorithm>
#include <limits>
#include <math.h>


RangeDiff::RangeDiff(map<string, BooleanDAG*>& functionMap_p, BooleanDAG& bdag_p, FloatManager& _floats, const map<string, int>& floatCtrls_p):NodeEvaluator(functionMap_p, bdag_p, _floats), floatCtrls(floatCtrls_p) {
  ranges.resize(bdag.size(), NULL);
  nctrls = floatCtrls_p.size();
	IntervalGrad::tmp = gsl_vector_alloc(nctrls);
}

RangeDiff::~RangeDiff(void) {
  for (int i = 0; i < ranges.size(); i++) {
		if (ranges[i] != NULL) {
			delete ranges[i];
		}
	}
	delete IntervalGrad::tmp;
}

void RangeDiff::visit( SRC_node& node ) { //TODO: deal with array src nodes
  //cout << "Visiting SRC node" << endl;
  string name = node.get_name();
  if (inputs->contains(name)) {
		float val;
		int ival = (*inputs)[name];
		if (isFloat(node)) {
			val = floats.getFloat(ival);
		} else {
			val = (float) ival;
		}
		IntervalGrad* interval = r(node);
		interval->update(val, val);
		gsl_vector* l = interval->getLGrad();
		gsl_vector* h = interval->getHGrad();
		for (int i = 0; i < nctrls; i++) {
			gsl_vector_set(l, i, 0.0);
			gsl_vector_set(h, i, 0.0);
		}
	}
}

void RangeDiff::visit( DST_node& node ) {
  //cout << "Visiting DST node" << endl;
}

void RangeDiff::visit( CTRL_node& node ) {
  //cout << "Visiting CTRL node" << endl;
	float val;
  int ival = (*inputs)[node.get_name()];
	if (isFloat(node)) {
		val = floats.getFloat(ival);
	} else {
		val = (float) ival;
	}
  int idx = - 1;
  string name = node.get_name();
	if (floatCtrls.find(name) != floatCtrls.end()) {
    idx = floatCtrls[name];
	}
	IntervalGrad* interval = r(node);
	interval->update(val, val);
	gsl_vector* l = interval->getLGrad();
	gsl_vector* h = interval->getHGrad();
  for (int i = 0; i < nctrls; i++) {
    if (i == idx) {
      gsl_vector_set(l, i, 1.0);
      gsl_vector_set(h, i, 1.0);
    } else {
      gsl_vector_set(l, i, 0.0);
      gsl_vector_set(h, i, 0.0);
    }
  }
}

void RangeDiff::visit( PLUS_node& node ) {
  //cout << "Visiting PLUS node" << endl;
	IntervalGrad* interval = r(node);
	IntervalGrad* minterval = r(node.mother);
	IntervalGrad* finterval = r(node.father);
	IntervalGrad::ig_plus(minterval, finterval, interval);
}

void RangeDiff::visit( TIMES_node& node ) {
  //cout << "Visiting TIMES node" << endl;
	IntervalGrad* interval = r(node);
	IntervalGrad* minterval = r(node.mother);
	IntervalGrad* finterval = r(node.father);
	IntervalGrad::ig_times(minterval, finterval, interval);
}

void RangeDiff::visit( ARRACC_node& node ) {
  //cout << "Visiting ARRACC node" << endl;
  Assert(node.mother->type == bool_node::SRC, "Something is wrong");
  SRC_node* inode = dynamic_cast<SRC_node*>(node.mother);
  if (!(*inputs).contains(inode->name)) {
    // take union of the two intervals
		IntervalGrad* interval = r(node);
		vector<IntervalGrad*> mIntervals;
		for (int i = 0; i < node.multi_mother.size(); i++) {
			mIntervals.push_back(r(node.multi_mother[i]));
		}
		IntervalGrad::ig_union(mIntervals, interval);
	} else {
		IntervalGrad* minterval = r(node.mother);
    Assert(minterval->getLow()  == minterval->getHigh(), "Something is wrong");
    int idx = minterval->getLow();
    if (idx < node.multi_mother.size() && idx >= 0) {
			IntervalGrad* interval = r(node);
			IntervalGrad* finterval = r(node.multi_mother[idx]);
			IntervalGrad::ig_copy(finterval, interval);
		} else {
			Assert(false, "ARRACC: out of bounds");
		}
  }
}

void RangeDiff::visit( DIV_node& node ) {
  //cout << "Visiting DIV node" << endl;
	IntervalGrad* interval = r(node);
	IntervalGrad* minterval = r(node.mother);
	IntervalGrad* finterval = r(node.father);
	IntervalGrad::ig_div(minterval, finterval, interval);
}

void RangeDiff::visit( MOD_node& node ) {
  cout << "Visiting MOD node" << endl;
  Assert(false, "NYI");
}

void RangeDiff::visit( NEG_node& node ) {
  //cout << "Visiting NEG node" << endl;
	IntervalGrad* interval = r(node);
	IntervalGrad* minterval = r(node.mother);
	IntervalGrad::ig_neg(minterval, interval);
}

void RangeDiff::visit( CONST_node& node ) {
  //cout << "Visiting CONST node" << endl;
  float val;
  if (node.isFloat()) {
    val = node.getFval();
  } else {
    val = (float) node.getVal();
  }
	IntervalGrad* interval = r(node);
	interval->update(val, val);
  gsl_vector* l = interval->getLGrad();
  gsl_vector* h = interval->getHGrad();
  for (int i = 0; i < nctrls; i++) {
    gsl_vector_set(l, i, 0.0);
    gsl_vector_set(h, i, 0.0);
  }
}

void RangeDiff::visit( LT_node& node ) {
  //cout << "Visiting LT node" << endl;
}

void RangeDiff::visit( EQ_node& node ) {
  //cout << "Visiting EQ node" << endl;
}

void RangeDiff::visit( ARRASS_node& node ) {
  cout << "Visiting ARRASS node" << endl;
  Assert(false, "NYI");
}

void RangeDiff::visit( UFUN_node& node ) {
	IntervalGrad* interval = r(node);
	IntervalGrad* minterval = r(node.multi_mother[0]);
	
	const string& name = node.get_ufname();
	if (name == "_cast_int_float_math") {
		IntervalGrad::ig_cast_int_float(minterval, interval);
	} else if (floats.hasFun(name)) {
		if (name == "arctan_math") {
			IntervalGrad::ig_arctan(minterval, interval);
		} else if (name == "sin_math") {
			IntervalGrad::ig_sin(minterval, interval);
		} else if (name == "cos_math") {
			IntervalGrad::ig_cos(minterval, interval);
		} else if (name == "tan_math") {
			IntervalGrad::ig_tan(minterval, interval);
		} else if (name == "sqrt_math") {
			IntervalGrad::ig_sqrt(minterval, interval);
		} else {
			Assert(false, "NYI");
		}
	} else {
		Assert(false, "NYI");
	}
}

void RangeDiff::visit( TUPLE_R_node& node) {
	if (node.mother->type == bool_node::UFUN) {
    Assert(((UFUN_node*)(node.mother))->multi_mother.size() == 1, "NYI"); // TODO: This assumes that the ufun has a single output
		IntervalGrad* minterval = r(node.mother);
		IntervalGrad* interval = r(node);
		IntervalGrad::ig_copy(minterval, interval);
	} else {
    Assert(false, "NYI");
  }
}

bool RangeDiff::run(VarStore& inputs_p) {
  funargs.clear();
  inputs = &inputs_p;
  int i=0;
  failedAssert = false;
  failedHAssert = false;
  for(BooleanDAG::iterator node_it = bdag.begin(); node_it != bdag.end(); ++node_it, ++i){
    (*node_it)->accept(*this);
		if(failedAssert){
      return true;
    }
    if(failedHAssert){
      return false;
    }
  }
  return false;
}

