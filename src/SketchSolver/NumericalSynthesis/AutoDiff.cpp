#include "AutoDiff.h"

AutoDiff::AutoDiff(map<string, BooleanDAG*>& functionMap_p, BooleanDAG& bdag_p, FloatManager& _floats, const map<string, int>& floatCtrls_p):NodeEvaluator(functionMap_p, bdag_p, _floats), floatCtrls(floatCtrls_p) {
  grads.resize(bdag.size(), NULL);
  nctrls = floatCtrls_p.size();
  for (int i = 0; i < grads.size(); i++) {
    grads[i] = gsl_vector_alloc(nctrls);
  }
  tmp = gsl_vector_alloc(nctrls);
}
AutoDiff::~AutoDiff(void) {
  for (int i = 0; i < grads.size(); i++) {
    if (grads[i] != NULL) {
      delete grads[i];
    }
  }
  delete tmp;
}

void AutoDiff::visit( SRC_node& node ) {
  //cout << "Visiting SRC node" << endl;
  NodeEvaluator::visit(node);
  gsl_vector* g = grads[node.id];
  for (int i = 0; i < nctrls; i++) {
    gsl_vector_set(g, i, 0.0);
  }
  
}
void AutoDiff::visit( DST_node& node ) {
  //cout << "Visiting DST node" << endl;
  NodeEvaluator::visit(node);
}

void AutoDiff::visit( CTRL_node& node ) {
  //cout << "Visiting CTRL node" << endl;
  NodeEvaluator::visit(node);
  int idx = - 1;
  string name = node.get_name();
  if (floatCtrls.find(name) != floatCtrls.end())
    idx = floatCtrls[name];
  //cout << name << " " << idx << " " << floats.getFloat(getValue(node)) << endl;
  gsl_vector* g = grads[node.id];
  for (int i = 0; i < nctrls; i++) {
    if (i == idx) {
      gsl_vector_set(g, i, 1.0);
    } else {
      gsl_vector_set(g, i, 0.0);
    }
  }
}
void AutoDiff::visit( PLUS_node& node ) {
  //cout << "Visiting PLUS node" << endl;
  NodeEvaluator::visit(node);
  gsl_vector* mgrads = grads[node.mother->id];
  gsl_vector* fgrads = grads[node.father->id];
  gsl_vector* g = grads[node.id];
  gsl_vector_memcpy(g, mgrads);
  gsl_vector_add(g, fgrads);

}
void AutoDiff::visit( TIMES_node& node ) {
  //cout << "Visiting TIMES node" << endl;
  NodeEvaluator::visit(node);
  gsl_vector* mgrads = grads[node.mother->id];
  gsl_vector* fgrads = grads[node.father->id];
  gsl_vector* g = grads[node.id];
  gsl_vector_memcpy(g, mgrads);
  gsl_vector_memcpy(tmp, fgrads);
  float mval, fval;
  if (node.getOtype() == OutType::FLOAT) {
    mval = floats.getFloat(i(*node.mother));
    fval = floats.getFloat(i(*node.father));
  } else {
    mval = i(*node.mother);
    fval = i(*node.father);
  }
  gsl_vector_scale(g, fval);
  gsl_vector_scale(tmp, mval);
  gsl_vector_add(g, tmp);
}
void AutoDiff::visit( ARRACC_node& node ) {
  //cout << "Visiting ARRACC node" << endl;
  NodeEvaluator::visit(node);
  int idx = i(*node.mother);
  if( idx < node.multi_mother.size() && idx >= 0){
    gsl_vector* g = grads[node.id];
    gsl_vector* m = grads[node.multi_mother[idx]->id];
    gsl_vector_memcpy(g, m);
  }
}
void AutoDiff::visit( DIV_node& node ) {
  //cout << "Visiting DIV node" << endl;
  NodeEvaluator::visit(node);
  gsl_vector* mgrads = grads[node.mother->id];
  gsl_vector* fgrads = grads[node.father->id];
  gsl_vector* g = grads[node.id];
  gsl_vector_memcpy(g, mgrads);
  gsl_vector_memcpy(tmp, fgrads);
  float mval, fval;
  if (node.getOtype() == OutType::FLOAT) {
    mval = floats.getFloat(i(*node.mother));
    fval = floats.getFloat(i(*node.father));
  } else {
    mval = i(*node.mother);
    fval = i(*node.father);
  }
  gsl_vector_scale(g, fval);
  gsl_vector_scale(tmp, mval);
  gsl_vector_sub(g, tmp);
  gsl_vector_scale(g, 1.0/(fval*fval));
}
void AutoDiff::visit( MOD_node& node ) {
  cout << "Visiting MOD node" << endl;
  Assert(false, "NYI");
  NodeEvaluator::visit(node);
}
void AutoDiff::visit( NEG_node& node ) {
  //cout << "Visiting NEG node" << endl;
  NodeEvaluator::visit(node);
  gsl_vector* mgrads = grads[node.mother->id];
  gsl_vector* g = grads[node.id];
  gsl_vector_memcpy(g, mgrads);
  gsl_vector_scale(g, -1.0);
}
void AutoDiff::visit( CONST_node& node ) {
  //cout << "Visiting CONST node" << endl;
  NodeEvaluator::visit(node);
  gsl_vector* g = grads[node.id];
  for (int i = 0; i < nctrls; i++) {
    gsl_vector_set(g, i, 0.0);
  }
}
void AutoDiff::visit( LT_node& node ) {
  //cout << "Visiting LT node" << endl;
  NodeEvaluator::visit(node);
}
void AutoDiff::visit( EQ_node& node ) {
  //cout << "Visiting EQ node" << endl;
  NodeEvaluator::visit(node);
}
void AutoDiff::visit( ARRASS_node& node ) {
  cout << "Visiting ARRASS node" << endl;
  Assert(false, "NYI");
  NodeEvaluator::visit(node);
}

void AutoDiff::visit( UFUN_node& node ){
  NodeEvaluator::visit(node);
  gsl_vector* mgrads = grads[node.multi_mother[0]->id];
  gsl_vector* g = grads[node.id];
  gsl_vector_memcpy(g, mgrads);
  float d = 0.0;
  const string& name = node.get_ufname();
  if (name == "_cast_int_float_math") {
    d = 1.0;
  }
  else if (floats.hasFun(name)) {
    float x = floats.getFloat(i(*node.multi_mother[0]));
    if (name == "arctan_math") {
      d = 1.0/(x*x + 1.0);
    } else if (name == "sin_math") {
      d = cos(x);
    } else if (name == "cos_math") {
      d = -sin(x);
    } else if (name == "tan_math") {
      d = 1.0/(cos(x)*cos(x));
    } else if (name == "sqrt_math") {
      d = 0.5/sqrt(x);
    } else {
      Assert(false, "NYI");
    }
  } else {
    Assert(false, "NYI");
  }
  gsl_vector_scale(g, d);
}

void AutoDiff::visit( TUPLE_R_node& node) {
  NodeEvaluator::visit(node);
  gsl_vector* g = grads[node.id];
  if (node.mother->type == bool_node::UFUN) {
    Assert(((UFUN_node*)(node.mother))->multi_mother.size() == 1, "NYI"); // TODO: This assumes that the ufun has a single output
    gsl_vector_memcpy(g, grads[node.mother->id]);
  } else {
    Assert(false, "NYI");
  }
}



