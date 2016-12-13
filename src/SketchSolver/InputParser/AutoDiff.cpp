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
  if (node.getOtype() == OutType::FLOAT) {
    float mval = floats.getFloat(i(*node.mother));
    float fval = floats.getFloat(i(*node.father));
    gsl_vector_scale(g, fval);
    gsl_vector_scale(tmp, mval);
  } else {
    int mval = i(*node.mother);
    int fval = i(*node.father);
    gsl_vector_scale(g, fval);
    gsl_vector_scale(tmp, mval);
  }
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
}
void AutoDiff::visit( MOD_node& node ) {
  //cout << "Visiting MOD node" << endl;
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
  //cout << "Visiting ARRASS node" << endl;
  NodeEvaluator::visit(node);
}