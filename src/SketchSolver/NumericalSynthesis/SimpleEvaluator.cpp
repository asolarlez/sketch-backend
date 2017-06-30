#include "SimpleEvaluator.h"
#include <algorithm>
#include <limits>
#include <math.h>


SimpleEvaluator::SimpleEvaluator(BooleanDAG& bdag_p, FloatManager& _floats, const map<string, int>& floatCtrls_p, const map<string, int>& boolCtrls_p): bdag(bdag_p), floats(_floats), floatCtrls(floatCtrls_p), boolCtrls(boolCtrls_p) {
	distances.resize(bdag.size(), NULL);
	int nctrls = floatCtrls.size() + boolCtrls.size();
	if (nctrls == 0) nctrls = 1;
	ctrls = gsl_vector_alloc(nctrls);
}

void SimpleEvaluator::visit( SRC_node& node ) {
  //cout << "Visiting SRC node" << endl;
	Assert(false, "NYI: SimpleEvaluator for src");
}

void SimpleEvaluator::visit( DST_node& node ) {
  //cout << "Visiting DST node" << endl;
	// Ignore
}

void SimpleEvaluator::visit( ASSERT_node& node ) {
	//cout << "Visiting ASSERT node" << endl;
	setvalue(node, d(node.mother));
}

void SimpleEvaluator::visit( CTRL_node& node ) {
  //cout << "Visiting CTRL node" << endl;
	string name = node.get_name();
	if (isFloat(node)) {
		int idx = -1;
		if (floatCtrls.find(name) != floatCtrls.end()) {
			idx = floatCtrls[name];
		} else {
			Assert(false, "All float ctrls should be handled by the numerical solver");
		}
		float val = gsl_vector_get(ctrls, idx);
		setvalue(node, val);
	} else {
		int idx = -1;
		if (boolCtrls.find(name) != boolCtrls.end()) {
			idx = boolCtrls[name];
		} else {
			Assert(false, "All bool ctrls should be handled here");
		}
		float val = gsl_vector_get(ctrls, idx);
		float dist = val - 0.5;
		setvalue(node, dist);
	}
}

void SimpleEvaluator::visit( PLUS_node& node ) {
  //cout << "Visiting PLUS node" << endl;
	Assert(isFloat(node), "NYI: plus with ints");
	setvalue(node, d(node.mother) + d(node.father));
}

void SimpleEvaluator::visit( TIMES_node& node ) {
  //cout << "Visiting TIMES node" << endl;
	Assert(isFloat(node), "NYI: times with ints");
	setvalue(node, d(node.mother)*d(node.father));
}

void SimpleEvaluator::visit( ARRACC_node& node ) {
  //cout << "Visiting ARRACC node" << endl;
	Assert(node.multi_mother.size() == 2, "NYI: SimpleEvaluator for ARRACC of size > 2");
	float m = d(node.mother);
	//cout << node.lprint() << " " << m << endl;
	int idx = (m >= 0) ? 1 : 0;
	setvalue(node, d(node.multi_mother[idx]));
}

void SimpleEvaluator::visit( DIV_node& node ) {
  //cout << "Visiting DIV node" << endl;
	Assert(isFloat(node), "NYI: div with ints");
	setvalue(node, d(node.mother)/d(node.father));
}

void SimpleEvaluator::visit( MOD_node& node ) {
  cout << "Visiting MOD node" << endl;
  Assert(false, "NYI: SimpleEvaluator mod");
}

void SimpleEvaluator::visit( NEG_node& node ) {
  //cout << "Visiting NEG node" << endl;
	Assert(isFloat(node), "NYI: neg with ints");
	setvalue(node, -d(node.mother));
}

void SimpleEvaluator::visit( CONST_node& node ) {
	if (node.isFloat()) {
		setvalue(node, node.getFval());
	} else {
		int val = node.getVal();
		if (node.getOtype() == OutType::BOOL) {
			float dist = (val == 1) ? 1000 : -1000;
			setvalue(node, dist);
		} else {
			Assert(false, "NYI: SimpleEvaluator integer constants");
		}
	}
}

void SimpleEvaluator::visit( LT_node& node ) {
	Assert(isFloat(node.mother) && isFloat(node.father), "NYI: SimpleEvaluator for lt with integer parents");
	float m = d(node.mother);
	float f = d(node.father);
	float d = f - m;
	if (d == 0) d = -MIN_VALUE;
	//cout << node.lprint() << " " << m << " " << f << " " << d << endl;
	setvalue(node, d);
}

void SimpleEvaluator::visit( EQ_node& node ) {
  //cout << "Visiting EQ node" << endl;
	Assert(false, "NYI: SimpleEvaluator for eq");
}

void SimpleEvaluator::visit( AND_node& node ) {
	float m = d(node.mother);
	float f = d(node.father);
	float d = (m < f) ? m : f;
	setvalue(node, d);
}

void SimpleEvaluator::visit( OR_node& node ) {
	float m = d(node.mother);
	float f = d(node.father);
	float d = (m > f) ? m : f;
	setvalue(node, d);
}

void SimpleEvaluator::visit( NOT_node& node ) {
	setvalue(node, -d(node.mother));
}

void SimpleEvaluator::visit( ARRASS_node& node ) {
  cout << "Visiting ARRASS node" << endl;
  Assert(false, "NYI: SimpleEvaluator for arrass");
}

void SimpleEvaluator::visit( UFUN_node& node ) {
	const string& name = node.get_ufname();
	float m = d(node.multi_mother[0]);
	float d;
	if (name == "_cast_int_float_math") {
		d = m;
	} else if (floats.hasFun(name)) {
		if (name == "arctan_math") {
			d = atan(m);
		} else if (name == "sin_math") {
			d = sin(m);
		} else if (name == "cos_math") {
			d = cos(m);
		} else if (name == "tan_math") {
			d = tan(m);
		} else if (name == "sqrt_math") {
			d = sqrt(m);
		} else if (name == "exp_math") {
			d = exp(m);
		} else {
			Assert(false, "NYI");
		}
	} else {
		Assert(false, "NYI");
	}
	setvalue(node, d);
}

void SimpleEvaluator::visit( TUPLE_R_node& node) {
	if (node.mother->type == bool_node::UFUN) {
    Assert(((UFUN_node*)(node.mother))->multi_mother.size() == 1, "NYI"); // TODO: This assumes that the ufun has a single output
		setvalue(node, d(node.mother));
	} else {
    Assert(false, "NYI");
  }
}

vector<tuple<float, int, int>> SimpleEvaluator::run(const gsl_vector* ctrls_p, map<int, int>& imap_p) {
	Assert(ctrls->size == ctrls_p->size, "SimpleEvaluator ctrl sizes are not matching");
	for (int i = 0; i < ctrls->size; i++) {
		gsl_vector_set(ctrls, i, gsl_vector_get(ctrls_p, i));
	}
  for(BooleanDAG::iterator node_it = bdag.begin(); node_it != bdag.end(); ++node_it){
    (*node_it)->accept(*this);
	}
	vector<tuple<float, int, int>> s;
	for (int i = 0; i < imap_p.size(); i++) {
		bool_node* n = bdag[imap_p[i]];
		bool hasArraccChild = false;
		FastSet<bool_node> children = n->children;
		for(child_iter it = children.begin(); it != children.end(); ++it) {
			if ((*it)->type == bool_node::ARRACC) {
				hasArraccChild = true;
				break;
			}
		}
		float dist = d(n);
		float cost = abs(dist);
		if (hasArraccChild) {
			cost = cost/1000.0;
		}
		s.push_back(make_tuple(cost, i, dist > 0));
	}
	return s;
}

double SimpleEvaluator::run1(const gsl_vector* ctrls_p, map<int, int>& inputValues_p) {
	Assert(ctrls->size == ctrls_p->size, "SimpleEvaluator ctrl sizes are not matching");
	for (int i = 0; i < ctrls->size; i++) {
		gsl_vector_set(ctrls, i, gsl_vector_get(ctrls_p, i));
	}
	double error = 0;
	for(BooleanDAG::iterator node_it = bdag.begin(); node_it != bdag.end(); ++node_it){
		bool_node* node = (*node_it);
		node->accept(*this);
		
		if (inputValues_p.find(node->id) != inputValues_p.end()) {
			int val = inputValues_p[node->id];
			error += computeError(d(node), val, node);
			setvalue(*node, (val == 1) ? 1000 : -1000);
		}
		if (node->type == bool_node::ASSERT) {
			error += computeError(d(node), 1, node);
			setvalue(*node, 1000);
		}
	}
	return error;
}

double SimpleEvaluator::computeError(float dist, int expected, bool_node* node) {
	if ((expected == 1 && dist < 0) || (expected == 0 && dist > 0)) {
		//cout << node->lprint() << " " << dist << endl;
		return pow(dist, 2);
	} else {
		return 0.0;
	}
}
