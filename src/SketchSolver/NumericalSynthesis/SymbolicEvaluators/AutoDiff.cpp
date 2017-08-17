#include "AutoDiff.h"
#include <algorithm>
#include <limits>
#include <math.h>


AutoDiff::AutoDiff(BooleanDAG& bdag_p, FloatManager& _floats, const map<string, int>& floatCtrls_p): bdag(bdag_p), floats(_floats), floatCtrls(floatCtrls_p){
	values.resize(bdag.size(), NULL);
	nctrls = floatCtrls_p.size();
	if (nctrls == 0) nctrls = 1;
	ctrls = gsl_vector_alloc(nctrls);
}

AutoDiff::~AutoDiff(void) {
	delete ctrls;
	for (int i = 0; i < values.size(); i++) {
		if (values[i] != NULL) {
			delete values[i];
		}
	}
}

void AutoDiff::visit( SRC_node& node ) {
	//cout << "Visiting SRC node" << endl;
	Assert(false, "NYI: AutoDiff for src");
}

void AutoDiff::visit( DST_node& node ) {
	//cout << "Visiting DST node" << endl;
	// Ignore
}

void AutoDiff::visit( ASSERT_node& node ) {
	//cout << "Visiting ASSERT node" << endl;
}

void AutoDiff::visit( CTRL_node& node ) {
	//cout << "Visiting CTRL node" << endl;
	string name = node.get_name();
	
	if (isFloat(node)) {
		int idx = -1;
		if (floatCtrls.find(name) != floatCtrls.end()) {
			idx = floatCtrls[name];
		} else {
			Assert(false, "All float holes should be handled by numerical solver");
		}
		ValueGrad* val = v(node);
		val->update(gsl_vector_get(ctrls, idx));
		val->set = true;
		GradUtil::default_grad(val->getGrad());
		gsl_vector_set(val->getGrad(), idx, 1.0);
		
	}
}

void AutoDiff::visit( PLUS_node& node ) {
	//cout << "Visiting PLUS node" << endl;
	Assert(isFloat(node), "NYI: plus with ints");
	ValueGrad* val = v(node);
	ValueGrad* mval = v(node.mother);
	ValueGrad* fval = v(node.father);
	ValueGrad::vg_plus(mval, fval, val);
}

void AutoDiff::visit( TIMES_node& node ) {
	//cout << "Visiting TIMES node" << endl;
	Assert(isFloat(node), "NYI: times with ints");
	ValueGrad* val = v(node);
	ValueGrad* mval = v(node.mother);
	ValueGrad* fval = v(node.father);
	ValueGrad::vg_times(mval, fval, val);
}

void AutoDiff::visit(ARRACC_node& node )  { // TODO: find a way to combine the two different representations for boolean nodes

	Assert(node.multi_mother.size() == 2, "NYI: AutoDiff ARRACC of size > 2");
	ValueGrad* val = v(node);
	if (node.getOtype() == OutType::BOOL) {
		val->set = false;
		return;
	}
	ValueGrad* m1 = v(node.multi_mother[0]);
	ValueGrad* m2 = v(node.multi_mother[1]);
	int ival = getInputValue(node.mother);
	if (ival == 0) {
		ValueGrad::vg_copy(m1, val);
	} else if (ival == 1) {
		ValueGrad::vg_copy(m2, val);
	} else {
		val->set = false;
	}
}

void AutoDiff::visit( DIV_node& node ) {
	//cout << "Visiting DIV node" << endl;
	Assert(isFloat(node), "NYI: div with ints");
	ValueGrad* val = v(node);
	ValueGrad* mval = v(node.mother);
	ValueGrad* fval = v(node.father);
	ValueGrad::vg_div(mval, fval, val);
}

void AutoDiff::visit( MOD_node& node ) {
	cout << "Visiting MOD node" << endl;
	Assert(false, "NYI: AutoDiff mod");
}

void AutoDiff::visit( NEG_node& node ) {
	//cout << "Visiting NEG node" << endl;
	Assert(isFloat(node), "NYI: neg with ints");
	ValueGrad* val = v(node);
	ValueGrad* mval = v(node.mother);
	ValueGrad::vg_neg(mval, val);
}

void AutoDiff::visit( CONST_node& node ) {
	if (node.getOtype() != OutType::BOOL) {
		ValueGrad* val = v(node);
		val->update(node.getFval());
		val->set = true;
		GradUtil::default_grad(val->getGrad());
	}
}

void AutoDiff::visit( LT_node& node ) {
	ValueGrad* val = v(node);
	
	// Comput the value from the parents
	Assert(isFloat(node.mother) && isFloat(node.father), "NYI: AutoDiff for lt with integer parents");
	ValueGrad* mval = v(node.mother);
	ValueGrad* fval = v(node.father);
	
	ValueGrad::vg_lt(mval, fval, val);
}

void AutoDiff::visit( EQ_node& node ) {
	//cout << "Visiting EQ node" << endl;
	Assert(false, "NYI: AutoDiff for eq");
}

void AutoDiff::visit( AND_node& node ) {
}

void AutoDiff::visit( OR_node& node ) {
}

void AutoDiff::visit( NOT_node& node ) {
}

void AutoDiff::visit( ARRASS_node& node ) {
	cout << "Visiting ARRASS node" << endl;
	Assert(false, "NYI: AutoDiff for arrass");
}

void AutoDiff::visit( UFUN_node& node ) {
	ValueGrad* val = v(node);
	ValueGrad* mval = v(node.multi_mother[0]);
	
	const string& name = node.get_ufname();
	if (name == "_cast_int_float_math") {
		ValueGrad::vg_cast_int_float(mval, val);
	} else if (floats.hasFun(name)) {
		if (name == "arctan_math") {
			ValueGrad::vg_arctan(mval, val);
		} else if (name == "sin_math") {
			ValueGrad::vg_sin(mval, val);
		} else if (name == "cos_math") {
			ValueGrad::vg_cos(mval, val);
		} else if (name == "tan_math") {
			ValueGrad::vg_tan(mval, val);
		} else if (name == "sqrt_math") {
			ValueGrad::vg_sqrt(mval, val);
		} else if (name == "exp_math") {
			ValueGrad::vg_exp(mval, val);
		} else {
			Assert(false, "NYI");
		}
	} else {
		Assert(false, "NYI");
	}
}

void AutoDiff::visit( TUPLE_R_node& node) {
	if (node.mother->type == bool_node::UFUN) {
		Assert(((UFUN_node*)(node.mother))->multi_mother.size() == 1, "NYI"); // TODO: This assumes that the ufun has a single output
		ValueGrad* mval = v(node.mother);
		ValueGrad* val = v(node);
		ValueGrad::vg_copy(mval, val);
	} else {
		Assert(false, "NYI");
	}
}

void AutoDiff::run(const gsl_vector* ctrls_p, const map<int, int>& inputValues_p) {
	Assert(ctrls->size == ctrls_p->size, "AutoDiff ctrl sizes are not matching");

	for (int i = 0; i < ctrls->size; i++) {
		gsl_vector_set(ctrls, i, gsl_vector_get(ctrls_p, i));
	}
	inputValues = inputValues_p;

	for(BooleanDAG::iterator node_it = bdag.begin(); node_it != bdag.end(); ++node_it){
		//cout << (*node_it)->lprint() << endl;
		(*node_it)->accept(*this);
	}
}

double AutoDiff::errorGD(gsl_vector* errorGrad_p) {
	errorGrad = errorGrad_p;
	error = 0;
	for (BooleanDAG::iterator node_it = bdag.begin(); node_it != bdag.end(); ++node_it) {
		bool_node* n = *node_it;
		if (n->type == bool_node::ASSERT) {
			ValueGrad* val = v(n->mother);
			if (val->set) {
				computeError(val->getVal(), 1, val->getGrad());
			}
		}
		if (n->getOtype() == OutType::BOOL) {
			int ival = getInputValue(n);
			ValueGrad* val = v(n);
			if (ival != DEFAULT_INP) {
				if (val->set) {
					computeError(val->getVal(), ival, val->getGrad());
					//cout << error << endl;
				}
			}
		}
	}
	return error;
}

void AutoDiff::computeError(float dist, int expected, gsl_vector* dg) {
	if ((expected == 1 && dist < 0) || (expected == 0 && dist > 0)) {
		error += abs(dist);
		
		gsl_vector_memcpy(GradUtil::tmp3, dg);
		gsl_vector_scale(GradUtil::tmp3, dist >= 0 ? 1 : -1);
		gsl_vector_add(errorGrad, GradUtil::tmp3);
	}
}

bool AutoDiff::check(bool_node* n, int expected) {
	ValueGrad* val = v(n);
	if (val->set) {
		float dist = val->getVal();
		if ((expected == 1 && dist < 0) || (expected == 0 && dist > 0))	{
			return false;
		}
	}
	return true;
}
