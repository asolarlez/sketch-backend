#include "BoolAutoDiff.h"
#include <algorithm>
#include <limits>
#include <math.h>


BoolAutoDiff::BoolAutoDiff(BooleanDAG& bdag_p, FloatManager& _floats, const map<string, int>& floatCtrls_p, const map<string, int>& boolCtrls_p): bdag(bdag_p), floats(_floats), floatCtrls(floatCtrls_p), boolCtrls(boolCtrls_p) {
	values.resize(bdag.size(), NULL);
	distances.resize(bdag.size(), NULL);
	nctrls = floatCtrls_p.size() + boolCtrls_p.size();
	if (nctrls == 0) nctrls = 1;
	ctrls = gsl_vector_alloc(nctrls);
}

BoolAutoDiff::~BoolAutoDiff(void) {
	delete ctrls;
	for (int i = 0; i < values.size(); i++) {
		if (values[i] != NULL) {
			delete values[i];
		}
		if (distances[i] != NULL) {
			delete distances[i];
		}
	}
}

void BoolAutoDiff::visit( SRC_node& node ) {
	//cout << "Visiting SRC node" << endl;
	Assert(false, "NYI: BoolAutoDiff for src");
}

void BoolAutoDiff::visit( DST_node& node ) {
	//cout << "Visiting DST node" << endl;
	// Ignore
}

void BoolAutoDiff::visit( ASSERT_node& node ) {
	//cout << "Visiting ASSERT node" << endl;
	DistanceGrad* dg = d(node);
	DistanceGrad* mdist = d(node.mother);
	int ival = getInputValue(node); // Get the value from SAT solver
	if (ival != DEFAULT_INP) {
		Assert(ival == 1, "Did SAT solver set 0 for an assert?");
	}
	Assert(mdist->set, "NYI: dsakhf");
	// Check if the value computed from mother satisfies the assert
	if (mdist->set) {
		computeError(mdist->dist, 1, mdist->grad, node, node.isHard());
	}
	dg->dist = 1000;
	GradUtil::default_grad(dg->grad);
	dg->set = true;
}

void BoolAutoDiff::visit( CTRL_node& node ) {
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
		GradUtil::default_grad(val->getGrad());
		gsl_vector_set(val->getGrad(), idx, 1.0);
		
	} else {
		DistanceGrad* dg = d(node);
		int ival = getInputValue(node);
		if (ival != DEFAULT_INP) {
			dg->set = false;
			ValueGrad* val = v(node);
			val->update((float) ival);
			GradUtil::default_grad(val->getGrad());
		} else {
			dg->set = false;
			Assert(node.getOtype() == OutType::BOOL, "NYI: BoolAutoDiff for integer ctrls");
			int idx = -1;
			if (boolCtrls.find(name) != boolCtrls.end()) {
				idx = boolCtrls[name];
			} else {
				Assert(false, "All bool holes should have a corresponding float approximate");
			}
			float cval = gsl_vector_get(ctrls, idx);
			ValueGrad* val = v(node);
			val->update(cval);
			GradUtil::default_grad(val->getGrad());
			gsl_vector_set(val->getGrad(), idx, 1.0);
			// add errors for ranges of new holes added to approximate boolean holes
			computeError(100.0*cval - 100.0*1.0, 0, val->getGrad(), node, false);
			computeError(100.0*cval, 1, val->getGrad(), node, false);
		}
	}
}

void BoolAutoDiff::visit( PLUS_node& node ) {
	//cout << "Visiting PLUS node" << endl;
	Assert(isFloat(node), "NYI: plus with ints");
	ValueGrad* val = v(node);
	ValueGrad* mval = v(node.mother);
	ValueGrad* fval = v(node.father);
	ValueGrad::vg_plus(mval, fval, val);
}

void BoolAutoDiff::visit( TIMES_node& node ) {
	//cout << "Visiting TIMES node" << endl;
	Assert(isFloat(node), "NYI: times with ints");
	ValueGrad* val = v(node);
	ValueGrad* mval = v(node.mother);
	ValueGrad* fval = v(node.father);
	ValueGrad::vg_times(mval, fval, val);
}

void BoolAutoDiff::visit(ARRACC_node& node )  { // TODO: find a way to combine the two different representations for boolean nodes

	Assert(node.multi_mother.size() == 2, "NYI: BoolAutoDiff ARRACC of size > 2");
	DistanceGrad* dist = d(node.mother);
	ValueGrad* dval = v(node.mother);
	DistanceGrad* mdist = d(node.multi_mother[0]);
	DistanceGrad* fdist = d(node.multi_mother[1]);
	ValueGrad* mval = v(node.multi_mother[0]);
	ValueGrad* fval = v(node.multi_mother[1]);
	DistanceGrad* dg = d(node);
	ValueGrad* val = v(node);
	if (dist->set) {
		if (!isFloat(node)) {
			Assert(node.getOtype() == OutType::BOOL, "NYI: BoolAutoDiff arracc with ints");
			if (mdist->set && fdist->set) {
				DistanceGrad::dg_ite(dist, mdist, fdist, dg); // compute using ands and ors
			} else if (!mdist->set && !fdist->set) {
				ValueGrad::vg_ite(mval, fval, dist, val); // sigmoid computation
				dg->set = false;
			} else {
				cout << node.lprint() << endl;
				Assert(false, "NYI: BoolAutoDiff bool holes");
			}
			int ival = getInputValue(node);
			if (ival != DEFAULT_INP) {
				// check that the value computed from parents matches the value set by the SAT solver.
				if (dg->set) {
					computeError(dg->dist, ival, dg->grad, node);
				}
				dg->dist = (ival == 1) ? 1000 : -1000;
				GradUtil::default_grad(dg->grad);
				dg->set = true;
			}

		} else {
			ValueGrad::vg_ite(mval, fval, dist, val); // sigmoid computation
		}
	} else {
		if (!isFloat(node)) {
			Assert(node.getOtype() == OutType::BOOL, "NYI: BoolAutoDiff arracc with ints");
			if (mdist->set && fdist->set) {
				ValueGrad::vg_ite(mdist, fdist, dval, dg); // Compute using boolean relaxation
			} else if (!mdist->set && !fdist->set) {
				Assert(false, "NYI:yurida");
			} else {
				Assert(false, "NYI: qewrq");
			}
			int ival = getInputValue(node);
			if (ival != DEFAULT_INP) {
				// check that the value computed from parents matches the value set by the SAT solver.
				if (dg->set) {
					computeError(dg->dist, ival, dg->grad, node);
				}
				dg->dist = (ival == 1) ? 1000 : -1000;
				GradUtil::default_grad(dg->grad);
				dg->set = true;
			}

		} else {
			ValueGrad::vg_ite(mval, fval, dval, val); // Compute using boolean relaxation
		}
	}
}

void BoolAutoDiff::visit( DIV_node& node ) {
	//cout << "Visiting DIV node" << endl;
	Assert(isFloat(node), "NYI: div with ints");
	ValueGrad* val = v(node);
	ValueGrad* mval = v(node.mother);
	ValueGrad* fval = v(node.father);
	ValueGrad::vg_div(mval, fval, val);
}

void BoolAutoDiff::visit( MOD_node& node ) {
	cout << "Visiting MOD node" << endl;
	Assert(false, "NYI: BoolAutoDiff mod");
}

void BoolAutoDiff::visit( NEG_node& node ) {
	//cout << "Visiting NEG node" << endl;
	Assert(isFloat(node), "NYI: neg with ints");
	ValueGrad* val = v(node);
	ValueGrad* mval = v(node.mother);
	ValueGrad::vg_neg(mval, val);
}

void BoolAutoDiff::visit( CONST_node& node ) {
	if (node.getOtype() != OutType::BOOL) {
		ValueGrad* val = v(node);
		val->update(node.getFval());
		GradUtil::default_grad(val->getGrad());
	} else {
		int val = node.getVal();
		DistanceGrad* dg = d(node);
		int ival = getInputValue(node);
		if (ival != DEFAULT_INP) {
			Assert(val == ival, "SAT solver set wrong value for a const?");
		}
		if (node.getOtype() == OutType::BOOL) {
			dg->dist = (val == 1) ? 1000 : -1000;
			GradUtil::default_grad(dg->grad);
			dg->set = true;
		} else {
			Assert(false, "NYI: BoolAutoDiff integer constants");
			dg->set = false;
		}
	}
}

void BoolAutoDiff::visit( LT_node& node ) {
	DistanceGrad* dg = d(node);
	
	// Comput the value from the parents
	Assert(isFloat(node.mother) && isFloat(node.father), "NYI: BoolAutoDiff for lt with integer parents");
	ValueGrad* mval = v(node.mother);
	ValueGrad* fval = v(node.father);
	
	ValueGrad::vg_lt(mval, fval, dg);
	
	int ival = getInputValue(node);
	if (ival != DEFAULT_INP) {
		// check that the value computed from parents matches the value set by the SAT solver.
		if (dg->set) {
			computeError(dg->dist, ival, dg->grad, node);
		}
		dg->dist = (ival == 1) ? 1000 : -1000;
		GradUtil::default_grad(dg->grad);
		dg->set = true;
	}
}

void BoolAutoDiff::visit( EQ_node& node ) {
	//cout << "Visiting EQ node" << endl;
	Assert(false, "NYI: BoolAutoDiff for eq");
}

void BoolAutoDiff::visit( AND_node& node ) {
	DistanceGrad* dg = d(node);
	
	// Compute the value from the parents
	DistanceGrad* mdist = d(node.mother);
	DistanceGrad* fdist = d(node.father);

	if (mdist->set && fdist->set) {
		DistanceGrad::dg_and(mdist, fdist, dg);
	} else if (!mdist->set && !fdist->set) {
		ValueGrad* val = v(node);
		ValueGrad* mval = v(node.mother);
		ValueGrad* fval = v(node.father);
		ValueGrad::vg_and(mval, fval, val);
		dg->set = false;
	} else {
		Assert(false, "dfahiq");
	}
	
	int ival = getInputValue(node);
	if (ival != DEFAULT_INP) {
		// check that the value computed from parents matches the value set by the SAT solver.
		if (dg->set) {
			computeError(dg->dist, ival, dg->grad, node);
		}
		dg->dist = (ival == 1) ? 1000 : -1000;
		GradUtil::default_grad(dg->grad);
		dg->set = true;
	}
}

void BoolAutoDiff::visit( OR_node& node ) {
	DistanceGrad* dg = d(node);
	
	// Compute the value from the parents
	DistanceGrad* mdist = d(node.mother);
	DistanceGrad* fdist = d(node.father);
	
	if (mdist->set && fdist->set) {
		DistanceGrad::dg_or(mdist, fdist, dg);
	} else if (!mdist->set && !fdist->set) {
		ValueGrad* val = v(node);
		ValueGrad* mval = v(node.mother);
		ValueGrad* fval = v(node.father);
		ValueGrad::vg_or(mval, fval, val);
		dg->set = false;
	} else {
		cout << node.lprint() << endl;
		cout << node.mother->lprint() << endl;
		cout << node.father->lprint() << endl;
		Assert(false, "dauhpw");
	}
	
	int ival = getInputValue(node);
	if (ival != DEFAULT_INP) {
		// check that the value computed from parents matches the value set by the SAT solver.
		if (dg->set) {
			computeError(dg->dist, ival, dg->grad, node);
		}
		dg->dist = (ival == 1) ? 1000 : -1000;
		GradUtil::default_grad(dg->grad);
		dg->set = true;
	}
}

void BoolAutoDiff::visit( NOT_node& node ) {
	DistanceGrad* dg = d(node);
	DistanceGrad* mdist = d(node.mother);

	if (mdist->set) {
		DistanceGrad::dg_not(mdist, dg);
	} else {
		ValueGrad* val = v(node);
		ValueGrad* mval = v(node.mother);
		ValueGrad::vg_not(mval, val);
		dg->set = false;
	}
	
	int ival = getInputValue(node);
	if (ival != DEFAULT_INP) {
		// check that the value computed from parents matches the value set by the SAT solver.
		if (dg->set) {
			computeError(dg->dist, ival, dg->grad, node);
		}
		dg->dist = (ival == 1) ? 1000 : -1000;
		GradUtil::default_grad(dg->grad);
		dg->set = true;
	}
}

void BoolAutoDiff::visit( ARRASS_node& node ) {
	cout << "Visiting ARRASS node" << endl;
	Assert(false, "NYI: BoolAutoDiff for arrass");
}

void BoolAutoDiff::visit( UFUN_node& node ) {
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

void BoolAutoDiff::visit( TUPLE_R_node& node) {
	if (node.mother->type == bool_node::UFUN) {
		Assert(((UFUN_node*)(node.mother))->multi_mother.size() == 1, "NYI"); // TODO: This assumes that the ufun has a single output
		ValueGrad* mval = v(node.mother);
		ValueGrad* val = v(node);
		ValueGrad::vg_copy(mval, val);
	} else {
		Assert(false, "NYI");
	}
}

double BoolAutoDiff::run(const gsl_vector* ctrls_p, map<int, int>& inputValues_p, gsl_vector* errorGrad_p) {
	Assert(ctrls->size == ctrls_p->size, "BoolAutoDiff ctrl sizes are not matching");

	for (int i = 0; i < ctrls->size; i++) {
		gsl_vector_set(ctrls, i, gsl_vector_get(ctrls_p, i));
	}
	inputValues = inputValues_p;
	errorGrad = errorGrad_p;
	error = 0;
	assertCtr = 0;//bdag.size();
	foundFailure = false;

	for(BooleanDAG::iterator node_it = bdag.begin(); node_it != bdag.end(); ++node_it){
		//cout << (*node_it)->lprint() << endl;
		(*node_it)->accept(*this);
	}
	return error;
}

void BoolAutoDiff::computeError(float dist, int expected, gsl_vector* dg, bool_node& node, bool relax) {
	assertCtr++;
	if (!foundFailure && (expected == 1 && dist < 0) || (expected == 0 && dist > 0)) {
		//cout << "Error: " << node.lprint() << " dist: " << dist << " exp: " << expected << endl;
		/*cout << "Grad: " ;
		 for (int i = 0; i < nctrls; i++) {
			cout << gsl_vector_get(dg, i) << " ";
		 }
		 cout << endl;*/
		
		error += abs(dist);
		
		gsl_vector_memcpy(GradUtil::tmp3, dg);
		gsl_vector_scale(GradUtil::tmp3, dist >= 0 ? 1 : -1);
		gsl_vector_add(errorGrad, GradUtil::tmp3);
		
		//cout << "Faield " << assertCtr  << " " << node.mother->lprint() << endl;
		
		/*if (!relax && gsl_blas_dnrm2(errorGrad) > 0.01 && error > 0.01) {
			//error += ASSERT_PENALTY * 1000.0/assertCtr;
			failedAssert = assertCtr;
			foundFailure = true;
		 }*/
	} else {
		if ((expected == 1 && dist < 0) || (expected == 0 && dist > 0)) {
			//cout << "Ignoring failed " << assertCtr << " " << node.mother->lprint() << endl;
		} else {
			//cout << "Passed " << assertCtr << endl;
		}
	}
}
