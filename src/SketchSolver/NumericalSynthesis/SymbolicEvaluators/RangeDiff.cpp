#include "RangeDiff.h"
#include <algorithm>
#include <limits>
#include <math.h>


RangeDiff::RangeDiff(BooleanDAG& bdag_p, FloatManager& _floats, const map<string, int>& floatCtrls_p): bdag(bdag_p), floats(_floats), floatCtrls(floatCtrls_p) {
	ranges.resize(bdag.size(), NULL);
	distances.resize(bdag.size(), NULL);
	nctrls = floatCtrls_p.size();
	if (nctrls == 0) nctrls = 1;
	ctrls = gsl_vector_alloc(nctrls);
}

RangeDiff::~RangeDiff(void) {
	gsl_vector_free(ctrls);
	for (int i = 0; i < ranges.size(); i++) {
		if (ranges[i] != NULL) {
			delete ranges[i];
		}
		if (distances[i] != NULL) {
			delete distances[i];
		}
	}
}

void RangeDiff::visit( SRC_node& node ) { //TODO: deal with array src nodes
	//cout << "Visiting SRC node" << endl;
	Assert(false, "NYI: rangediff for src");
}

void RangeDiff::visit( DST_node& node ) {
	//cout << "Visiting DST node" << endl;
	// Ignore
}

void RangeDiff::visit( ASSERT_node& node ) {
	//cout << "Visiting ASSERT node" << endl;
	DistanceGrad* dg = d(node);
	DistanceGrad* mdist = d(node.mother);
	int ival = getInputValue(node); // Get the value from SAT solver
	if (ival != DEFAULT_INP) {
		Assert(ival == 1, "Did SAT solver set 0 for an assert?");
	}
	// Check if the value computed from mother satisfies the assert
	if (mdist->set) {
		computeError(mdist->dist, 1, mdist->grad, node, node.isHard());
	}
	dg->dist = 1000;
	GradUtil::default_grad(dg->grad);
	dg->set = true;
}

void RangeDiff::visit( CTRL_node& node ) {
	//cout << "Visiting CTRL node" << endl;
	string name = node.get_name();
	if (isFloat(node)) {
		int idx = -1;
		if (floatCtrls.find(name) != floatCtrls.end()) {
			idx = floatCtrls[name];
		} else {
			Assert(false, "All float ctlrs should be handled by the numerical solver");
		}
		double val = gsl_vector_get(ctrls, idx);
		IntervalGrad* interval = r(node);
		interval->update(val, val);
		interval->singleton = true;
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
	} else {
		DistanceGrad* dg = d(node);
		int ival = getInputValue(node);
		if (ival != DEFAULT_INP) {
			if (node.getOtype() == OutType::BOOL) {
				dg->dist = (ival == 1) ? 1000 : -1000;
				GradUtil::default_grad(dg->grad);
				dg->set = true;
			} else {
				Assert(false, "NYI: rangediff for integer ctrls");
				dg->set = false;
			}
		} else {
			dg->set = false;
		}
	}
}

void RangeDiff::visit( PLUS_node& node ) {
	//cout << "Visiting PLUS node" << endl;
	Assert(isFloat(node), "NYI: plus with ints");
	IntervalGrad* interval = r(node);
	IntervalGrad* minterval = r(node.mother);
	IntervalGrad* finterval = r(node.father);
	IntervalGrad::ig_plus(minterval, finterval, interval);
}

void RangeDiff::visit( TIMES_node& node ) {
	//cout << "Visiting TIMES node" << endl;
	Assert(isFloat(node), "NYI: times with ints");
	IntervalGrad* interval = r(node);
	IntervalGrad* minterval = r(node.mother);
	IntervalGrad* finterval = r(node.father);
	if (node.mother == node.father) { // square
		IntervalGrad::ig_square(minterval, interval);
	} else {
		IntervalGrad::ig_times(minterval, finterval, interval);
	}
}

void RangeDiff::visit( ARRACC_node& node ) {
	//cout << "Visiting ARRACC node" << endl;
	DistanceGrad* dist = d(node.mother);
	if (!isFloat(node)) {
		Assert(node.getOtype() == OutType::BOOL, "NYI: arrac with ints");
		DistanceGrad* mdist = d(node.multi_mother[0]);
		DistanceGrad* fdist = d(node.multi_mother[1]);
		DistanceGrad* dg = d(node);
		DistanceGrad::dg_ite(dist, mdist, fdist, dg);
		
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
		return;
	}
#if FINE_GRAIN_RANGES
	if (dist->set) {
		Assert(node.multi_mother.size() == 2, "NYI: Fine grained range for ARRACC of size > 2");
		IntervalGrad* interval = r(node);
		IntervalGrad* minterval = r(node.multi_mother[0]);
		IntervalGrad* finterval = r(node.multi_mother[1]);
		
		if (!isFloat(node)) {
			if (dist->dist >= 0) {
				IntervalGrad::ig_copy(finterval, interval);
			} else {
				IntervalGrad::ig_copy(minterval, interval);
			}
			return;
		}
		//cout << "Dist: " << dist->dist << endl;
		// take conditional union of the two intervals
		IntervalGrad::ig_conditionalUnion(minterval, finterval, dist, interval);
		return;
	}
	
#endif
	// take union of the two intervals
	IntervalGrad* interval = r(node);
	vector<IntervalGrad*> mIntervals;
	for (int i = 0; i < node.multi_mother.size(); i++) {
		mIntervals.push_back(r(node.multi_mother[i]));
	}
	IntervalGrad::ig_union(mIntervals, interval);
}

void RangeDiff::visit( DIV_node& node ) {
	//cout << "Visiting DIV node" << endl;
	Assert(isFloat(node), "NYI: div with ints");
	IntervalGrad* interval = r(node);
	IntervalGrad* minterval = r(node.mother);
	IntervalGrad* finterval = r(node.father);
	IntervalGrad::ig_div(minterval, finterval, interval);
}

void RangeDiff::visit( MOD_node& node ) {
	cout << "Visiting MOD node" << endl;
	Assert(false, "NYI: rangediff mod");
}

void RangeDiff::visit( NEG_node& node ) {
	//cout << "Visiting NEG node" << endl;
	Assert(isFloat(node), "NYI: neg with ints");
	IntervalGrad* interval = r(node);
	IntervalGrad* minterval = r(node.mother);
	IntervalGrad::ig_neg(minterval, interval);
}

void RangeDiff::visit( CONST_node& node ) {
	if (node.isFloat()) {
		double val = node.getFval();
		IntervalGrad* interval = r(node);
		interval->update(val, val);
		interval->singleton = true;
		GradUtil::default_grad(interval->getLGrad());
		GradUtil::default_grad(interval->getHGrad());
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
			Assert(false, "NYI: rangediff integer constants");
			dg->set = false;
		}
	}
}

void RangeDiff::visit( LT_node& node ) {
	DistanceGrad* dg = d(node);
	
	// Comput the value from the parents
	Assert(isFloat(node.mother) && isFloat(node.father), "NYI: rangediff for lt with integer parents");
	IntervalGrad* minterval = r(node.mother);
	IntervalGrad* finterval = r(node.father);
	
	IntervalGrad::ig_lt(minterval, finterval, dg);
	
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

void RangeDiff::visit( EQ_node& node ) {
	//cout << "Visiting EQ node" << endl;
	Assert(false, "NYI: rangediff for eq");
}

void RangeDiff::visit( AND_node& node ) {
	DistanceGrad* dg = d(node);
	
	// Compute the value from the parents
	DistanceGrad* mdist = d(node.mother);
	DistanceGrad* fdist = d(node.father);
	
	DistanceGrad::dg_and(mdist, fdist, dg);
	
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

void RangeDiff::visit( OR_node& node ) {
	DistanceGrad* dg = d(node);
	
	// Compute the value from the parents
	DistanceGrad* mdist = d(node.mother);
	DistanceGrad* fdist = d(node.father);
	
	DistanceGrad::dg_or(mdist, fdist, dg);
	
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

void RangeDiff::visit( NOT_node& node ) {
	DistanceGrad* dg = d(node);
	DistanceGrad* mdist = d(node.mother);
	
	DistanceGrad::dg_not(mdist, dg);
	
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

void RangeDiff::visit( ARRASS_node& node ) {
	cout << "Visiting ARRASS node" << endl;
	Assert(false, "NYI: rangediff for arrass");
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
		} else if (name == "exp_math") {
			IntervalGrad::ig_exp(minterval, interval);
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

double RangeDiff::run(const gsl_vector* ctrls_p, map<int, int>& inputValues_p, gsl_vector* errorGrad_p) {
	Assert(ctrls->size == ctrls_p->size, "RangeDiff ctrl sizes are not matching");
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

void RangeDiff::computeError(double dist, int expected, gsl_vector* dg, bool_node& node, bool relax) {
	assertCtr++;
	if (!foundFailure && (expected == 1 && dist < 0) || (expected == 0 && dist > 0)) {
		//cout << "Error: " << node.lprint() << " dist: " << dist << " exp: " << expected << endl;
		/*cout << "Grad: " ;
		 for (int i = 0; i < nctrls; i++) {
			cout << gsl_vector_get(dg, i) << " ";
		 }
		 cout << endl;*/
		
		error += pow(dist, 2);
		
		gsl_vector_memcpy(GradUtil::tmp3, dg);
		gsl_vector_scale(GradUtil::tmp3, 2*dist);
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
