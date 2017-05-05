#include "GlobalEvaluator.h"
#include <algorithm>
#include <limits>
#include <math.h>

gsl_vector* GlobalEvaluator::tmp;

GlobalEvaluator::GlobalEvaluator(BooleanDAG& bdag_p, FloatManager& _floats, const map<string, int>& floatCtrls_p): bdag(bdag_p), floats(_floats), floatCtrls(floatCtrls_p) {
  values.resize(bdag.size());
  nctrls = floatCtrls_p.size();
	if (nctrls == 0) nctrls = 1;
	tmp = gsl_vector_alloc(nctrls);
	lp = new LP();
}

GlobalEvaluator::~GlobalEvaluator(void) {
  for (int i = 0; i < values.size(); i++) {
		for (int j = 0; j < values[i].size(); j++) {
			delete values[i][j];
		}
	}
	delete tmp;
}

void GlobalEvaluator::visit( SRC_node& node ) { //TODO: deal with array src nodes
  //cout << "Visiting SRC node" << endl;
	Assert(false, "NYI: GlobalEvaluator for src");
}

void GlobalEvaluator::visit( DST_node& node ) {
  //cout << "Visiting DST node" << endl;
	// Ignore
}


void GlobalEvaluator::visit( CTRL_node& node ) {
  //cout << "Visiting CTRL node" << endl;
	string name = node.get_name();
	if (ctrls->contains(name)) {
		Assert(isFloat(node), "Numerical Solver should deal with only float holes");
		float val = floats.getFloat((*ctrls)[name]);
		int idx = - 1;
		if (floatCtrls.find(name) != floatCtrls.end()) {
			idx = floatCtrls[name];
		}
		gsl_vector* g = default_grad(nctrls); //TODO: try to avoid allocating and reallocating these everytime
		gsl_vector_set(g, idx, 1);
		
		Region* r = new Region();
		Value* v = new Value(val, g);
		RegionValuePair* rv = new RegionValuePair(r, v);
		ValuesList& vl = getvalues(node);
		vl.clear();
		vl.push_back(rv);
	} else {
		Assert(!isFloat(node), "All float holes should be dealt by the numerical solver");
		Assert(node.getOtype() == OutType::BOOL, "NYI: GlobalEvaluator integer holes");
		ValuesList& vl = getvalues(node);
		vl.clear();
		
		Region* rt = new Region();
		Value* vt = new Value(1, default_grad(nctrls));
		vl.push_back(new RegionValuePair(rt, vt));
		
		Region* rf = new Region();
		Value* vf = new Value(0, default_grad(nctrls));
		vl.push_back(new RegionValuePair(rf, vf));
	}
}

void GlobalEvaluator::visit( CONST_node& node ) {
	float val;
	if (node.isFloat()) {
		val = node.getFval();
	} else {
		val = node.getVal();
	}
	
	ValuesList& vl = getvalues(node);
	vl.clear();
	
	Region* r = new Region();
	Value* v = new Value(val, default_grad(nctrls));
	vl.push_back(new RegionValuePair(r, v));
}

void GlobalEvaluator::visit( PLUS_node& node ) {
  //cout << "Visiting PLUS node" << endl;
	Assert(isFloat(node), "NYI: plus with ints");
	ValuesList& vl = getvalues(node);
	vl.clear();
	
	const ValuesList& ml = getvalues(node.mother);
	const ValuesList& fl = getvalues(node.father);
	
	for (int i = 0; i < ml.size(); i++) {
		for (int j = 0; j < fl.size(); j++) {
			RegionValuePair* rv1 = ml[i];
			RegionValuePair* rv2 = fl[j];
			
			RegionValuePair* rv = new RegionValuePair(Region::intersect(rv1->r, rv2->r), Value::add(rv1->v, rv2->v));
			vl.push_back(rv);
		}
	}
}

void GlobalEvaluator::visit( TIMES_node& node ) {
  //cout << "Visiting TIMES node" << endl;
	Assert(isFloat(node), "NYI: times with ints");
	ValuesList& vl = getvalues(node);
	vl.clear();
	if (node.mother == node.father) {
		const ValuesList& ml = getvalues(node.mother);
		
		for (int i = 0; i < ml.size(); i++) {
			RegionValuePair* rv1 = ml[i];
			RegionValuePair* rv = new RegionValuePair(Region::copy(rv1->r), Value::square(rv1->v));
			vl.push_back(rv);
		}
	} else {
		const ValuesList& ml = getvalues(node.mother);
		const ValuesList& fl = getvalues(node.father);
	
		for (int i = 0; i < ml.size(); i++) {
			for (int j = 0; j < fl.size(); j++) {
				RegionValuePair* rv1 = ml[i];
				RegionValuePair* rv2 = fl[j];
			
				RegionValuePair* rv = new RegionValuePair(Region::intersect(rv1->r, rv2->r), Value::mult(rv1->v, rv2->v));
				vl.push_back(rv);
			}
		}
	}
}

void GlobalEvaluator::visit( DIV_node& node ) {
  //cout << "Visiting DIV node" << endl;
	Assert(isFloat(node), "NYI: div with ints");
	ValuesList& vl = getvalues(node);
	vl.clear();
	
	const ValuesList& ml = getvalues(node.mother);
	const ValuesList& fl = getvalues(node.father);
	
	for (int i = 0; i < ml.size(); i++) {
		for (int j = 0; j < fl.size(); j++) {
			RegionValuePair* rv1 = ml[i];
			RegionValuePair* rv2 = fl[j];
			
			RegionValuePair* rv = new RegionValuePair(Region::intersect(rv1->r, rv2->r), Value::div(rv1->v, rv2->v));
			vl.push_back(rv);
		}
	}
}

void GlobalEvaluator::visit( MOD_node& node ) {
  cout << "Visiting MOD node" << endl;
  Assert(false, "NYI: GlobalEvaluator mod");
}

void GlobalEvaluator::visit( NEG_node& node ) {
  //cout << "Visiting NEG node" << endl;
	Assert(isFloat(node), "NYI: neg with ints");
	ValuesList& vl = getvalues(node);
	vl.clear();
	
	const ValuesList& ml = getvalues(node.mother);
	
	for (int i = 0; i < ml.size(); i++) {
		RegionValuePair* rv1 = ml[i];
		RegionValuePair* rv = new RegionValuePair(Region::copy(rv1->r), Value::neg(rv1->v));
		vl.push_back(rv);
	}
}

void GlobalEvaluator::visit( ARRACC_node& node ) {
	//cout << "Visiting ARRACC node" << endl;
	Assert(isFloat(node), "NYI: GlobalEvaluator for arracc with ints");
	Assert(node.multi_mother.size() == 2, "NYI: GlobalEvaluator for arracc with more than two choices");
	ValuesList& vl = getvalues(node);
	vl.clear();
	
	const ValuesList& ml = getvalues(node.mother);
	const ValuesList& vl0 = getvalues(node.multi_mother[0]);
	const ValuesList& vl1 = getvalues(node.multi_mother[1]);
	
	for (int i = 0; i < ml.size(); i++) {
		RegionValuePair* rv1 = ml[i];
		if (rv1->v->val == 0.0) {
			for (int j = 0; j < vl0.size(); j++) {
				RegionValuePair* rv2 = vl0[j];
				RegionValuePair* rv = new RegionValuePair(Region::intersect(rv1->r, rv2->r), Value::copy(rv2->v));
				vl.push_back(rv);
			}
		} else {
			Assert(rv1->v->val == 1.0, "Boolean values can only be either 0 or 1");
			for (int j = 0; j < vl1.size(); j++) {
				RegionValuePair* rv2 = vl1[j];
				RegionValuePair* rv = new RegionValuePair(Region::intersect(rv1->r, rv2->r), Value::copy(rv2->v));
				vl.push_back(rv);
			}
		}
	}
}

void GlobalEvaluator::visit( LT_node& node ) {
	Assert(isFloat(node.mother) && isFloat(node.father), "NYI: GlobalEvaluator for lt with integer parents");
	ValuesList& vl = getvalues(node);
	vl.clear();
	
	const ValuesList& ml = getvalues(node.mother);
	const ValuesList& fl = getvalues(node.father);
	
	for (int i = 0; i < ml.size(); i++) {
		for (int j = 0; j < fl.size(); j++) {
			RegionValuePair* rv1 = ml[i];
			RegionValuePair* rv2 = fl[j];
			Line* lt = new Line(rv1->v->val - rv2->v->val, sub_grad(rv1->v->grad, rv2->v->grad));
			Region* rt = Region::intersect(rv1->r, rv2->r);
			rt->addLine(lt);
			RegionValuePair* rvt = new RegionValuePair(rt, new Value(1.0, default_grad(nctrls)));
			vl.push_back(rvt);
			Line* lf = new Line(-rv1->v->val + rv2->v->val, sub_grad(rv2->v->grad, rv1->v->grad));
			Region* rf = Region::intersect(rv1->r, rv2->r);
			rf->addLine(lf);
			RegionValuePair* rvf = new RegionValuePair(rf, new Value(0.0, default_grad(nctrls)));
			vl.push_back(rvf);
		}
	}
}

void GlobalEvaluator::visit( EQ_node& node ) {
  //cout << "Visiting EQ node" << endl;
	Assert(false, "NYI: GlobalEvaluator for eq");
}

void GlobalEvaluator::visit( AND_node& node ) {
	ValuesList& vl = getvalues(node);
	vl.clear();
	const ValuesList& ml = getvalues(node.mother);
	const ValuesList& fl = getvalues(node.father);
	
	for (int i = 0; i < ml.size(); i++) {
		for (int j = 0; j < fl.size(); j++) {
			RegionValuePair* rv1 = ml[i];
			RegionValuePair* rv2 = fl[j];
			Assert(rv1->v->val == 1.0 || rv1->v->val == 0.0, "Boolean values can only be 0 or 1");
			Assert(rv2->v->val == 1.0 || rv2->v->val == 0.0, "Boolean values can only be 0 or 1");
			
			Value* v;
			if (rv1->v->val == 1.0 && rv2->v->val == 1.0) {
				v = new Value(1.0, default_grad(nctrls));
			} else {
				v = new Value(0.0, default_grad(nctrls));
			}
			
			RegionValuePair* rv = new RegionValuePair(Region::intersect(rv1->r, rv2->r), v);
			vl.push_back(rv);
		}
	}
}

void GlobalEvaluator::visit( OR_node& node ) {
	ValuesList& vl = getvalues(node);
	vl.clear();
	const ValuesList& ml = getvalues(node.mother);
	const ValuesList& fl = getvalues(node.father);
	
	for (int i = 0; i < ml.size(); i++) {
		for (int j = 0; j < fl.size(); j++) {
			RegionValuePair* rv1 = ml[i];
			RegionValuePair* rv2 = fl[j];
			Assert(rv1->v->val == 1.0 || rv1->v->val == 0.0, "Boolean values can only be 0 or 1");
			Assert(rv2->v->val == 1.0 || rv2->v->val == 0.0, "Boolean values can only be 0 or 1");
			
			Value* v;
			if (rv1->v->val == 1.0 || rv2->v->val == 1.0) {
				v = new Value(1.0, default_grad(nctrls));
			} else {
				v = new Value(0.0, default_grad(nctrls));
			}
			
			RegionValuePair* rv = new RegionValuePair(Region::intersect(rv1->r, rv2->r), v);
			vl.push_back(rv);
		}
	}
}

void GlobalEvaluator::visit( NOT_node& node ) {
	ValuesList& vl = getvalues(node);
	vl.clear();
	const ValuesList& ml = getvalues(node.mother);
	
	for (int i = 0; i < ml.size(); i++) {
		RegionValuePair* rv1 = ml[i];
		Assert(rv1->v->val == 1.0 || rv1->v->val == 0.0, "Boolean values can only be 0 or 1");
		
		Value* v;
		if (rv1->v->val == 0.0) {
			v = new Value(1.0, default_grad(nctrls));
		} else {
			v = new Value(0.0, default_grad(nctrls));
		}
		
		RegionValuePair* rv = new RegionValuePair(Region::copy(rv1->r), v);
		vl.push_back(rv);
	}
}

void GlobalEvaluator::visit( ASSERT_node& node ) {
	//cout << "Visiting ASSERT node" << endl;
	int ival = getInputValue(node); // Get the value from SAT solver
	if (ival != DEFAULT_INP) {
		Assert(ival == 1, "Did SAT solver set 0 for an assert?");
	}
	// Do nothing
}


void GlobalEvaluator::visit( ARRASS_node& node ) {
  cout << "Visiting ARRASS node" << endl;
  Assert(false, "NYI: GlobalEvaluator for arrass");
}

void GlobalEvaluator::visit( UFUN_node& node ) {
	ValuesList& vl = getvalues(node);
	vl.clear();

	ValuesList& ml = getvalues(node.multi_mother[0]);
	const string& name = node.get_ufname();
	for (int i = 0; i < ml.size(); i++) {
		RegionValuePair* rv1 = ml[i];
		RegionValuePair* rv;
		if (name == "_cast_int_float_math") {
			rv = new RegionValuePair(Region::copy(rv1->r), Value::copy(rv1->v));
		} else if (floats.hasFun(name)) {
			if (name == "arctan_math") {
				rv = new RegionValuePair(Region::copy(rv1->r), Value::arctan(rv1->v));
			} else if (name == "sin_math") {
				rv = new RegionValuePair(Region::copy(rv1->r), Value::sin(rv1->v));
			} else if (name == "cos_math") {
				rv = new RegionValuePair(Region::copy(rv1->r), Value::cos(rv1->v));
			} else if (name == "tan_math") {
				rv = new RegionValuePair(Region::copy(rv1->r), Value::tan(rv1->v));
			} else if (name == "sqrt_math") {
				rv = new RegionValuePair(Region::copy(rv1->r), Value::sqrt(rv1->v));
			} else {
				Assert(false, "NYI");
			}
		} else {
			Assert(false, "NYI");
		}
		vl.push_back(rv);
	}
}

void GlobalEvaluator::visit( TUPLE_R_node& node) {
	if (node.mother->type == bool_node::UFUN) {
    Assert(((UFUN_node*)(node.mother))->multi_mother.size() == 1, "NYI"); // TODO: This assumes that the ufun has a single output
		ValuesList& ml = getvalues(node.mother);
		setvalues(node, ml);
	} else {
    Assert(false, "NYI");
  }
}

void GlobalEvaluator::truncateFarRegions(ValuesList& vl, const gsl_vector* x0, int numRegions) {
	if (vl.size() > numRegions) {
		vector<double> maxDists;
		for (int i = 0; i < vl.size(); i++) {
			maxDists.push_back(vl[i]->r->getMaxDist());
		}
		std::sort(maxDists.begin(), maxDists.end());
		double cutoff = maxDists[numRegions];
		//cout << "Cutoff: " << cutoff << endl;
		
		ValuesList newvl;
		for (int i = 0; i < vl.size(); i++) {
			if (vl[i]->r->getMaxDist() < cutoff) {
				newvl.push_back(vl[i]);
			}
		}
		vl = newvl;
	}
}

void GlobalEvaluator::truncateFarRegions(vector<Region*>& vl, const gsl_vector* x0, int numRegions) {
	if (vl.size() > numRegions) {
		vector<double> maxDists;
		for (int i = 0; i < vl.size(); i++) {
			maxDists.push_back(vl[i]->getMaxDist());
		}
		std::sort(maxDists.begin(), maxDists.end());
		double cutoff = maxDists[numRegions];
		
		vector<Region*> newvl;
		for (int i = 0; i < vl.size(); i++) {
			if (vl[i]->getMaxDist() < cutoff) {
				newvl.push_back(vl[i]);
			}
		}
		vl = newvl;
	}
}

void GlobalEvaluator::truncateUnfeasibleRegions(ValuesList& vl, const gsl_vector* x0) {
	if (vl.size() > 0) {
		ValuesList newvl;
		for (int i = 0; i < vl.size(); i++) {
			if (lp->check(vl[i]->r, x0)) {
				newvl.push_back(vl[i]);
			}
		}
		//cout << "Truncated from " << vl.size() << " to " << newvl.size() << endl;
		vl = newvl;
		if (vl.size() <= 0) {
			cout << "dafad" << endl;
		}
		Assert(vl.size() > 0, "No values? something is wrong");
	}
}

void GlobalEvaluator::truncateUnfeasibleRegions(vector<Region*>& vl, const gsl_vector* x0) {
	if (vl.size() > 0) {
		vector<Region*> newvl;
		for (int i = 0; i < vl.size(); i++) {
			if (lp->check(vl[i], x0)) {
				newvl.push_back(vl[i]);
			}
		}
		//cout << "Truncated from " << vl.size() << " to " << newvl.size() << endl;
		/*if (newvl.size() <= 0) {
			map<Line*, int> lc;
			for (int i = 0; i < vl.size(); i++) {
				for (int j = 0; j < vl[i]->size(); j++) {
					Line* l = vl[i]->get(j);
					if (lc.find(l) != lc.end()) {
						lc[l]++;
					} else {
						lc[l] = 1;
					}
				}
			}
			Line* maxLine;
			int maxCount = 0;
			for (auto it = lc.begin(); it != lc.end(); it++) {
				if (it->second == vl.size()) {
					cout << it->first->print() << " " << it->first->getDist() << endl;
				}
				if (it->second > maxCount) {
					maxCount = it->second;
					maxLine = it->first;
				}
			}
			cout << "Max: " << maxLine->print() << " count: " << maxCount << " / " << vl.size() << endl;
		}*/
		vl = newvl;
		Assert(vl.size() > 0, "No values? something is wrong");
		// TODO: do some kind of merging or dropping if the size is still above the threshold
	}
}

void GlobalEvaluator::truncate(ValuesList& vl, const gsl_vector* x0, int numRegions) {
	truncateFarRegions(vl, x0, numRegions);
	truncateUnfeasibleRegions(vl, x0);
}

void GlobalEvaluator::truncate(vector<Region*>& vl, const gsl_vector* x0, int numRegions) {
	truncateFarRegions(vl, x0, numRegions);
	truncateUnfeasibleRegions(vl, x0);
}


double GlobalEvaluator::run(VarStore& ctrls_p, map<int, int>& inputValues_p, gsl_vector* errorGrad_p, const gsl_vector* x) {
	cout << gsl_vector_get(x, 0) << " " << gsl_vector_get(x, 1) << " " << gsl_vector_get(x, 2) << endl;
	ctrls = &ctrls_p;
	inputValues = inputValues_p;
	errorGrad = errorGrad_p;
	error = 0;
  for(BooleanDAG::iterator node_it = bdag.begin(); node_it != bdag.end(); ++node_it){
    (*node_it)->accept(*this);
		ValuesList& vl = getvalues(*node_it);
		cout << (*node_it)->lprint() << endl;
		cout << "Before: " << vl.size() << endl;
		truncate(vl, x, 100);
		cout << "After: " << vl.size() << endl;
	}
	// process all asserts
	cout << "Processing asserts" << endl;
	vector<Region*> trueRegions;
	bool first = true;
	for (BooleanDAG::iterator node_it = bdag.begin(); node_it != bdag.end(); ++node_it) {
		bool_node* n = *node_it;
		if (n->type == bool_node::ASSERT) {
			vector<Region*> newTrueRegions;
			ValuesList& vl = getvalues(n->mother);
			for (int i = 0; i < vl.size(); i++) {
				RegionValuePair* rv = vl[i];
				if (rv->v->val == 1.0) {
					if (first) {
						newTrueRegions.push_back(Region::copy(rv->r));
					} else {
						for (int j = 0; j < trueRegions.size(); j++) {
							newTrueRegions.push_back(Region::intersect(trueRegions[j], rv->r));
						}
					}
				}
			}
			trueRegions.clear();
			trueRegions = newTrueRegions;
			cout << "Before: " << trueRegions.size() << endl;
			truncate(trueRegions, x, 100);
			cout << "After: " << trueRegions.size() << endl;
			
			/*double minDist = numeric_limits<double>::max();
			for (int i = 0; i < trueRegions.size(); i++) {
				if (trueRegions[i]->getMaxDist() < minDist) {
					minDist = trueRegions[i]->getMaxDist();
				}
			}
			cout << "Min dist: " << minDist << endl;*/
			
			/*ofstream file("/Users/Jeevu/projects/symdiff/scripts/parallelPark/graphs1/o"+ to_string(ctr++) +".txt");
			for (int i = 0; i < x->size; i++) {
				file << gsl_vector_get(x, i) << ";";
			}
			file << endl;
			
			for (int i = 0; i < vl.size(); i++) {
				RegionValuePair* rv = vl[i];
				if (rv->v->val == 1.0) {
				for (int j = 0; j < rv->r->size(); j++) {
					Line* l = rv->r->get(j);
					for (int k = 0; k < nctrls; k++) {
						file << gsl_vector_get(l->m, k) << ",";
					}
					file << l->d << ";";
				}
				file << endl;
				}
			}
			
			file.close();*/
			
			/*ofstream file("/Users/Jeevu/projects/symdiff/scripts/parallelPark/graphs1/t"+ to_string(ctr++) +".txt");
			for (int i = 0; i < x->size; i++) {
				file << gsl_vector_get(x, i) << ";";
			}
			file << endl;
			
			for (int i = 0; i < trueRegions.size(); i++) {
				for (int j = 0; j < trueRegions[i]->size(); j++) {
					Line* l = trueRegions[i]->get(j);
					for (int k = 0; k < nctrls; k++) {
						file << gsl_vector_get(l->m, k) << ",";
					}
					file << l->d << ";";
				}
				file << endl;
			}
			
			file.close();*/
			
			if (first) {
				first = false;
			}
		}
	}
	
	// check if any of the true region is feasible
	//cout << trueRegions.size() << endl;
	//for (int i = 0; i < trueRegions.size(); i++) {
	//	cout << trueRegions[i]->print() << endl;
	//	cout << trueRegions[i]->getMaxDist() << endl;
	//	cout << lp->check(trueRegions[i], x) << endl;
	//}
	bool foundFeasbile = false;
	error = numeric_limits<double>::max();
	for (int i = 0; i < trueRegions.size(); i++) {
		if (lp->check(trueRegions[i], x)) {
			double maxDist = trueRegions[i]->getMaxDist();
			if (maxDist < error) {
				//cout << "Error: " << maxDist << endl;
				error = maxDist;
				gsl_vector_memcpy(errorGrad, trueRegions[i]->getMaxGrad());
			}
		}
	}
	cout << "Error: " << error << " Grad: " << gsl_vector_get(errorGrad, 0) << " " << gsl_vector_get(errorGrad, 1) << endl;
  return error;
}

gsl_vector* GlobalEvaluator::add_grad(gsl_vector* g1, gsl_vector* g2) {
	gsl_vector* g = gsl_vector_alloc(g1->size);
	gsl_vector_memcpy(g, g1);
	gsl_vector_add(g, g2);
	return g;
}

gsl_vector* GlobalEvaluator::sub_grad(gsl_vector* g1, gsl_vector* g2) {
	gsl_vector* g = gsl_vector_alloc(g1->size);
	gsl_vector_memcpy(g, g1);
	gsl_vector_sub(g, g2);
	return g;
}

gsl_vector* GlobalEvaluator::mult_grad(double v1, double v2, gsl_vector* g1, gsl_vector* g2) {
	gsl_vector* g = gsl_vector_alloc(g1->size);
	gsl_vector_memcpy(g, g1);
	gsl_vector_scale(g, v2);
	gsl_vector_memcpy(tmp, g2);
	gsl_vector_scale(tmp, v1);
	gsl_vector_add(g, tmp);
	return g;
}

gsl_vector* GlobalEvaluator::div_grad(double v1, double v2, gsl_vector* g1, gsl_vector* g2) {
	gsl_vector* g = gsl_vector_alloc(g1->size);
	gsl_vector_memcpy(g, g1);
	gsl_vector_scale(g, v2);
	gsl_vector_memcpy(tmp, g2);
	gsl_vector_scale(tmp, v1);
	gsl_vector_sub(g, tmp);
	gsl_vector_scale(g, 1.0/(v2*v2));
	return g;
}

gsl_vector* GlobalEvaluator::neg_grad(gsl_vector* g1) {
	gsl_vector* g = gsl_vector_alloc(g1->size);
	gsl_vector_memcpy(g, g1);
	gsl_vector_scale(g, -1.0);
	return g;
}

gsl_vector* GlobalEvaluator::copy_grad(gsl_vector* g1) {
	gsl_vector* g = gsl_vector_alloc(g1->size);
	gsl_vector_memcpy(g, g1);
	return g;
}

gsl_vector* GlobalEvaluator::arctan_grad(double v1, gsl_vector* g1) {
	gsl_vector* g = gsl_vector_alloc(g1->size);
	float dm = 1.0/(v1 * v1 + 1.0);
	gsl_vector_memcpy(g, g1);
	gsl_vector_scale(g, dm);
	return g;
}

gsl_vector* GlobalEvaluator::tan_grad(double v1, gsl_vector* g1) {
	gsl_vector* g = gsl_vector_alloc(g1->size);
	float dm = 1.0/(cos(v1)*cos(v1));
	gsl_vector_memcpy(g, g1);
	gsl_vector_scale(g, dm);
	return g;
}

gsl_vector* GlobalEvaluator::cos_grad(double v1, gsl_vector* g1) {
	gsl_vector* g = gsl_vector_alloc(g1->size);
	float dm = -sin(v1);
	gsl_vector_memcpy(g, g1);
	gsl_vector_scale(g, dm);
	return g;
}

gsl_vector* GlobalEvaluator::sin_grad(double v1, gsl_vector* g1) {
	gsl_vector* g = gsl_vector_alloc(g1->size);
	float dm = cos(v1);
	gsl_vector_memcpy(g, g1);
	gsl_vector_scale(g, dm);
	return g;
}

gsl_vector* GlobalEvaluator::sqrt_grad(double v1, gsl_vector* g1) {
	gsl_vector* g = gsl_vector_alloc(g1->size);
	float dm = 0.5/sqrt(v1);
	gsl_vector_memcpy(g, g1);
	gsl_vector_scale(g, dm);
	return g;
}

gsl_vector* GlobalEvaluator::square_grad(double v1, gsl_vector* g1) {
	gsl_vector* g = gsl_vector_alloc(g1->size);
	float dm = 2*v1;
	gsl_vector_memcpy(g, g1);
	gsl_vector_scale(g, dm);
	return g;
}

Value* Value::add(Value* v1, Value* v2) {
	return new Value(v1->val + v2->val, GlobalEvaluator::add_grad(v1->grad, v2->grad));
}

Value* Value::mult(Value* v1, Value* v2) {
	return new Value(v1->val * v2->val, GlobalEvaluator::mult_grad(v1->val, v2->val, v1->grad, v2->grad));
}

Value* Value::div(Value* v1, Value* v2) {
	return new Value(v1->val / v2->val, GlobalEvaluator::div_grad(v1->val, v2->val, v1->grad, v2->grad));
}

Value* Value::neg(Value* v1) {
	return new Value(-v1->val, GlobalEvaluator::neg_grad(v1->grad));
}

Value* Value::copy(Value* v1) {
	return new Value(v1->val, GlobalEvaluator::copy_grad(v1->grad));
}

Value* Value::arctan(Value* v1) {
	return new Value(std::atan(v1->val), GlobalEvaluator::arctan_grad(v1->val, v1->grad));
}

Value* Value::tan(Value* v1) {
	return new Value(std::tan(v1->val), GlobalEvaluator::tan_grad(v1->val, v1->grad));
}

Value* Value::cos(Value* v1) {
	return new Value(std::cos(v1->val), GlobalEvaluator::cos_grad(v1->val, v1->grad));
}

Value* Value::sin(Value* v1) {
	return new Value(std::sin(v1->val), GlobalEvaluator::sin_grad(v1->val, v1->grad));
}

Value* Value::sqrt(Value* v1) {
	return new Value(std::sqrt(v1->val), GlobalEvaluator::sqrt_grad(v1->val, v1->grad));
}

Value* Value::square(Value* v1) {
	return new Value(v1->val * v1->val, GlobalEvaluator::square_grad(v1->val, v1->grad));
}

