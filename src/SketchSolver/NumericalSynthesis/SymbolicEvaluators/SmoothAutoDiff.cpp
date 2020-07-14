#include "SmoothAutoDiff.h"
#include <algorithm>
#include <limits>
#include <math.h>


SmoothAutoDiff::SmoothAutoDiff(BooleanDAG& bdag_p, map<string, int>& floatCtrls_p): bdag(bdag_p), floatCtrls(floatCtrls_p) {
	values.resize(bdag.size());
	distances.resize(bdag.size());
    for (int i = 0; i < bdag.size(); i++) {
        values[i].resize(MAX_REGIONS, NULL);
        distances[i].resize(MAX_REGIONS, NULL);
    }
    nctrls = floatCtrls.size();
    if (nctrls == 0) nctrls = 1;
	ctrls = gsl_vector_alloc(nctrls);
}

SmoothAutoDiff::~SmoothAutoDiff(void) {
	gsl_vector_free(ctrls);
	for (int i = 0; i < values.size(); i++) {
		for (int j = 0; j < values[i].size(); j++) {
            if (values[i][j] != NULL) {
                delete values[i][j];
            }
        }
	}
    for (int i = 0; i < distances.size(); i++) {
        for (int j = 0; j < distances[i].size(); j++) {
            if (distances[i][j] != NULL) {
                delete distances[i][j];
            }
        }
    }
}

void SmoothAutoDiff::visit( SRC_node& node ) {
	//cout << "Visiting SRC node" << endl;
	Assert(false, "NYI: SmoothAutoDiff for src");
}

void SmoothAutoDiff::visit( DST_node& node ) {
	//cout << "Visiting DST node" << endl;
	// Ignore
}

void SmoothAutoDiff::visit( ASSERT_node& node ) {
    DistanceGrad* dg = d(node, cur_idx);
    GradUtil::default_grad(dg->grad);
	//cout << "Visiting ASSERT node" << endl;
}

void SmoothAutoDiff::visit( CTRL_node& node ) {
	//cout << "Visiting CTRL node" << endl;
	string name = node.get_name();
	
	if (isFloat(node)) {
		int idx = -1;
		if (floatCtrls.find(name) != floatCtrls.end()) {
			idx = floatCtrls[name];
		} else {
			Assert(false, "All float holes should be handled by numerical solver");
		}
		ValueGrad* val = v(node, cur_idx);
		val->update(gsl_vector_get(ctrls, idx));
		val->set = true;
		GradUtil::default_grad(val->getGrad());
		gsl_vector_set(val->getGrad(), idx, 1.0);
		
	} else {
        Assert(false, "Bool ctrls not yet supported");
	}
}

void SmoothAutoDiff::visit( PLUS_node& node ) {
	//cout << "Visiting PLUS node" << endl;
	Assert(isFloat(node), "NYI: plus with ints");
	ValueGrad* val = v(node, cur_idx);
	ValueGrad* mval = v(node.mother, cur_idx);
	ValueGrad* fval = v(node.father, cur_idx);
	ValueGrad::vg_plus(mval, fval, val);
}

void SmoothAutoDiff::visit( TIMES_node& node ) {
	//cout << "Visiting TIMES node" << endl;
	Assert(isFloat(node), "NYI: times with ints");
	ValueGrad* val = v(node, cur_idx);
	ValueGrad* mval = v(node.mother, cur_idx);
	ValueGrad* fval = v(node.father, cur_idx);
	ValueGrad::vg_times(mval, fval, val);
}

void SmoothAutoDiff::visit(ARRACC_node& node )  {
	Assert(node.multi_mother.size() == 2, "NYI: SmoothAutoDiff ARRACC of size > 2");
	Assert(node.getOtype() == OutType::FLOAT, "NYI: SmoothAutoDiff ARRACC for bool nodes")
	
	ValueGrad* val  = v(node, cur_idx);
    ValueGrad* mval = v(node.multi_mother[0], cur_idx);
    ValueGrad* fval = v(node.multi_mother[1], cur_idx);
    int cval = getInputValue(node.mother);
    if (cval == 1) {
        ValueGrad::vg_copy(fval, val);
    } else if (cval == 0) {
        ValueGrad::vg_copy(mval, val);
    } else {
        auto it = find(condNodes.begin(), condNodes.end(), node.mother->id);
        if (it != condNodes.end()) {
            int idx = it - condNodes.begin();
            bool v = getAssignment(cur_idx - 1, idx);
            if (v == 1) {
                ValueGrad::vg_copy(fval, val);
            } else {
                ValueGrad::vg_copy(mval, val);
            }
        } else {
            DistanceGrad* cond = d(node.mother, cur_idx);
            Assert(cond->set, "Cond must be set");
            bool v = cond->dist >= 0.0;
            if (v == 1) {
                ValueGrad::vg_copy(fval, val);
            } else {
                ValueGrad::vg_copy(mval, val);
            }
        }
    }
}

void SmoothAutoDiff::visit( DIV_node& node ) {
	//cout << "Visiting DIV node" << endl;
	Assert(isFloat(node), "NYI: div with ints");
	ValueGrad* val = v(node, cur_idx);
	ValueGrad* mval = v(node.mother, cur_idx);
	ValueGrad* fval = v(node.father, cur_idx);
	ValueGrad::vg_div(mval, fval, val);
}

void SmoothAutoDiff::visit( MOD_node& node ) {
	cout << "Visiting MOD node" << endl;
	Assert(false, "NYI: SmoothAutoDiff mod");
}

void SmoothAutoDiff::visit( NEG_node& node ) {
	//cout << "Visiting NEG node" << endl;
	Assert(isFloat(node), "NYI: neg with ints");
	ValueGrad* val = v(node, cur_idx);
	ValueGrad* mval = v(node.mother, cur_idx);
	ValueGrad::vg_neg(mval, val);
}

void SmoothAutoDiff::visit( CONST_node& node ) {
	if (node.getOtype() == OutType::FLOAT) {
		ValueGrad* val = v(node, cur_idx);
		val->update(node.getFval());
		val->set = true;
		GradUtil::default_grad(val->getGrad());
	} else {
		int val = node.getVal();
		ValueGrad* vg = v(node, cur_idx);
		DistanceGrad* dg = d(node, cur_idx);
		if (node.getOtype() == OutType::BOOL) {
			dg->dist = (val == 1) ? 1000 : -1000;
			GradUtil::default_grad(dg->grad);
			dg->set = true;
		} else {
            Assert(false, "Integers are not handled yet");
		}
	}
}

void SmoothAutoDiff::visit( LT_node& node ) {
	DistanceGrad* dg = d(node, cur_idx);
	
	// Comput the value from the parents
	Assert(isFloat(node.mother) && isFloat(node.father), "NYI: SmoothAutoDiff for lt with integer parents");
	ValueGrad* mval = v(node.mother, cur_idx);
	ValueGrad* fval = v(node.father, cur_idx);
	
	ValueGrad::vg_lt(mval, fval, dg);
}

void SmoothAutoDiff::visit( EQ_node& node ) {
	//cout << "Visiting EQ node" << endl;
	Assert(false, "NYI: SmoothAutoDiff for eq");
}

void SmoothAutoDiff::visit( AND_node& node ) {
	DistanceGrad* dg = d(node, cur_idx);
    // Compute the value from the parents
    DistanceGrad* mdist = d(node.mother, cur_idx);
    DistanceGrad* fdist = d(node.father, cur_idx);
	if (mdist->set && fdist->set) {
		DistanceGrad::dg_and(mdist, fdist, dg);
	} else {
		dg->set = false;
	}
}

void SmoothAutoDiff::visit( OR_node& node ) {
	DistanceGrad* dg = d(node, cur_idx);
	// Compute the value from the parents
	DistanceGrad* mdist = d(node.mother, cur_idx);
	DistanceGrad* fdist = d(node.father, cur_idx);
	if (mdist->set && fdist->set) {
		DistanceGrad::dg_or(mdist, fdist, dg);
	} else {
		dg->set = false;
	}
}

void SmoothAutoDiff::visit( NOT_node& node ) {
	DistanceGrad* dg = d(node, cur_idx);
	DistanceGrad* mdist = d(node.mother, cur_idx);

	if (mdist->set) {
		DistanceGrad::dg_not(mdist, dg);
	} else {
		dg->set = false;
	}
}

void SmoothAutoDiff::visit( ARRASS_node& node ) {
	cout << "Visiting ARRASS node" << endl;
	Assert(false, "NYI: SmoothAutoDiff for arrass");
}

void SmoothAutoDiff::visit( UFUN_node& node ) {
    DistanceGrad* dg = d(node, cur_idx);
    GradUtil::default_grad(dg->grad);
	ValueGrad* val = v(node, cur_idx);
	ValueGrad* mval = v(node.multi_mother[0], cur_idx);
	
	const string& name = node.get_ufname();
	if (name == "_cast_int_float_math") {
		ValueGrad::vg_cast_int_float(mval, val);
	} else if (name == "arctan_math") {
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
}

void SmoothAutoDiff::visit( TUPLE_R_node& node) {
	if (node.mother->type == bool_node::UFUN) {
		Assert(((UFUN_node*)(node.mother))->multi_mother.size() == 1, "NYI"); // TODO: This assumes that the ufun has a single output
		ValueGrad* mval = v(node.mother, cur_idx);
		ValueGrad* val = v(node, cur_idx);
		ValueGrad::vg_copy(mval, val);
	} else {
		Assert(false, "NYI");
	}
}


void SmoothAutoDiff::setInputs(Interface* inputValues_p) {
    inputValues = inputValues_p;
}


void SmoothAutoDiff::run(const gsl_vector* ctrls_p) {
    Assert(ctrls->size == ctrls_p->size, "SmoothAutoDiff ctrl sizes are not matching");
    
    for (int i = 0; i < ctrls->size; i++) {
        gsl_vector_set(ctrls, i, gsl_vector_get(ctrls_p, i));
    }

    cur_idx = 1;
    condNodes.clear();
    for (int i = 0; i < bdag.size(); i++) {
        bdag[i]->accept(*this);
    }
    vector<pair<double, int>> condDists;
    //cout << "Conditions: " << endl;
    for (int i = 0; i < bdag.size(); i++) {
        if (inputValues->isInput(i) && !inputValues->hasValue(i)) {
            if (Util::hasArraccChild(bdag[i])) {
                double dist = d(bdag[i], 1)->dist;
                double cost = abs(dist);
                condDists.push_back(make_pair(cost, i));
                //cout << i << " " <<  (bdag[i]->lprint()) << " " << dist << endl;
            }
        }
    }
    sort(condDists.begin(), condDists.end());
    if (condDists.size() == 0) {
        for (int i = 0; i < bdag.size(); i++) {
            ValueGrad* vg0 = v(bdag[i], 0);
            ValueGrad* vg1 = v(bdag[i], 1);
            ValueGrad::vg_copy(vg1, vg0);

            DistanceGrad* dg0 = d(bdag[i], 0);
            DistanceGrad* dg1 = d(bdag[i], 1);
            DistanceGrad::dg_copy(dg1, dg0);
        }
        return;
    }
    int ctr = 0;
    for (int i = 0; i < condDists.size(); i++) {
        int nodeid = get<1>(condDists[i]);
        double dist = get<0>(condDists[i]);
        DistanceGrad* dg = d(bdag[nodeid], 1);
        //cout << bdag[nodeid]->lprint() << " " << dg->dist << " " << gsl_vector_get(dg->grad, 0) << endl;
        ctr++;
        if (dist*GradUtil::BETA < -10) break;
        if (condNodes.size() < MAX_CONDS) {
            condNodes.push_back(nodeid);
        }
        //if (condNodes.size() >= MAX_CONDS) break;
    }
    //cout << "Conds: " << ctr << " " << MAX_CONDS << endl;

    //cout << "Considering nodes: " << Util::print(condNodes) << endl;

    int numRegions = pow(2, condNodes.size());
    for (cur_idx = 1; cur_idx < numRegions + 1; cur_idx++) {
        for (int i = 0; i < bdag.size(); i++) {
            bdag[i]->accept(*this);
        }

        // get the multiplication coefficient and gradient in GradUtil::tmp
        double s = 1.0;
        GradUtil::default_grad(GradUtil::tmp);
        for (int i = 0; i < condNodes.size(); i++) {
            DistanceGrad* dg = d(bdag[condNodes[i]], cur_idx);
            bool v = getAssignment(cur_idx - 1, i);
            //cout << v << ",";
            double s1;
            if (v == 1) {
                s1 = GradUtil::sigmoid(dg->dist, dg->grad, GradUtil::tmp1);
            } else {
                GradUtil::compute_neg_grad(dg->grad, GradUtil::tmp2);
                s1 = GradUtil::sigmoid(-dg->dist, GradUtil::tmp2, GradUtil::tmp1);
            }
            GradUtil::compute_mult_grad(s, s1, GradUtil::tmp, GradUtil::tmp1, GradUtil::tmp3);
            gsl_vector_memcpy(GradUtil::tmp, GradUtil::tmp3);
            s = s*s1;
        }
        //cout << endl;
        //cout << "Factor: " << s <<  " (" << gsl_vector_get(GradUtil::tmp, 0) << ")" << endl;

        // multiple add nodes values by the above multiplication coefficient

        for (int i = 0; i < bdag.size(); i++) {
            ValueGrad* vg0 = v(bdag[i], 0);
            ValueGrad* vg1 = v(bdag[i], cur_idx);
            DistanceGrad* dg0 = d(bdag[i], 0);
            DistanceGrad* dg1 = d(bdag[i], cur_idx);
            if (bdag[i]->getOtype() == OutType::BOOL && dg1->set) {
                //cout << bdag[i]->lprint() << " " << dg0->dist  << " (" << gsl_vector_get(dg0->grad, 0) << ")" << " " << dg1->dist << " (" << gsl_vector_get(dg1->grad, 0) << ")" << " ";
                if (cur_idx == 1) {
                    dg0->dist = 0;
                    GradUtil::default_grad(dg0->grad);
                }
                double v = s * dg1->dist;
                GradUtil::compute_mult_grad(s, dg1->dist, GradUtil::tmp, dg1->grad, GradUtil::tmp1);
                gsl_blas_daxpy(1.0, GradUtil::tmp1, dg0->grad);
                dg0->dist = dg0->dist + v;
                dg0->set = true;
                //cout << dg0->dist  << " (" << gsl_vector_get(dg0->grad, 0) << ")" << endl;
            } 
            if (vg1->set) {
                //cout << bdag[i]->lprint() << " " << vg0->getVal() << " (" <<gsl_vector_get(vg0->getGrad(), 0) << ")" << " " << vg1->getVal() << " (" << gsl_vector_get(vg1->getGrad(), 0) << ")" << " ";
                if (cur_idx == 1) {
                    vg0->update(0);
                    GradUtil::default_grad(vg0->getGrad());
                }
                double v = s * vg1->getVal();
                GradUtil::compute_mult_grad(s, vg1->getVal(), GradUtil::tmp, vg1->getGrad(), GradUtil::tmp1);
                gsl_blas_daxpy(1.0, GradUtil::tmp1, vg0->getGrad());
                vg0->update(vg0->getVal() + v);
                vg0->set = true;
                //cout << vg0->getVal() << " (" << gsl_vector_get(vg0->getGrad(), 0) << ")" << endl;
            }
        }
    }

    /*for (int i = 0; i < bdag.size(); i++) {
        ValueGrad* vg0 = v(bdag[i], 0);
        DistanceGrad* dg0 = d(bdag[i], 0);
        if (bdag[i]->getOtype() == OutType::BOOL && dg0->set) {
            cout << bdag[i]->lprint() << " " << dg0->dist;
            for (int j = 1; j <= numRegions; j++) {
                cout << d(bdag[i], j)->dist << " ";
            }
            cout << endl;
        } else {
            cout << bdag[i]->lprint() << " " << vg0->getVal();
            for (int j = 1; j <= numRegions; j++) {
                cout << v(bdag[i], j)->getVal() << " ";
            }
            cout << endl;
        }
    }*/
}




double SmoothAutoDiff::getErrorOnConstraint(int nodeid, gsl_vector* grad) { // Negative means errors TODO: name is confusing
    bool_node* node = bdag[nodeid];
    if (Util::isSqrt(node)) {
        return getSqrtError(node, grad);
    } else if (node->type == bool_node::ASSERT) {
        return getAssertError(node, grad);
    } else if (node->type == bool_node::CTRL && node->getOtype() == OutType::BOOL) {
        return getBoolCtrlError(node, grad);
    } else if (node->getOtype() == OutType::BOOL) {
        return getBoolExprError(node, grad);
    } else {
        Assert(false, "Unknown node for computing error");
    }
}

double SmoothAutoDiff::getSqrtError(bool_node* node, gsl_vector* grad) {
    UFUN_node* un = (UFUN_node*) node;
    bool_node* x = un->multi_mother[0];
    ValueGrad* val = v(x, 0);
    Assert(val->set, "Sqrt node is not set");
    gsl_vector_memcpy(grad, val->getGrad());
    return val->getVal();
}

double SmoothAutoDiff::getAssertError(bool_node* node, gsl_vector* grad) {
    DistanceGrad* dg = d(node->mother, 0);
    Assert(dg->set, "Assert node is not set");
    gsl_vector_memcpy(grad, dg->grad);
    return dg->dist;
}

double SmoothAutoDiff::getBoolCtrlError(bool_node* node, gsl_vector* grad) {
    string name = node->get_name();
    int idx = -1;
    if (floatCtrls.find(name) != floatCtrls.end()) {
        idx = floatCtrls[name];
    } else {
        Assert(false, "qwe8yqwi");
    }
    GradUtil::default_grad(grad);
    gsl_vector_set(grad, idx, 1.0);
    double dist = gsl_vector_get(ctrls, idx);
    
    gsl_vector_scale(grad, 2*dist - 1);
    return 0.1 - dist*(1-dist); // TODO: magic numbers
}

double SmoothAutoDiff::getBoolExprError(bool_node* node, gsl_vector* grad) {
    Assert (inputValues->hasValue(node->id), "Boolean expression not yet set");
    int val = inputValues->getValue(node->id);
    
    DistanceGrad* dg = d(node, 0);
    Assert(dg->set, "Boolean expression distance is not set");
    gsl_vector_memcpy(grad, dg->grad);
    if (val == 1) {
        return dg->dist;
    } else {
        gsl_vector_scale(grad, -1.0);
        return -dg->dist;
    }
}




double SmoothAutoDiff::getErrorOnConstraint(int nodeid) {
    bool_node* node = bdag[nodeid];
    if (Util::isSqrt(node)) {
        return getSqrtError(node);
    } else if (node->type == bool_node::ASSERT) {
        return getAssertError(node);
    } else if (node->type == bool_node::CTRL && node->getOtype() == OutType::BOOL) {
        return getBoolCtrlError(node);
    } else if (node->getOtype() == OutType::BOOL) {
        return getBoolExprError(node);
    } else {
        Assert(false, "Unknown node for computing error");
    }
}

double SmoothAutoDiff::getSqrtError(bool_node* node) {
    UFUN_node* un = (UFUN_node*) node;
    bool_node* x = un->multi_mother[0];
    ValueGrad* val = v(x, 0);
    Assert(val->set, "Sqrt node is not set");
    return val->getVal();
}

double SmoothAutoDiff::getAssertError(bool_node* node) {
    DistanceGrad* dg = d(node->mother, 0);
    Assert(dg->set, "Assert node is not set");
    return dg->dist;
}

double SmoothAutoDiff::getBoolCtrlError(bool_node* node) {
    string name = node->get_name();
    int idx = -1;
    if (floatCtrls.find(name) != floatCtrls.end()) {
        idx = floatCtrls[name];
    } else {
        Assert(false, "qwe8yqwi");
    }
    double dist = gsl_vector_get(ctrls, idx);
    return 0.1 - dist*(1-dist); // TODO: magic numbers
}

double SmoothAutoDiff::getBoolExprError(bool_node* node) {
    Assert (inputValues->hasValue(node->id), "Boolean expression not yet set");
    int val = inputValues->getValue(node->id);
    
    DistanceGrad* dg = d(node, 0);
    Assert(dg->set, "Boolean expression distance is not set");
    if (val == 1) {
        return dg->dist;
    } else {
        return -dg->dist;
    }
}



