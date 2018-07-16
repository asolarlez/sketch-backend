#include "KLocalityAutoDiff.h"
#include <algorithm>
#include <limits>
#include <math.h>


KLocalityAutoDiff::KLocalityAutoDiff(BooleanDAG& bdag_p, map<string, int>& floatCtrls_p): bdag(bdag_p), floatCtrls(floatCtrls_p) {
	values.resize(bdag.size());
	distances.resize(bdag.size());
    paths.resize(bdag.size());
    sizes.resize(bdag.size(), 0);
    for (int i = 0; i < bdag.size(); i++) {
        values[i].resize(MAX_REGIONS, NULL);
        distances[i].resize(MAX_REGIONS, NULL);
        paths[i].resize(MAX_REGIONS, NULL);
    }
    nctrls = floatCtrls.size();
    if (nctrls == 0) nctrls = 1;
	ctrls = gsl_vector_alloc(nctrls);


    for (BooleanDAG::iterator node_it = bdag.begin(); node_it != bdag.end(); ++node_it) {
        bool_node* n = *node_it;
        set<int> c;
        const vector<bool_node*>& parents = n->parents();
        for (int i = 0; i < parents.size(); i++) {
            bool_node* parent = parents[i];
            set<int>& pc = conds[parent->id];
            c.insert(pc.begin(), pc.end());
        }
        if (n->type == bool_node::ARRACC) {
            c.insert(n->mother->id);
        }
        cout << n->lprint() << " " << Util::print(c) << endl;
        conds.push_back(c);
    }
}

KLocalityAutoDiff::~KLocalityAutoDiff(void) {
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
    for (int i = 0; i < paths.size(); i++) {
        for (int j = 0; j < paths[i].size(); j++) {
            if (paths[i][j] != NULL) {
                delete paths[i][j];
            }
        }
    }
}

void KLocalityAutoDiff::visit( SRC_node& node ) {
	//cout << "Visiting SRC node" << endl;
	Assert(false, "NYI: KLocalityAutoDiff for src");
}

void KLocalityAutoDiff::visit( DST_node& node ) {
	//cout << "Visiting DST node" << endl;
	// Ignore
}

void KLocalityAutoDiff::copyNodes(bool_node& node, bool_node* m) {
    int msize = size(m);
    int k = 1;
    for (int i = 1; i <= msize; i++) {
        ValueGrad* val = v(node, k);
        ValueGrad* mval = v(m, i);
        ValueGrad::vg_copy(mval, val);

        DistanceGrad* dg = d(node, k);
        DistanceGrad* mdist = d(m, i);
        DistanceGrad::dg_copy(mdist, dg);
        k++;
    }
    Assert(k == msize + 1, "Something is wrong here");
    setsize(node, msize);
}

void KLocalityAutoDiff::visit( ASSERT_node& node ) {
	//cout << "Visiting ASSERT node" << endl;
    copyNodes(node, node.mother);
    
}

void KLocalityAutoDiff::visit( CTRL_node& node ) {
	//cout << "Visiting CTRL node" << endl;
	string name = node.get_name();
	
	if (isFloat(node)) {
		int idx = -1;
		if (floatCtrls.find(name) != floatCtrls.end()) {
			idx = floatCtrls[name];
		} else {
			Assert(false, "All float holes should be handled by numerical solver");
		}
		ValueGrad* val = v(node, 1);
		val->update(gsl_vector_get(ctrls, idx));
		val->set = true;
		GradUtil::default_grad(val->getGrad());
		gsl_vector_set(val->getGrad(), idx, 1.0);
        
        DistanceGrad* dg = d(node, 1);
        dg->dist = 1;
        GradUtil::default_grad(dg->grad);
        dg->set = true;
        setsize(node, 1);
	} else {
        Assert(false, "Bool ctrls not yet supported");
	}
}


void KLocalityAutoDiff::visit( PLUS_node& node ) {
	//cout << "Visiting PLUS node" << endl;
	Assert(isFloat(node), "NYI: plus with ints");
    
    if (size(node.mother) == size(node.father)) {
        for (int i = 1; i <= size(node.mother); i++) {
            ValueGrad* val = v(node, i);
            ValueGrad* mval = v(node.mother, i);
            ValueGrad* fval = v(node.father, i);
            ValueGrad::vg_plus(mval, fval, val);

            DistanceGrad* dg = d(node, i);
            DistanceGrad* mdist = d(node.mother, i);
            DistanceGrad::dg_copy(mdist, dg);
        }
        setsize(node, size(node.mother));
    } else if (size(node.mother) == 1) {
        ValueGrad* mval = v(node.mother, 1);
        for (int i = 1; i <= size(node.father); i++) {
            ValueGrad* val = v(node, i);
            ValueGrad* fval = v(node.father, i);
            ValueGrad::vg_plus(mval, fval, val);

            DistanceGrad* dg = d(node, i);
            DistanceGrad* fdist = d(node.father, i);
            DistanceGrad::dg_copy(fdist, dg);
        }
        setsize(node, size(node.father));
    } else if (size(node.father) == 1) {
        ValueGrad* fval = v(node.father, 1);
        for (int i = 1; i <= size(node.mother); i++) {
            ValueGrad* val = v(node, i);
            ValueGrad* mval = v(node.mother, i);
            ValueGrad::vg_plus(mval, fval, val);

            DistanceGrad* dg = d(node, i);
            DistanceGrad* mdist = d(node.mother, i);
            DistanceGrad::dg_copy(mdist, dg);
        }
        setsize(node, size(node.mother));
    } else {
        Assert(false, "NYI");
    }

}

void KLocalityAutoDiff::visit( TIMES_node& node ) {
	//cout << "Visiting TIMES node" << endl;
	Assert(isFloat(node), "NYI: times with ints");
	if (size(node.mother) == size(node.father)) {
        for (int i = 1; i <= size(node.mother); i++) {
            ValueGrad* val = v(node, i);
            ValueGrad* mval = v(node.mother, i);
            ValueGrad* fval = v(node.father, i);
            ValueGrad::vg_times(mval, fval, val);

            DistanceGrad* dg = d(node, i);
            DistanceGrad* mdist = d(node.mother, i);
            DistanceGrad::dg_copy(mdist, dg);
        }
        setsize(node, size(node.mother));
    } else if (size(node.mother) == 1) {
        ValueGrad* mval = v(node.mother, 1);
        for (int i = 1; i <= size(node.father); i++) {
            ValueGrad* val = v(node, i);
            ValueGrad* fval = v(node.father, i);
            ValueGrad::vg_times(mval, fval, val);

            DistanceGrad* dg = d(node, i);
            DistanceGrad* fdist = d(node.father, i);
            DistanceGrad::dg_copy(fdist, dg);
        }
        setsize(node, size(node.father));
    } else if (size(node.father) == 1) {
        ValueGrad* fval = v(node.father, 1);
        for (int i = 1; i <= size(node.mother); i++) {
            ValueGrad* val = v(node, i);
            ValueGrad* mval = v(node.mother, i);
            ValueGrad::vg_times(mval, fval, val);

            DistanceGrad* dg = d(node, i);
            DistanceGrad* mdist = d(node.mother, i);
            DistanceGrad::dg_copy(mdist, dg);
        }
        setsize(node, size(node.mother));
    } else {
        Assert(false, "NYI");
    }
}

void KLocalityAutoDiff::visit(ARRACC_node& node )  {
	Assert(node.multi_mother.size() == 2, "NYI: KLocalityAutoDiff ARRACC of size > 2");
	Assert(node.getOtype() == OutType::FLOAT, "NYI: KLocalityAutoDiff ARRACC for bool nodes")
	int cval = getInputValue(node.mother);
    if (cval == 1) {
        copyNodes(node, node.multi_mother[1]);
        return;
    } else if (cval == 0) {
        copyNodes(node, node.multi_mother[0]);
        return;
    } 
    int csize = size(node.mother);
    int msize = size(node.multi_mother[0]);
    int fsize = size(node.multi_mother[1]);
    //cout << csize << " " << msize << " " << fsize << endl;
    if (csize == 1) {
        ValueGrad* cval = v(node.mother, 1);
        double s = GradUtil::sigmoid(cval->getVal());
        //cout << s << endl;
        if (false && (s > 0.9 || s < 0.1)) {
            if (msize == fsize) {
                for (int i = 1; i <= msize; i++) {
                    ValueGrad* val = v(node, i);
                    ValueGrad* mval = v(node.multi_mother[0], i);
                    ValueGrad* fval = v(node.multi_mother[1], i);
                    double v = ValueGrad::vg_ite(mval, fval, cval->getVal(), cval->getGrad(), val->getGrad());
                    val->update(v);
                    val->set = true;

                    DistanceGrad* dg = d(node, i);
                    dg->dist = 1;
                    GradUtil::default_grad(dg->grad);
                    dg->set = true;
                }
                setsize(node, msize);
            } 
        } 
    }  
    if (msize == fsize && msize + fsize < MAX_REGIONS) {
        int k = 1;
        for (int i = 1; i <= fsize; i++) {
            ValueGrad* val = v(node, k);
            ValueGrad* fval = v(node.multi_mother[1], i);
            ValueGrad* cval = v(node.mother, i);
            ValueGrad::vg_copy(fval, val);

            DistanceGrad* dg = d(node, k);
            DistanceGrad* fdist = d(node.multi_mother[1], i);
            double s = GradUtil::sigmoid(cval->getVal(), cval->getGrad(), GradUtil::tmp);
            GradUtil::compute_mult_grad(fdist->dist, s, fdist->grad, GradUtil::tmp, dg->grad);
            dg->dist = fdist->dist * s;
            dg->set = true;
            k++;
        }
        for (int i = 1; i <= msize; i++) {
            ValueGrad* val = v(node, k);
            ValueGrad* mval = v(node.multi_mother[0], i);
            ValueGrad* cval = v(node.mother, i);
            ValueGrad::vg_copy(mval, val);

            DistanceGrad* dg = d(node, k);
            DistanceGrad* mdist = d(node.multi_mother[0], i);
            GradUtil::compute_neg_grad(cval->getGrad(), GradUtil::tmp1);
            double s = GradUtil::sigmoid(-cval->getVal(), GradUtil::tmp1, GradUtil::tmp);
            GradUtil::compute_mult_grad(mdist->dist, s, mdist->grad, GradUtil::tmp, dg->grad);
            dg->dist = mdist->dist * s;
            dg->set = true;
            k++;
        }

        setsize(node, k-1);
                
    } else if (msize == 1 && 2*fsize < MAX_REGIONS) {
        int k = 1;
        for (int i = 1; i <= fsize; i++) {
            ValueGrad* val = v(node, k);
            ValueGrad* fval = v(node.multi_mother[1], i);
            ValueGrad* cval = v(node.mother, i);
            ValueGrad::vg_copy(fval, val);

            DistanceGrad* dg = d(node, k);
            DistanceGrad* fdist = d(node.multi_mother[1], i);
            double s = GradUtil::sigmoid(cval->getVal(), cval->getGrad(), GradUtil::tmp);
            GradUtil::compute_mult_grad(fdist->dist, s, fdist->grad, GradUtil::tmp, dg->grad);
            dg->dist = fdist->dist * s;
            dg->set = true;
            k++;
        }
        for (int i = 1; i <= fsize; i++) {
            ValueGrad* val = v(node, k);
            ValueGrad* mval = v(node.multi_mother[0], 1);
            ValueGrad* cval = v(node.mother, i);
            ValueGrad::vg_copy(mval, val);

            DistanceGrad* dg = d(node, k);
            DistanceGrad* mdist = d(node.mother, i);
            GradUtil::compute_neg_grad(cval->getGrad(), GradUtil::tmp1);
            double s = GradUtil::sigmoid(-cval->getVal(), GradUtil::tmp1, GradUtil::tmp);
            GradUtil::compute_mult_grad(mdist->dist, s, mdist->grad, GradUtil::tmp, dg->grad);
            dg->dist = mdist->dist * s;
            dg->set = true;
            k++;
        }

        setsize(node, k-1);
    } else if (msize == fsize && msize + fsize >= MAX_REGIONS) {
        int k = 1;
        for (int i = 1; i <= fsize; i+=2) {
            ValueGrad* val = v(node, k);
            DistanceGrad* dg = d(node, k);

            ValueGrad* fval1 = v(node.multi_mother[1], i);
            ValueGrad* cval1 = v(node.mother, i);
            DistanceGrad* fdist1 = d(node.multi_mother[1], i);
            double s1 = GradUtil::sigmoid(cval1->getVal(), cval1->getGrad(), GradUtil::tmp);
            GradUtil::compute_mult_grad(fdist1->dist, s1, fdist1->grad, GradUtil::tmp, GradUtil::tmp1);
            double d1 = fdist1->dist * s1;
            double v1 = fval1->getVal();

            ValueGrad* fval2 = v(node.multi_mother[1], i+1);
            ValueGrad* cval2 = v(node.mother, i+1);
            DistanceGrad* fdist2 = d(node.multi_mother[1], i+1);
            double s2 = GradUtil::sigmoid(cval2->getVal(), cval2->getGrad(), GradUtil::tmp);
            GradUtil::compute_mult_grad(fdist2->dist, s2, fdist2->grad, GradUtil::tmp, GradUtil::tmp2);
            double d2 = fdist2->dist * s2;
            double v2 = fval2->getVal();


            dg->dist = d1 + d2;
            GradUtil::compute_plus_grad(GradUtil::tmp1, GradUtil::tmp2, dg->grad);
            double v = (v1*d1 + v2*d2);

            GradUtil::compute_mult_grad(v1, d1, fval1->getGrad(), GradUtil::tmp1, val->getGrad());
            GradUtil::compute_mult_grad(v2, d2, fval2->getGrad(), GradUtil::tmp2, GradUtil::tmp);
            gsl_blas_daxpy(1.0, GradUtil::tmp, val->getGrad());

            if (d1 + d2 > 1e-4) {
                GradUtil::compute_div_grad(v, d1 + d2, val->getGrad(), dg->grad, GradUtil::tmp);
                gsl_vector_memcpy(val->getGrad(), GradUtil::tmp);
                val->update(v/(d1+d2));
            } else {
                val->update(v);
            }
            val->set = true;
            dg->set = true;
            k++;
        }
        for (int i = 1; i <= msize; i+=2) {
            ValueGrad* val = v(node, k);
            DistanceGrad* dg = d(node, k);

            ValueGrad* mval1 = v(node.multi_mother[0], i);
            ValueGrad* cval1 = v(node.mother, i);
            DistanceGrad* mdist1 = d(node.multi_mother[0], i);
            double s1 = GradUtil::sigmoid(-cval1->getVal(), GradUtil::tmp2, GradUtil::tmp);
            GradUtil::compute_mult_grad(mdist1->dist, s1, mdist1->grad, GradUtil::tmp, GradUtil::tmp1);
            double d1 = mdist1->dist * s1;
            double v1 = mval1->getVal();

            ValueGrad* mval2 = v(node.multi_mother[0], i+1);
            ValueGrad* cval2 = v(node.mother, i+1);
            DistanceGrad* mdist2 = d(node.multi_mother[0], i+1);
            GradUtil::compute_neg_grad(cval2->getGrad(), GradUtil::tmp2);
            double s2 = GradUtil::sigmoid(-cval2->getVal(), GradUtil::tmp2, GradUtil::tmp);
            GradUtil::compute_mult_grad(mdist2->dist, s2, mdist2->grad, GradUtil::tmp, GradUtil::tmp2);
            double d2 = mdist2->dist * s2;
            double v2 = mval2->getVal();


            dg->dist = d1 + d2;
            GradUtil::compute_plus_grad(GradUtil::tmp1, GradUtil::tmp2, dg->grad);
            double v = (v1*d1 + v2*d2);

            GradUtil::compute_mult_grad(v1, d1, mval1->getGrad(), GradUtil::tmp1, val->getGrad());
            GradUtil::compute_mult_grad(v2, d2, mval2->getGrad(), GradUtil::tmp2, GradUtil::tmp);
            gsl_blas_daxpy(1.0, GradUtil::tmp, val->getGrad());

            if (d1 + d2 > 1e-4) {
                GradUtil::compute_div_grad(v, d1 + d2, val->getGrad(),  dg->grad, GradUtil::tmp);
                gsl_vector_memcpy(val->getGrad(), GradUtil::tmp);
                val->update(v/(d1+d2));
            } else {
                val->update(v);
            }
            val->set = true;
            dg->set = true;
            k++;
        }

        setsize(node, k-1);
    } else if (msize == 1 && 2*fsize >= MAX_REGIONS) {
        int k = 1; 
        for (int i = 1; i <= fsize; i+=2) {
            ValueGrad* val = v(node, k);
            DistanceGrad* dg = d(node, k);

            ValueGrad* fval1 = v(node.multi_mother[1], i);
            ValueGrad* cval1 = v(node.mother, i);
            DistanceGrad* fdist1 = d(node.multi_mother[1], i);
            double s1 = GradUtil::sigmoid(cval1->getVal(), cval1->getGrad(), GradUtil::tmp);
            GradUtil::compute_mult_grad(fdist1->dist, s1, fdist1->grad, GradUtil::tmp, GradUtil::tmp1);
            double d1 = fdist1->dist * s1;
            double v1 = fval1->getVal();

            ValueGrad* fval2 = v(node.multi_mother[1], i+1);
            ValueGrad* cval2 = v(node.mother, i+1);
            DistanceGrad* fdist2 = d(node.multi_mother[1], i+1);
            double s2 = GradUtil::sigmoid(cval2->getVal(), cval2->getGrad(), GradUtil::tmp);
            GradUtil::compute_mult_grad(fdist2->dist, s2, fdist2->grad, GradUtil::tmp, GradUtil::tmp2);
            double d2 = fdist2->dist * s2;
            double v2 = fval2->getVal();


            dg->dist = d1 + d2;
            GradUtil::compute_plus_grad(GradUtil::tmp1, GradUtil::tmp2, dg->grad);
            double v = (v1*d1 + v2*d2);

            GradUtil::compute_mult_grad(v1, d1, fval1->getGrad(), GradUtil::tmp1, val->getGrad());
            GradUtil::compute_mult_grad(v2, d2, fval2->getGrad(), GradUtil::tmp2, GradUtil::tmp);
            gsl_blas_daxpy(1.0, GradUtil::tmp, val->getGrad());
            if ( d1 + d2 > 1e-4) {
                GradUtil::compute_div_grad(v, d1 + d2, val->getGrad(), dg->grad, GradUtil::tmp);
                gsl_vector_memcpy(val->getGrad(), GradUtil::tmp);
                val->update(v/(d1+d2));
            } else {
                val->update(v);
            }
            val->set = true;
            dg->set = true;
            k++;
        }
        for (int i = 1; i <= fsize; i+=2) {
            ValueGrad* val = v(node, k);
            DistanceGrad* dg = d(node, k);

            ValueGrad* mval = v(node.multi_mother[0], 1);
            ValueGrad::vg_copy(mval, val);

            ValueGrad* cval1 = v(node.mother, i);
            DistanceGrad* mdist1 = d(node.mother, i);
            GradUtil::compute_neg_grad(cval1->getGrad(), GradUtil::tmp2);
            double s1 = GradUtil::sigmoid(-cval1->getVal(), GradUtil::tmp2, GradUtil::tmp);
            GradUtil::compute_mult_grad(mdist1->dist, s1, mdist1->grad, GradUtil::tmp, GradUtil::tmp1);
            double d1 = mdist1->dist * s1;

             ValueGrad* cval2 = v(node.mother, i+1);
            DistanceGrad* mdist2 = d(node.mother, i+1);
            GradUtil::compute_neg_grad(cval2->getGrad(), GradUtil::tmp2);
            double s2 = GradUtil::sigmoid(-cval2->getVal(), GradUtil::tmp2, GradUtil::tmp);
            GradUtil::compute_mult_grad(mdist2->dist, s2, mdist2->grad, GradUtil::tmp, GradUtil::tmp2);
            double d2 = mdist2->dist * s2;


            dg->dist = d1 + d2;
            GradUtil::compute_plus_grad(GradUtil::tmp1, GradUtil::tmp2, dg->grad);
            dg->set = true;
            k++;
        }
        setsize(node, k-1);
    }
}

void KLocalityAutoDiff::visit( DIV_node& node ) {
	//cout << "Visiting DIV node" << endl;
	Assert(isFloat(node), "NYI: div with ints");
	if (size(node.mother) == size(node.father)) {
        for (int i = 1; i <= size(node.mother); i++) {
            ValueGrad* val = v(node, i);
            ValueGrad* mval = v(node.mother, i);
            ValueGrad* fval = v(node.father, i);
            ValueGrad::vg_div(mval, fval, val);

            DistanceGrad* dg = d(node, i);
            DistanceGrad* mdist = d(node.mother, i);
            DistanceGrad::dg_copy(mdist, dg);
        }
        setsize(node, size(node.mother));
    } else if (size(node.mother) == 1) {
        ValueGrad* mval = v(node.mother, 1);
        for (int i = 1; i <= size(node.father); i++) {
            ValueGrad* val = v(node, i);
            ValueGrad* fval = v(node.father, i);
            ValueGrad::vg_div(mval, fval, val);

            DistanceGrad* dg = d(node, i);
            DistanceGrad* fdist = d(node.father, i);
            DistanceGrad::dg_copy(fdist, dg);
        }
        setsize(node, size(node.father));
    } else if (size(node.father) == 1) {
        ValueGrad* fval = v(node.father, 1);
        for (int i = 1; i <= size(node.mother); i++) {
            ValueGrad* val = v(node, i);
            ValueGrad* mval = v(node.mother, i);
            ValueGrad::vg_div(mval, fval, val);

            DistanceGrad* dg = d(node, i);
            DistanceGrad* mdist = d(node.mother, i);
            DistanceGrad::dg_copy(mdist, dg);
        }
        setsize(node, size(node.mother));
    } else {
        Assert(false, "NYI");
    }
}

void KLocalityAutoDiff::visit( MOD_node& node ) {
	cout << "Visiting MOD node" << endl;
	Assert(false, "NYI: KLocalityAutoDiff mod");
}

void KLocalityAutoDiff::visit( NEG_node& node ) {
	//cout << "Visiting NEG node" << endl;
	Assert(isFloat(node), "NYI: neg with ints");
    int msize = size(node.mother);
    int k = 1;
    for (int i = 1; i <= msize; i++) {
        ValueGrad* val = v(node, k);
        ValueGrad* mval = v(node.mother, i);
        ValueGrad::vg_neg(mval, val);

        DistanceGrad* dg = d(node, k);
        DistanceGrad* mdist = d(node.mother, i);
        DistanceGrad::dg_copy(mdist, dg);
        k++;
    }
	setsize(node, msize);
}

void KLocalityAutoDiff::visit( CONST_node& node ) {
	if (node.getOtype() == OutType::FLOAT) {
		ValueGrad* val = v(node, 1);
		val->update(node.getFval());
		val->set = true;
		GradUtil::default_grad(val->getGrad());

        DistanceGrad* dg = d(node, 1);
        dg->dist = 1;
        GradUtil::default_grad(dg->grad);
        dg->set = true;
        setsize(node, 1);
	} else {
		if (node.getOtype() == OutType::BOOL) {
            int val = node.getVal();
            ValueGrad* vg = v(node, 1);
            vg->update((val == 1) ? 1000 : -1000);
            GradUtil::default_grad(vg->getGrad());
            vg->set = true;
            
            DistanceGrad* dg = d(node, 1);
            dg->dist = 1;
            GradUtil::default_grad(dg->grad);
            dg->set = true;
            setsize(node, 1);
			
		} else {
            Assert(false, "Integers are not handled yet");
		}
	}
}

void KLocalityAutoDiff::visit( LT_node& node ) {
    Assert(isFloat(node.mother) && isFloat(node.father), "NYI: KLocalityAutoDiff for lt with integer parents");

    if (size(node.mother) == size(node.father)) {
        for (int i = 1; i <= size(node.mother); i++) {
            ValueGrad* val = v(node, i);
            ValueGrad* mval = v(node.mother, i);
            ValueGrad* fval = v(node.father, i);
            ValueGrad::vg_lt(mval, fval, val);

            DistanceGrad* dg = d(node, i);
            DistanceGrad* mdist = d(node.mother, i);
            DistanceGrad::dg_copy(mdist, dg);
        }
        setsize(node, size(node.mother));
    } else if (size(node.mother) == 1) {
        ValueGrad* mval = v(node.mother, 1);
        for (int i = 1; i <= size(node.father); i++) {
            ValueGrad* val = v(node, i);
            ValueGrad* fval = v(node.father, i);
            ValueGrad::vg_lt(mval, fval, val);

            DistanceGrad* dg = d(node, i);
            DistanceGrad* fdist = d(node.father, i);
            DistanceGrad::dg_copy(fdist, dg);
        }
        setsize(node, size(node.father));
    } else if (size(node.father) == 1) {
        ValueGrad* fval = v(node.father, 1);
        for (int i = 1; i <= size(node.mother); i++) {
            ValueGrad* val = v(node, i);
            ValueGrad* mval = v(node.mother, i);
            ValueGrad::vg_lt(mval, fval, val);

            DistanceGrad* dg = d(node, i);
            DistanceGrad* mdist = d(node.mother, i);
            DistanceGrad::dg_copy(mdist, dg);
        }
        setsize(node, size(node.mother));
    } else {
        Assert(false, "NYI");
    }
}

void KLocalityAutoDiff::visit( EQ_node& node ) {
	//cout << "Visiting EQ node" << endl;
	Assert(false, "NYI: KLocalityAutoDiff for eq");
}

void KLocalityAutoDiff::visit( AND_node& node ) {
    if (size(node.mother) == size(node.father)) {
        for (int i = 1; i <= size(node.mother); i++) {
            ValueGrad* val = v(node, i);
            ValueGrad* mval = v(node.mother, i);
            ValueGrad* fval = v(node.father, i);
            ValueGrad::vg_and(mval, fval, val);

            DistanceGrad* dg = d(node, i);
            DistanceGrad* mdist = d(node.mother, i);
            DistanceGrad::dg_copy(mdist, dg);
        }
        setsize(node, size(node.mother));
    } else if (size(node.mother) == 1) {
        ValueGrad* mval = v(node.mother, 1);
        for (int i = 1; i <= size(node.father); i++) {
            ValueGrad* val = v(node, i);
            ValueGrad* fval = v(node.father, i);
            ValueGrad::vg_and(mval, fval, val);

            DistanceGrad* dg = d(node, i);
            DistanceGrad* fdist = d(node.father, i);
            DistanceGrad::dg_copy(fdist, dg);
        }
        setsize(node, size(node.father));
    } else if (size(node.father) == 1) {
        ValueGrad* fval = v(node.father, 1);
        for (int i = 1; i <= size(node.mother); i++) {
            ValueGrad* val = v(node, i);
            ValueGrad* mval = v(node.mother, i);
            ValueGrad::vg_and(mval, fval, val);

            DistanceGrad* dg = d(node, i);
            DistanceGrad* mdist = d(node.mother, i);
            DistanceGrad::dg_copy(mdist, dg);
        }
        setsize(node, size(node.mother));
    } else {
        Assert(false, "NYI");
    }
}

void KLocalityAutoDiff::visit( OR_node& node ) {
    if (size(node.mother) == size(node.father)) {
        for (int i = 1; i <= size(node.mother); i++) {
            ValueGrad* val = v(node, i);
            ValueGrad* mval = v(node.mother, i);
            ValueGrad* fval = v(node.father, i);
            ValueGrad::vg_or(mval, fval, val);

            DistanceGrad* dg = d(node, i);
            DistanceGrad* mdist = d(node.mother, i);
            DistanceGrad::dg_copy(mdist, dg);
        }
        setsize(node, size(node.mother));
    } else if (size(node.mother) == 1) {
        ValueGrad* mval = v(node.mother, 1);
        for (int i = 1; i <= size(node.father); i++) {
            ValueGrad* val = v(node, i);
            ValueGrad* fval = v(node.father, i);
            ValueGrad::vg_or(mval, fval, val);

            DistanceGrad* dg = d(node, i);
            DistanceGrad* fdist = d(node.father, i);
            DistanceGrad::dg_copy(fdist, dg);
        }
        setsize(node, size(node.father));
    } else if (size(node.father) == 1) {
        ValueGrad* fval = v(node.father, 1);
        for (int i = 1; i <= size(node.mother); i++) {
            ValueGrad* val = v(node, i);
            ValueGrad* mval = v(node.mother, i);
            ValueGrad::vg_or(mval, fval, val);

            DistanceGrad* dg = d(node, i);
            DistanceGrad* mdist = d(node.mother, i);
            DistanceGrad::dg_copy(mdist, dg);
        }
        setsize(node, size(node.mother));
    } else {
        Assert(false, "NYI");
    }
}

void KLocalityAutoDiff::visit( NOT_node& node ) {
	int msize = size(node.mother);
    int k = 1;
    for (int i = 1; i <= msize; i++) {
        ValueGrad* val = v(node, k);
        ValueGrad* mval = v(node.mother, i);
        ValueGrad::vg_not(mval, val);

        DistanceGrad* dg = d(node, k);
        DistanceGrad* mdist = d(node.mother, i);
        DistanceGrad::dg_copy(mdist, dg);
        k++;
    }
    setsize(node, msize);
}

void KLocalityAutoDiff::visit( ARRASS_node& node ) {
	cout << "Visiting ARRASS node" << endl;
	Assert(false, "NYI: KLocalityAutoDiff for arrass");
}

void KLocalityAutoDiff::doUfun(UFUN_node& node, ValueGrad* mval, ValueGrad* val) {
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

void KLocalityAutoDiff::visit( UFUN_node& node ) {
    int msize = size(node.multi_mother[0]);
    int k = 1;
    for (int i = 1; i <= msize; i++) {
        ValueGrad* val = v(node, k);
        ValueGrad* mval = v(node.multi_mother[0], i);
        doUfun(node, mval, val);

        DistanceGrad* dg = d(node, k);
        DistanceGrad* mdist = d(node.multi_mother[0], i);
        DistanceGrad::dg_copy(mdist, dg);
        k++;
    }
    setsize(node, msize);
}

void KLocalityAutoDiff::visit( TUPLE_R_node& node) {
	if (node.mother->type == bool_node::UFUN) {
		Assert(((UFUN_node*)(node.mother))->multi_mother.size() == 1, "NYI"); // TODO: This assumes that the ufun has a single output
		copyNodes(node, node.mother);
	} else {
		Assert(false, "NYI");
	}
}


void KLocalityAutoDiff::setInputs(Interface* inputValues_p) {
    inputValues = inputValues_p;
}



double KLocalityAutoDiff::merge(bool_node* n, gsl_vector* grad) {
    double res = 0;
    GradUtil::default_grad(grad);
    Assert(grad != GradUtil::tmp, "Grad vector is being rewritten");
    //cout << n->lprint() << " ";
    for (int i = 1; i <= size(n); i++) {
        ValueGrad* vg = v(n, i);
        DistanceGrad* dg = d(n, i);
        //cout << "(" << vg->getVal() << "," << dg->dist << ") ";
        res += vg->getVal() * dg->dist;
        GradUtil::compute_mult_grad(vg->getVal(), dg->dist, vg->getGrad(), dg->grad, GradUtil::tmp);
        gsl_blas_daxpy(1.0, GradUtil::tmp, grad);
    }
    //cout << "total: " << res << endl;
    return res;
}


void KLocalityAutoDiff::run(const gsl_vector* ctrls_p) {
    Assert(ctrls->size == ctrls_p->size, "KLocalityAutoDiff ctrl sizes are not matching");
    
    for (int i = 0; i < ctrls->size; i++) {
        gsl_vector_set(ctrls, i, gsl_vector_get(ctrls_p, i));
    }
    cout << gsl_vector_get(ctrls, 0) << endl;

    for (int i = 0; i < bdag.size(); i++) {
        bool_node* n = bdag[i];
        n->accept(*this);
        //cout << n->lprint() << endl;
        if (size(n) == 0) {
            Assert(false, "Size is 0");
        }
        double dsum = 0;
        for (int k = 1; k <= size(n); k++) {
            ValueGrad* vg = v(n, k);
            DistanceGrad* dg = d(n, k);
            //cout << "(" << vg->getVal() << "," << dg->dist << ") ";
            dsum += dg->dist;
        }
        //cout << endl;
        int s = size(n);
    }
    
    // do final merge
    for (int i = 0; i < bdag.size(); i++) {
        ValueGrad* val = v(bdag[i], 0);
        double mergedVal = merge(bdag[i], val->getGrad());
        val->update(mergedVal);
        val->set = true;
    }

}

double KLocalityAutoDiff::getErrorOnConstraint(int nodeid, gsl_vector* grad) { // Negative means errors TODO: name is confusing
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

double KLocalityAutoDiff::getSqrtError(bool_node* node, gsl_vector* grad) {
    UFUN_node* un = (UFUN_node*) node;
    bool_node* x = un->multi_mother[0];
    ValueGrad* val = v(x, 0);
    Assert(val->set, "Sqrt node is not set");
    gsl_vector_memcpy(grad, val->getGrad());
    return val->getVal();
}

double KLocalityAutoDiff::getAssertError(bool_node* node, gsl_vector* grad) {
    ValueGrad* val = v(node->mother, 0);
    Assert(val->set, "Assert node is not set");
    gsl_vector_memcpy(grad, val->getGrad());
    return val->getVal();
}

double KLocalityAutoDiff::getBoolCtrlError(bool_node* node, gsl_vector* grad) {
    Assert(false, "not yet supported");
}

double KLocalityAutoDiff::getBoolExprError(bool_node* node, gsl_vector* grad) {
    Assert (inputValues->hasValue(node->id), "Boolean expression not yet set");
    int bv = inputValues->getValue(node->id);
    
    ValueGrad* val = v(node, 0);
    Assert(val->set, "Boolean expression distance is not set");
    gsl_vector_memcpy(grad, val->getGrad());
    if (bv == 1) {
        return val->getVal();
    } else {
        gsl_vector_scale(val->getGrad(), -1.0);
        return -val->getVal();
    }
}




double KLocalityAutoDiff::getErrorOnConstraint(int nodeid) {
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

double KLocalityAutoDiff::getSqrtError(bool_node* node) {
    UFUN_node* un = (UFUN_node*) node;
    bool_node* x = un->multi_mother[0];
    ValueGrad* val = v(x, 0);
    Assert(val->set, "Sqrt node is not set");
    return val->getVal();
}

double KLocalityAutoDiff::getAssertError(bool_node* node) {
    ValueGrad* val = v(node->mother, 0);
    Assert(val->set, "Assert node is not set");
    return val->getVal();
}

double KLocalityAutoDiff::getBoolCtrlError(bool_node* node) {
    Assert(false, "Nyi");
}

double KLocalityAutoDiff::getBoolExprError(bool_node* node) {
    Assert (inputValues->hasValue(node->id), "Boolean expression not yet set");
    int bv = inputValues->getValue(node->id);
    
    ValueGrad* val = v(node, 0);
    Assert(val->set, "Boolean expression distance is not set");
    if (bv == 1) {
        return val->getVal();
    } else {
        return -val->getVal();
    }
}



