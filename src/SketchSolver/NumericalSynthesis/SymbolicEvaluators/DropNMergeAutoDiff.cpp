#include "DropNMergeAutoDiff.h"
#include <algorithm>
#include <limits>
#include <math.h>


DropNMergeAutoDiff::DropNMergeAutoDiff(BooleanDAG& bdag_p, map<string, int>& floatCtrls_p): bdag(bdag_p), floatCtrls(floatCtrls_p) {
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
}

DropNMergeAutoDiff::~DropNMergeAutoDiff(void) {
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

void DropNMergeAutoDiff::visit( SRC_node& node ) {
	//cout << "Visiting SRC node" << endl;
	Assert(false, "NYI: DropNMergeAutoDiff for src");
}

void DropNMergeAutoDiff::visit( DST_node& node ) {
	//cout << "Visiting DST node" << endl;
	// Ignore
}

void DropNMergeAutoDiff::copyNodes(bool_node& node, bool_node* m) {
    int msize = size(m);
    int k = 1;
    for (int i = 1; i <= msize; i++) {
        Path* pm = path(m, i);
        
        Path* p = path(node, k);
        p->empty();
        Path::combinePath(pm, p); // copy path of pm to p

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

void DropNMergeAutoDiff::visit( ASSERT_node& node ) {
	//cout << "Visiting ASSERT node" << endl;
    copyNodes(node, node.mother);
    
}

void DropNMergeAutoDiff::visit( CTRL_node& node ) {
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
        Path* p = path(node, 1);
        p->empty();
        DistanceGrad* dg = d(node, 1);
        dg->dist = 1;
        GradUtil::default_grad(dg->grad);
        dg->set = true;
        setsize(node, 1);
	} else {
        Assert(false, "Bool ctrls not yet supported");
	}
}

vector<tuple<double, int, int>> DropNMergeAutoDiff::getPairs(bool_node* m, bool_node* f) {
    vector<tuple<double, int, int>> pairs;
    int msize = size(m);
    int fsize = size(f);
    for (int i = 1; i <= msize; i++) {
        for (int j = 1; j <= fsize; j++) {
            Path* pm = path(m, i);
            Path* pf = path(f, j);
            if (Path::isCompatible(pm, pf)) {
                DistanceGrad* mdist = d(m, i);
                DistanceGrad* fdist = d(f, j);
                double dist = DistanceGrad::dg_combine(mdist, fdist);
                if (true || dist > 0.01) {
                    pairs.push_back(make_tuple(dist, i, j));
                }
            }
        }
    }
    sort(pairs.begin(), pairs.end());
    reverse(pairs.begin(), pairs.end());
    return pairs;
}

vector<tuple<double, int, int, vector<int>>> DropNMergeAutoDiff::getArraccPairs(bool_node* cnode, bool_node* fnode, bool_node* tnode) {
    vector<tuple<double, int, int, vector<int>>> pairs;
    int csize = size(cnode);
    int fsize = size(fnode);
    int tsize = size(tnode);

    for (int i = 1; i <= fsize; i++) {
        Path* pf = path(fnode, i);
        DistanceGrad* fdist = d(fnode, i);
        double sum_dist = 0;
        vector<int> cids;
        for (int j = 1; j <= csize; j++) {
            Path* pc = path(cnode, j);
            if (Path::isCompatible(pc, pf) && pf->getVal(cnode->id) != 1) {
                ValueGrad* cval = v(cnode, j);
                DistanceGrad* cdist = d(cnode, j);
                double dist = DistanceGrad::dg_combine(cdist, fdist, cval->getVal(), 0);
                sum_dist += dist;
                cids.push_back(j);
            }
        }
        pairs.push_back(make_tuple(sum_dist, i, 0, cids));

    }

    for (int i = 1; i <= tsize; i++) {
        Path* pt = path(tnode, i);
        DistanceGrad* tdist = d(tnode, i);
        double sum_dist = 0;
        vector<int> cids;
        for (int j = 1; j <= csize; j++) {
            Path* pc = path(cnode, j);
            if (Path::isCompatible(pc, pt) && pt->getVal(cnode->id) != 0) {
                ValueGrad* cval = v(cnode, j);
                DistanceGrad* cdist = d(cnode, j);
                double dist = DistanceGrad::dg_combine(cdist, tdist, cval->getVal(), 1);
                sum_dist += dist;
                cids.push_back(j);
            }
        }
        pairs.push_back(make_tuple(sum_dist, i, 1, cids));

    }

    /*for (int i = 1; i <= csize; i++) {
        ValueGrad* cval = v(cnode, i);
        Path* pc = path(cnode, i);
        DistanceGrad* cdist = d(cnode, i);
        double cs = GradUtil::sigmoid(cval->getVal());
        if (true || cs < 0.99) {
            // consider false nodes
            for (int j = 1; j <= fsize; j++) {
                Path* pf = path(fnode, j);
                if (Path::isCompatible(pc, pf) && pf->getVal(cnode->id) != 1) {
                    DistanceGrad* fdist = d(fnode, j);
                    double dist = DistanceGrad::dg_combine(cdist, fdist, cval->getVal(), 0);
                    if (true || dist > 0.01) {
                        pairs.push_back(make_tuple(dist, i, j, 0));
                    }
                }
            }
        } 
        if (true || cs > 0.01) {
            // consider true nodes
            for (int j = 1; j <= tsize; j++) {
                Path* pt = path(tnode, j);
                if (Path::isCompatible(pc, pt) && pt->getVal(cnode->id) != 0) {
                    DistanceGrad* tdist = d(tnode, j);
                    double dist = DistanceGrad::dg_combine(cdist, tdist, cval->getVal(), 1);
                    if (true || dist > 0.01) {
                        pairs.push_back(make_tuple(dist, i, j, 1));
                    }
                }
            }
        }
    }*/
    sort(pairs.begin(), pairs.end());
    reverse(pairs.begin(), pairs.end());
    return pairs;
}

void DropNMergeAutoDiff::visit( PLUS_node& node ) {
	//cout << "Visiting PLUS node" << endl;
	Assert(isFloat(node), "NYI: plus with ints");
    const vector<tuple<double, int, int>>& pairs = getPairs(node.mother, node.father);

    int k = 1;
    for (auto it = pairs.begin(); it != pairs.end(); it++) {
        int midx = get<1>(*it);
        int fidx = get<2>(*it);
        Path* pm = path(node.mother, midx);
        Path* pf = path(node.father, fidx);
        Assert(k < MAX_REGIONS, "k exceeded max_regions");

        Path* p = path(node, k);
        p->empty();
        Path::combinePaths(pm, pf, p);
        ValueGrad* val = v(node, k);
        ValueGrad* mval = v(node.mother, midx);
        ValueGrad* fval = v(node.father, fidx);
        ValueGrad::vg_plus(mval, fval, val);

        DistanceGrad* dg = d(node, k);
        DistanceGrad* mdist = d(node.mother, midx);
        DistanceGrad* fdist = d(node.father, fidx);
        DistanceGrad::dg_combine(mdist, fdist, dg);
        k++;
        if (k >= MAX_REGIONS) break;
        
    }
	setsize(node, k-1);
}

void DropNMergeAutoDiff::visit( TIMES_node& node ) {
	//cout << "Visiting TIMES node" << endl;
	Assert(isFloat(node), "NYI: times with ints");
	const vector<tuple<double, int, int>>& pairs = getPairs(node.mother, node.father);

    int k = 1;
    for (auto it = pairs.begin(); it != pairs.end(); it++) {
        int midx = get<1>(*it);
        int fidx = get<2>(*it);
        Path* pm = path(node.mother, midx);
        Path* pf = path(node.father, fidx);

        Assert(k < MAX_REGIONS, "k exceeded max_regions");

        Path* p = path(node, k);
        p->empty();
        Path::combinePaths(pm, pf, p);

        ValueGrad* val = v(node, k);
        ValueGrad* mval = v(node.mother, midx);
        ValueGrad* fval = v(node.father, fidx);
        ValueGrad::vg_times(mval, fval, val);

        DistanceGrad* dg = d(node, k);
        DistanceGrad* mdist = d(node.mother, midx);
        DistanceGrad* fdist = d(node.father, fidx);
        DistanceGrad::dg_combine(mdist, fdist, dg);
        k++;
        if (k >= MAX_REGIONS) break;
        
    }
    setsize(node, k-1);
}

void DropNMergeAutoDiff::visit(ARRACC_node& node )  {
	Assert(node.multi_mother.size() == 2, "NYI: DropNMergeAutoDiff ARRACC of size > 2");
	Assert(node.getOtype() == OutType::FLOAT, "NYI: DropNMergeAutoDiff ARRACC for bool nodes")
	int cval = getInputValue(node.mother);
    if (cval == 1) {
        copyNodes(node, node.multi_mother[1]);
        return;
    } else if (cval == 0) {
        copyNodes(node, node.multi_mother[0]);
        return;
    } 

    const vector<tuple<double, int, int, vector<int>>>& pairs = getArraccPairs(node.mother, node.multi_mother[0], node.multi_mother[1]);

    int k = 1;
    for (auto it = pairs.begin(); it != pairs.end(); it++) {
        int vidx = get<1>(*it);
        int bv = get<2>(*it);
        const vector<int>& cids = get<3>(*it);
        Assert(bv == 0 || bv == 1, "bv must be 0 or 1");

        Assert(k < MAX_REGIONS, "k exceeded max_regions");
        Path* p = path(node, k);
        p->empty();

        Path* pv = path(node.multi_mother[bv], vidx);
        vector<Path*> pcs;
        for (int i = 0; i < cids.size(); i++) {
            pcs.push_back(path(node.mother, cids[i]));
        }
        Path::combinePaths(pcs, pv, p);
        p->addCond(node.mother->id, bv);

        ValueGrad* val = v(node, k);
        ValueGrad* bval = v(node.multi_mother[bv], vidx);
        ValueGrad::vg_copy(bval, val);

        DistanceGrad* dg = d(node, k);
        DistanceGrad* vdist = d(node.multi_mother[bv], vidx);
        vector<DistanceGrad*> cdists;
        vector<ValueGrad*> cvals;
        for (int i = 0; i < cids.size(); i++) {
            cdists.push_back(d(node.mother, cids[i]));
            cvals.push_back(v(node.mother, cids[i]));
        }
        DistanceGrad::dg_combine(cdists, vdist, cvals, bv, dg);
        k++;
        if (k >= MAX_REGIONS) break;
    }
    setsize(node, k-1);
}

void DropNMergeAutoDiff::visit( DIV_node& node ) {
	//cout << "Visiting DIV node" << endl;
	Assert(isFloat(node), "NYI: div with ints");
	const vector<tuple<double, int, int>>& pairs = getPairs(node.mother, node.father);

    int k = 1;
    for (auto it = pairs.begin(); it != pairs.end(); it++) {
        int midx = get<1>(*it);
        int fidx = get<2>(*it);
        Path* pm = path(node.mother, midx);
        Path* pf = path(node.father, fidx);
        
        Assert(k < MAX_REGIONS, "k exceeded max_regions");

        Path* p = path(node, k);
        p->empty();
        Path::combinePaths(pm, pf, p);

        ValueGrad* val = v(node, k);
        ValueGrad* mval = v(node.mother, midx);
        ValueGrad* fval = v(node.father, fidx);
        ValueGrad::vg_div(mval, fval, val);

        DistanceGrad* dg = d(node, k);
        DistanceGrad* mdist = d(node.mother, midx);
        DistanceGrad* fdist = d(node.father, fidx);
        DistanceGrad::dg_combine(mdist, fdist, dg);
        k++;
        if (k >= MAX_REGIONS) break;
        
    }
    setsize(node, k-1);
}

void DropNMergeAutoDiff::visit( MOD_node& node ) {
	cout << "Visiting MOD node" << endl;
	Assert(false, "NYI: DropNMergeAutoDiff mod");
}

void DropNMergeAutoDiff::visit( NEG_node& node ) {
	//cout << "Visiting NEG node" << endl;
	Assert(isFloat(node), "NYI: neg with ints");
    int msize = size(node.mother);
    int k = 1;
    for (int i = 1; i <= msize; i++) {
        Path* pm = path(node.mother, i);
        
        Path* p = path(node, k);
        p->empty();
        Path::combinePath(pm, p);

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

void DropNMergeAutoDiff::visit( CONST_node& node ) {
	if (node.getOtype() == OutType::FLOAT) {
		ValueGrad* val = v(node, 1);
		val->update(node.getFval());
		val->set = true;
		GradUtil::default_grad(val->getGrad());
        Path* p = path(node, 1);
        p->empty();
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
            Path* p = path(node, 1);
            p->empty();
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

void DropNMergeAutoDiff::visit( LT_node& node ) {
    Assert(isFloat(node.mother) && isFloat(node.father), "NYI: DropNMergeAutoDiff for lt with integer parents");

    const vector<tuple<double, int, int>>& pairs = getPairs(node.mother, node.father);

    int k = 1;
    for (auto it = pairs.begin(); it != pairs.end(); it++) {
        int midx = get<1>(*it);
        int fidx = get<2>(*it);
        Path* pm = path(node.mother, midx);
        Path* pf = path(node.father, fidx);
        
        Assert(k < MAX_REGIONS, "k exceeded max_regions");

        Path* p = path(node, k);
        p->empty();
        Path::combinePaths(pm, pf, p);

        ValueGrad* val = v(node, k);
        ValueGrad* mval = v(node.mother, midx);
        ValueGrad* fval = v(node.father, fidx);
        ValueGrad::vg_lt(mval, fval, val);

        DistanceGrad* dg = d(node, k);
        DistanceGrad* mdist = d(node.mother, midx);
        DistanceGrad* fdist = d(node.father, fidx);
        DistanceGrad::dg_combine(mdist, fdist, dg);
        k++;
        if (k >= MAX_REGIONS) break;
        
    }
    setsize(node, k-1);
}

void DropNMergeAutoDiff::visit( EQ_node& node ) {
	//cout << "Visiting EQ node" << endl;
	Assert(false, "NYI: DropNMergeAutoDiff for eq");
}

void DropNMergeAutoDiff::visit( AND_node& node ) {
	const vector<tuple<double, int, int>>& pairs = getPairs(node.mother, node.father);

    int k = 1;
    for (auto it = pairs.begin(); it != pairs.end(); it++) {
        int midx = get<1>(*it);
        int fidx = get<2>(*it);
        Path* pm = path(node.mother, midx);
        Path* pf = path(node.father, fidx);

        Assert(k < MAX_REGIONS, "k exceeded max_regions");

        
        Path* p = path(node, k);
        p->empty();
        Path::combinePaths(pm, pf, p);

        ValueGrad* val = v(node, k);
        ValueGrad* mval = v(node.mother, midx);
        ValueGrad* fval = v(node.father, fidx);
        ValueGrad::vg_and(mval, fval, val);

        DistanceGrad* dg = d(node, k);
        DistanceGrad* mdist = d(node.mother, midx);
        DistanceGrad* fdist = d(node.father, fidx);
        DistanceGrad::dg_combine(mdist, fdist, dg);
        k++;
        if (k >= MAX_REGIONS) break;
        
    }
    setsize(node, k-1);
}

void DropNMergeAutoDiff::visit( OR_node& node ) {
	const vector<tuple<double, int, int>>& pairs = getPairs(node.mother, node.father);

    int k = 1;
    for (auto it = pairs.begin(); it != pairs.end(); it++) {
        int midx = get<1>(*it);
        int fidx = get<2>(*it);
        Path* pm = path(node.mother, midx);
        Path* pf = path(node.father, fidx);

        Assert(k < MAX_REGIONS, "k exceeded max_regions");
        
        Path* p = path(node, k);
        p->empty();
        Path::combinePaths(pm, pf, p);

        ValueGrad* val = v(node, k);
        ValueGrad* mval = v(node.mother, midx);
        ValueGrad* fval = v(node.father, fidx);
        ValueGrad::vg_or(mval, fval, val);

        DistanceGrad* dg = d(node, k);
        DistanceGrad* mdist = d(node.mother, midx);
        DistanceGrad* fdist = d(node.father, fidx);
        DistanceGrad::dg_combine(mdist, fdist, dg);
        k++;
        if (k >= MAX_REGIONS) break;
        
    }
    setsize(node, k-1);
}

void DropNMergeAutoDiff::visit( NOT_node& node ) {
	int msize = size(node.mother);
    int k = 1;
    for (int i = 1; i <= msize; i++) {
        Path* pm = path(node.mother, i);
        
        Path* p = path(node, k);
        p->empty();
        Path::combinePath(pm, p);

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

void DropNMergeAutoDiff::visit( ARRASS_node& node ) {
	cout << "Visiting ARRASS node" << endl;
	Assert(false, "NYI: DropNMergeAutoDiff for arrass");
}

void DropNMergeAutoDiff::doUfun(UFUN_node& node, ValueGrad* mval, ValueGrad* val) {
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

void DropNMergeAutoDiff::visit( UFUN_node& node ) {
    int msize = size(node.multi_mother[0]);
    int k = 1;
    for (int i = 1; i <= msize; i++) {
        Path* pm = path(node.multi_mother[0], i);
        
        Assert(k < MAX_REGIONS, "k exceeded max_regions");

        Path* p = path(node, k);
        p->empty();
        Path::combinePath(pm, p);

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

void DropNMergeAutoDiff::visit( TUPLE_R_node& node) {
	if (node.mother->type == bool_node::UFUN) {
		Assert(((UFUN_node*)(node.mother))->multi_mother.size() == 1, "NYI"); // TODO: This assumes that the ufun has a single output
		copyNodes(node, node.mother);
	} else {
		Assert(false, "NYI");
	}
}


void DropNMergeAutoDiff::setInputs(Interface* inputValues_p) {
    inputValues = inputValues_p;
}

void DropNMergeAutoDiff::normalize(bool_node* n) {
    GradUtil::default_grad(GradUtil::tmp);
    double sum = 0.0;
    for (int i = 1; i <= size(n); i++) {
        DistanceGrad* dg = d(n, i);
        //cout << "(" << vg->getVal() << "," << dg->dist << ") ";
        sum += dg->dist;
        gsl_blas_daxpy(1.0, dg->grad, GradUtil::tmp);
    }

    for (int i = 1; i <= size(n); i++) {
        DistanceGrad* dg = d(n, i);
        GradUtil::compute_div_grad(dg->dist, sum, dg->grad, GradUtil::tmp, GradUtil::tmp1);
        gsl_vector_memcpy(dg->grad, GradUtil::tmp1);
        dg->dist = dg->dist/sum;
    }
}

double DropNMergeAutoDiff::merge(bool_node* n, gsl_vector* grad) {
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


pair<double, double> DropNMergeAutoDiff::merge(bool_node* n, int mergeIdx, gsl_vector* grad, gsl_vector* dist_grad) {
    double res = 0;
    double dist = 0;
    GradUtil::default_grad(grad);
    GradUtil::default_grad(dist_grad);
    Assert(grad != GradUtil::tmp, "Grad vector is being rewritten");
    Assert(dist_grad != GradUtil::tmp, "Grad vector is being rewritten");
    
    //cout << n->lprint() << " ";
    for (int i = mergeIdx; i <= size(n); i++) {
        ValueGrad* vg = v(n, i);
        DistanceGrad* dg = d(n, i);
        //cout << "(" << vg->getVal() << "," << dg->dist << ") ";
        res += vg->getVal() * dg->dist;
        dist += dg->dist;
        GradUtil::compute_mult_grad(vg->getVal(), dg->dist, vg->getGrad(), dg->grad, GradUtil::tmp);
        gsl_blas_daxpy(1.0, GradUtil::tmp, grad);
        gsl_blas_daxpy(1.0, dg->grad, dist_grad);
    }
    //cout << "total: " << res << endl;
    return make_pair(res, dist);
}


double DropNMergeAutoDiff::mean(bool_node* n) {
    double res = 0;
    for (int i = 1; i <= size(n); i++) {
        ValueGrad* vg = v(n, i);
        DistanceGrad* dg = d(n, i);
        res += vg->getVal() * dg->dist;
    }
    return res;
}

double DropNMergeAutoDiff::stdDev(bool_node* n, double mean) {
    if (size(n) == 1) {
        return 0.0;
    }
    double std_dev = 0.0;
    for (int i = 1; i <= size(n); i++) {
        ValueGrad* vg = v(n, i);
        DistanceGrad* dg = d(n, i);
        std_dev += abs(vg->getVal() - mean) * dg->dist;
    }
    return std_dev;
}

bool DropNMergeAutoDiff::doMergeAll(double mean, double std_dev, int size) {
    if (abs(std_dev)/abs(mean)  < 0.01) {
        return true;
    }
    
    return false;
}


void DropNMergeAutoDiff::run(const gsl_vector* ctrls_p) {
    Assert(ctrls->size == ctrls_p->size, "DropNMergeAutoDiff ctrl sizes are not matching");
    
    for (int i = 0; i < ctrls->size; i++) {
        gsl_vector_set(ctrls, i, gsl_vector_get(ctrls_p, i));
    }
    cout << gsl_vector_get(ctrls, 0) << endl;

    for (int i = 0; i < bdag.size(); i++) {
        bool_node* n = bdag[i];
        n->accept(*this);
        normalize(n);
        cout << n->lprint() << endl;
        if (size(n) == 0) {
            Assert(false, "Size is 0");
        }
        double dsum = 0;
        for (int k = 1; k <= size(n); k++) {
            ValueGrad* vg = v(n, k);
            DistanceGrad* dg = d(n, k);
            cout << "(" << vg->getVal() << "," << dg->dist << ") ";
            dsum += dg->dist;
        }
        cout << endl;
        int s = size(n);
        if (s > 1) {
            double mean_ = mean(n);
            double std_dev = stdDev(n, mean_ );
            cout << mean_ << " " << std_dev << " " << dsum << " " << s;
            if (doMergeAll(mean_, std_dev, s)) {
                cout << " Merge all";
                double mergedVal = merge(n, GradUtil::tmp2);
                ValueGrad* val = v(n, 1);
                val->update(mergedVal);
                gsl_vector_memcpy(val->getGrad(), GradUtil::tmp2);
                val->set = true;

                Path* p = path(n, 1);
                p->empty();

                DistanceGrad* dg = d(n, 1);
                dg->dist = 1;
                GradUtil::default_grad(dg->grad);
                dg->set = true;
                setsize(*n, 1);
            } else if (s >= MAX_REGIONS - 1 && d(n, 1)->dist > 0.3) {
                cout << " Merge half";
                int mergeIdx = (MAX_REGIONS - 1)/2;
                pair<double, double> mergedVal = merge(n, mergeIdx, GradUtil::tmp2, GradUtil::tmp3);
                ValueGrad* val = v(n, mergeIdx);
                val->update(mergedVal.first);
                gsl_vector_memcpy(val->getGrad(), GradUtil::tmp2);
                val->set = true;

                Path* p = path(n, mergeIdx);
                p->empty(); // TODO: not sure what the path should be

                DistanceGrad* dg = d(n, mergeIdx);
                dg->dist = mergedVal.second;
                gsl_vector_memcpy(dg->grad, GradUtil::tmp3);
                dg->set = true;
                setsize(*n, mergeIdx);
            }
            cout << endl;
        } 
    }
    
    // do final merge
    for (int i = 0; i < bdag.size(); i++) {
        ValueGrad* val = v(bdag[i], 0);
        double mergedVal = merge(bdag[i], val->getGrad());
        val->update(mergedVal);
        val->set = true;
    }

}

double DropNMergeAutoDiff::getErrorOnConstraint(int nodeid, gsl_vector* grad) { // Negative means errors TODO: name is confusing
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

double DropNMergeAutoDiff::getSqrtError(bool_node* node, gsl_vector* grad) {
    UFUN_node* un = (UFUN_node*) node;
    bool_node* x = un->multi_mother[0];
    ValueGrad* val = v(x, 0);
    Assert(val->set, "Sqrt node is not set");
    gsl_vector_memcpy(grad, val->getGrad());
    return val->getVal();
}

double DropNMergeAutoDiff::getAssertError(bool_node* node, gsl_vector* grad) {
    ValueGrad* val = v(node->mother, 0);
    Assert(val->set, "Assert node is not set");
    gsl_vector_memcpy(grad, val->getGrad());
    return val->getVal();
}

double DropNMergeAutoDiff::getBoolCtrlError(bool_node* node, gsl_vector* grad) {
    Assert(false, "not yet supported");
}

double DropNMergeAutoDiff::getBoolExprError(bool_node* node, gsl_vector* grad) {
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




double DropNMergeAutoDiff::getErrorOnConstraint(int nodeid) {
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

double DropNMergeAutoDiff::getSqrtError(bool_node* node) {
    UFUN_node* un = (UFUN_node*) node;
    bool_node* x = un->multi_mother[0];
    ValueGrad* val = v(x, 0);
    Assert(val->set, "Sqrt node is not set");
    return val->getVal();
}

double DropNMergeAutoDiff::getAssertError(bool_node* node) {
    ValueGrad* val = v(node->mother, 0);
    Assert(val->set, "Assert node is not set");
    return val->getVal();
}

double DropNMergeAutoDiff::getBoolCtrlError(bool_node* node) {
    Assert(false, "Nyi");
}

double DropNMergeAutoDiff::getBoolExprError(bool_node* node) {
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



