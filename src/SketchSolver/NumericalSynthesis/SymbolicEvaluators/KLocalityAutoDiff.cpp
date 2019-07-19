#include "KLocalityAutoDiff.h"
#include <algorithm>
#include <limits>
#include <math.h>


KLocalityAutoDiff::KLocalityAutoDiff(BooleanDAG& bdag_p, map<string, int>& floatCtrls_p): bdag(bdag_p), floatCtrls(floatCtrls_p) {
	values.resize(bdag.size());
	distances.resize(bdag.size());
    paths.resize(bdag.size());
    mergeNodes.resize(bdag.size());
    sizes.resize(bdag.size(), 0);
    for (int i = 0; i < bdag.size(); i++) {
        values[i].resize(MAX_REGIONS, NULL);
        distances[i].resize(MAX_REGIONS, NULL);
        paths[i].resize(MAX_REGIONS, NULL);
        mergeNodes[i].resize(MAX_REGIONS, NULL);
    }
    nctrls = floatCtrls.size();
    if (nctrls == 0) nctrls = 1;
	ctrls = gsl_vector_alloc(nctrls);

    getMergeNodes();
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

    for (int i = 0; i < mergeNodes.size(); i++) {
        for (int j = 0; j < mergeNodes.size(); j++) {
            if (mergeNodes[i][j] != NULL) {
                delete mergeNodes[i][j];
            }
        }
    }
}

double getSortCond(Path* p) {
    return -p->length();
}

void KLocalityAutoDiff::mergePaths(vector<tuple<double, vector<tuple<int, int, int>>, Path*>>& mergedPaths) {
    
    while (mergedPaths.size() >= MAX_REGIONS) { 
        sort(mergedPaths.begin(), mergedPaths.end());
        int idx1 = 0;
        int minConjugate = 10000;
        int idx2 = -1;
        for (int i = 0; i < mergedPaths.size(); i++) {
            idx1 = i;
            auto& t1 = mergedPaths[idx1];
            Path* p1 = get<2>(t1);

            for (int j = idx1 + 1; j < mergedPaths.size(); j++) {
                auto& t2 = mergedPaths[j];
                Path* p2 = get<2>(t2);
                int conjIdx = Path::isConjugate(p1, p2);
                if (conjIdx >= 0 && conjIdx < minConjugate) {
                    minConjugate = conjIdx;
                    idx2 = j;
                }
                if (minConjugate == 0) {
                    break;
                }
            }
            if (idx2 != -1) {
                break;
            }
        }
        

        if (idx2 == -1) {
            //for (int i = 0; i < mergedPaths.size(); i++) {
            //    cout << get<2>(mergedPaths[i])->toString() << endl;
            //}
            Assert(false, "Conjugate not found");
        }
        auto& t1 = mergedPaths[idx1];
        Path* p1 = get<2>(t1);

        auto& t2 = mergedPaths[idx2];
        Path* p2 = get<2>(t2);
        Path* p = Path::pUnion(p1, p2);
        //pathStrings.insert(p->toString());
        //cout << "Merging " << p1->toString() << " + " << p2->toString() << " -> " << p->toString() << endl;
        
        vector<tuple<int, int, int>> l;
        l.insert(l.end(), get<1>(t1).begin(), get<1>(t1).end());
        l.insert(l.end(), get<1>(t2).begin(), get<1>(t2).end());
        mergedPaths.push_back(make_tuple(getSortCond(p), l, p));
        Assert(idx2 > idx1, "idx2 should be greater than idx1");
        mergedPaths.erase(mergedPaths.begin() + idx2);
        mergedPaths.erase(mergedPaths.begin() + idx1);
    }
}


void KLocalityAutoDiff::getMergeNodesBinop(bool_node* n) {
    int msize = size(n->mother);
    int fsize = size(n->father);
    vector<tuple<double, vector<tuple<int, int, int>>, Path*>> validPaths;
    for (int i = 1; i <= msize; i++) {
        Path* mpath = path(n->mother, i);
        for (int j = 1; j <= fsize; j++) {
            Path* fpath = path(n->father, j);
            if (Path::isCompatible(mpath, fpath)) {
                Path* newpath = Path::pIntersect(mpath, fpath);
                //pathStrings.insert(newpath->toString());
                vector<tuple<int, int, int>> l;
                l.push_back(make_tuple(i, j, 0));
                validPaths.push_back(make_tuple(getSortCond(newpath), l, newpath));
            }
        }
    }

    mergePaths(validPaths);
    for (int i = 1; i<= validPaths.size(); i++) {
        Path* p = path(n, i);
        p->empty();
        Path::combinePath(get<2>(validPaths[i-1]), p);
        MergeNodesList* l = new MergeNodesList();
        vector<tuple<int, int, int>>& pairs = get<1>(validPaths[i-1]);
        for (auto it = pairs.begin(); it != pairs.end(); it++) {
                l->add(n->mother->id, get<0>(*it), n->father->id, get<1>(*it));
        }
        mergeNodes[n->id][i] = l;
    } 
    setsize(n, validPaths.size());

}

void KLocalityAutoDiff::getMergeNodesUnop(bool_node* n, bool_node* m) {
    for (int i = 1; i <= size(m); i++) {
        Path* mpath = path(m, i);
        Path* p = path(n, i);
        p->empty();
        Path::combinePath(mpath, p);
    }
    setsize(n, size(m));
}

void KLocalityAutoDiff::getMergeNodesArracc(bool_node* node) {
    ARRACC_node* n = (ARRACC_node*) node;
    bool_node* cnode = n->mother;
    bool_node* mnode = n->multi_mother[0];
    bool_node* fnode = n->multi_mother[1];
    int csize = size(cnode);
    int msize = size(mnode);
    int fsize = size(fnode);
    vector<tuple<double, vector<tuple<int, int, int>>, Path*>> validPaths;
    for (int i = 1; i <= csize; i++) {
        Path* cpath = path(cnode, i);
        for (int j = 1; j <= msize; j++) {
            Path* mpath = path(mnode, j);
            if (Path::isCompatible(cpath, mpath) && mpath->getVal(cnode->id) != 1) {
                Path* newpath = Path::pIntersect(cpath, mpath);
                //pathStrings.insert(newpath->toString());
                newpath->addCond(cnode->id, 0);
                //pathStrings.insert(newpath->toString());
                vector<tuple<int, int, int>> l;
                l.push_back(make_tuple(i, j, 0));
                validPaths.push_back(make_tuple(getSortCond(newpath), l, newpath));
            }
        }

        for (int j = 1; j <= fsize; j++) {
            Path* fpath = path(fnode, j);
            if (Path::isCompatible(cpath, fpath) && fpath->getVal(cnode->id) != 0) {
                Path* newpath = Path::pIntersect(cpath, fpath);
                //pathStrings.insert(newpath->toString());
                newpath->addCond(cnode->id, 1);
                //pathStrings.insert(newpath->toString());
                vector<tuple<int, int, int>> l;
                l.push_back(make_tuple(i, j, 1));
                validPaths.push_back(make_tuple(getSortCond(newpath), l, newpath));
            }
        }
    }

    mergePaths(validPaths);
    for (int i = 1; i<= validPaths.size(); i++) {
        Path* p = path(n, i);
        p->empty();
        Path::combinePath(get<2>(validPaths[i-1]), p);
        MergeNodesList* l = new MergeNodesList();
        vector<tuple<int, int, int>>& pairs = get<1>(validPaths[i-1]);
        for (auto it = pairs.begin(); it != pairs.end(); it++) {
            l->add(n->mother->id, get<0>(*it), n->multi_mother[get<2>(*it)]->id, get<1>(*it));
        }
        mergeNodes[n->id][i] = l;
    } 
    setsize(n, validPaths.size());

}

void KLocalityAutoDiff::getMergeNodes() {
    for (BooleanDAG::iterator node_it = bdag.begin(); node_it != bdag.end(); ++node_it) {
        bool_node* n = *node_it;
        if (n->type == bool_node::CTRL || n->type == bool_node::CONST) {
            Path* p = path(n, 1);
            p->empty();
            //pathStrings.insert(p->toString());
            setsize(n, 1);
        } else if (n->type ==  bool_node::PLUS || n->type == bool_node::TIMES || n->type == bool_node::DIV || n->type == bool_node::LT || n->type == bool_node::AND || n->type == bool_node::OR) {
            getMergeNodesBinop(n);
        } else if (n->type == bool_node::NEG || n->type == bool_node::NOT || n->type == bool_node::ASSERT ||  n->type == bool_node::TUPLE_R) {
            getMergeNodesUnop(n, n->mother);
        } else if (n->type == bool_node::UFUN) {
            getMergeNodesUnop(n, ((UFUN_node*)n)->multi_mother[0]);
        } else if (n->type == bool_node::ARRACC) {
            getMergeNodesArracc(n);
        }
        //for (int j = 1; j <= size(n); j++) {
        //    pathStrings.insert(paths[n->id][j]->toString());
        //}

        /*cout << n->lprint() << endl;
        for (int j = 1; j <= size(n); j++) {
            cout << paths[n->id][j]->toString() << endl;
            if (mergeNodes[n->id][j] != NULL) {
                cout << mergeNodes[n->id][j]->print() << endl;
            }
        }*/
    }

    //for (auto it = pathStrings.begin(); it != pathStrings.end(); it++) {
    //    cout << *it << endl;
    //}
    //cout << pathStrings.size() << endl;
    //exit(0);
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

void KLocalityAutoDiff::combineDistance(bool_node& node, int nodeid1, int idx1, int nodeid2, int idx2, double& dist, gsl_vector* dg_grad) {
    Assert(dg_grad != GradUtil::tmp2, "Reusing tmp vectors");
    Assert(dg_grad != GradUtil::tmp3, "Reusing tmp vectors");
    DistanceGrad* mdist = d(bdag[nodeid1], idx1);
    DistanceGrad* fdist = d(bdag[nodeid2], idx2);

    Path* mpath = path(bdag[nodeid1], idx1);
    Path* fpath = path(bdag[nodeid2], idx2);

    if (Path::isSubset(mpath, fpath)) {
        dist = DistanceGrad::dg_copy(fdist, dg_grad);
    } else if (Path::isSubset(fpath, mpath)) {
        dist = DistanceGrad::dg_copy(mdist, dg_grad);
    } else if (Path::isDisjoint(mpath, fpath)) {
        dist = DistanceGrad::dg_times(mdist, fdist, dg_grad);
    } else {
        //cout << "KKK: " << mpath->toString() << " + " << fpath->toString() << endl;
        //cout << "KKK: " << mdist->dist << " " << fdist->dist << endl;
        dist = DistanceGrad::dg_times(mdist, fdist, dg_grad);
        //Assert(false, "Not yet supported");
    }

    if (node.type == bool_node::ARRACC) {
        ARRACC_node* an = (ARRACC_node*)(&node);

        ValueGrad* cval;
        int bv;
        if (nodeid1 == an->mother->id) {
            cval = v(bdag[nodeid1], idx1);
            if (nodeid2 == an->multi_mother[0]->id) {
                bv = 0;
            } else {
                bv = 1;
            }
        } else {
            cval = v(bdag[nodeid2], idx2);
            if (nodeid1 == an->multi_mother[0]->id) {
                bv = 0;
            } else {
                bv = 1;
            }
        }

        double s;
        if (bv == 1) {
            s = GradUtil::sigmoid(cval->getVal(), cval->getGrad(), GradUtil::tmp2);
        } else {
            GradUtil::compute_neg_grad(cval->getGrad(), GradUtil::tmp3);
            s = GradUtil::sigmoid(-cval->getVal(), GradUtil::tmp3, GradUtil::tmp2);
        }

        GradUtil::compute_mult_grad(dist, s, dg_grad, GradUtil::tmp2, GradUtil::tmp3);
        gsl_vector_memcpy(dg_grad, GradUtil::tmp3);
        dist = dist*s;
    }
}

void KLocalityAutoDiff::doPair(bool_node& node, int nodeid1, int idx1, int nodeid2, int idx2, double& val, gsl_vector* val_grad) {

    ValueGrad* mval = v(bdag[nodeid1], idx1);
    ValueGrad* fval = v(bdag[nodeid2], idx2);
    
    if (node.type == bool_node::PLUS) {
        val = ValueGrad::vg_plus(mval, fval, val_grad);
    } else if (node.type == bool_node::TIMES) {
        val = ValueGrad::vg_times(mval, fval, val_grad);
    } else if (node.type == bool_node::DIV) {
        val = ValueGrad::vg_div(mval, fval, val_grad);
    } else if (node.type == bool_node::LT) {
        val = ValueGrad::vg_lt(mval, fval, val_grad);
    } else if (node.type == bool_node::AND) {
        val = ValueGrad::vg_and(mval, fval, val_grad);
    } else if (node.type == bool_node::OR) {
        val = ValueGrad::vg_or(mval, fval, val_grad);
    } else if (node.type == bool_node::ARRACC) {
        ARRACC_node* an = (ARRACC_node*)(&node);
        if (nodeid1 == an->mother->id) {
            val = ValueGrad::vg_copy(fval, val_grad);
        } else if (nodeid2 == an->mother->id) {
            val = ValueGrad::vg_copy(mval, val_grad);
        } else {
            Assert(false, "arracc: cond should be in the pair");
        }
    } else {
        Assert(false, "unknown binop");
    }
}


void KLocalityAutoDiff::pairNodes(bool_node& node) {
    for (int i = 1; i <= size(node); i++) {
        ValueGrad* val = v(node, i);
        DistanceGrad* dg = d(node, i);
        val->set = true;
        dg->set = true;

        MergeNodesList* l = mergeNodes[node.id][i];
        double totalVal = 0.0;
        double totalDist = 0.0;
        GradUtil::default_grad(val->getGrad());
        GradUtil::default_grad(dg->grad);

        for (int j = 0; j < l->size(); j++) {
            auto& t = l->getTuple(j);

            double v = 0.0;
            double dist = 0.0;
            GradUtil::default_grad(GradUtil::tmp);
            GradUtil::default_grad(GradUtil::tmp1);

            doPair(node, get<0>(t), get<1>(t), get<2>(t), get<3>(t), v,  GradUtil::tmp);
            combineDistance(node, get<0>(t), get<1>(t), get<2>(t), get<3>(t), dist, GradUtil::tmp1);

            totalVal += v*dist;
            totalDist += dist;

            GradUtil::compute_mult_grad(v, dist, GradUtil::tmp, GradUtil::tmp1, GradUtil::tmp2);
            gsl_blas_daxpy(1.0, GradUtil::tmp2, val->getGrad());
            gsl_blas_daxpy(1.0, GradUtil::tmp1, dg->grad);
        }
        if (totalDist > 1e-4) {
            GradUtil::compute_div_grad(totalVal, totalDist, val->getGrad(), dg->grad, GradUtil::tmp);
            gsl_vector_memcpy(val->getGrad(), GradUtil::tmp);
            val->update(totalVal/totalDist);
        } else {
            val->update(totalVal);
        }
        dg->dist = totalDist;
    }
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
    pairNodes(node);
}

void KLocalityAutoDiff::visit( TIMES_node& node ) {
	//cout << "Visiting TIMES node" << endl;
	Assert(isFloat(node), "NYI: times with ints");
	pairNodes(node);
}

void KLocalityAutoDiff::visit(ARRACC_node& node )  {
	Assert(node.multi_mother.size() == 2, "NYI: KLocalityAutoDiff ARRACC of size > 2");
	Assert(node.getOtype() == OutType::FLOAT, "NYI: KLocalityAutoDiff ARRACC for bool nodes")
	/*int cval = getInputValue(node.mother);
    if (cval == 1) {
        copyNodes(node, node.multi_mother[1]);
        return;
    } else if (cval == 0) {
        copyNodes(node, node.multi_mother[0]);
        return;
    }*/
    pairNodes(node); 
}

void KLocalityAutoDiff::visit( DIV_node& node ) {
	//cout << "Visiting DIV node" << endl;
	Assert(isFloat(node), "NYI: div with ints");
	pairNodes(node);
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
    pairNodes(node);
}

void KLocalityAutoDiff::visit( EQ_node& node ) {
	//cout << "Visiting EQ node" << endl;
	Assert(false, "NYI: KLocalityAutoDiff for eq");
}

void KLocalityAutoDiff::visit( AND_node& node ) {
    pairNodes(node);
}

void KLocalityAutoDiff::visit( OR_node& node ) {
    pairNodes(node);
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

void KLocalityAutoDiff::normalize(bool_node* n) {
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


void KLocalityAutoDiff::run(const gsl_vector* ctrls_p) {
    Assert(ctrls->size == ctrls_p->size, "KLocalityAutoDiff ctrl sizes are not matching");
    
    for (int i = 0; i < ctrls->size; i++) {
        gsl_vector_set(ctrls, i, gsl_vector_get(ctrls_p, i));
    }
    //cout << gsl_vector_get(ctrls, 0) << endl;

    for (int i = 0; i < bdag.size(); i++) {
        bool_node* n = bdag[i];
        n->accept(*this);
        normalize(n);
        //cout << n->lprint() << endl;
        if (size(n) == 0) {
            Assert(false, "Size is 0");
        }
        double dsum = 0;
        for (int k = 1; k <= size(n); k++) {
            ValueGrad* vg = v(n, k);
            DistanceGrad* dg = d(n, k);
            //cout << "Vals: (" << vg->getVal() << "," << dg->dist << ") ";
            //cout << "Grads: (" << gsl_vector_get(vg->getGrad(), 0) << "," << gsl_vector_get(dg->grad, 0) << ") ";
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
        //cout << bdag[i]->lprint() << endl;
        //cout << mergedVal << endl;
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



