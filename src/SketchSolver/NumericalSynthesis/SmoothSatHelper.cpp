#include "SmoothSatHelper.h"


int SnoptEvaluator::counter;


SmoothSatHelper::SmoothSatHelper(FloatManager& _fm, BooleanDAG* _dag, map<int, int>& _imap): NumericalSolverHelper(_fm, _dag, _imap) {
	//dag->lprint(cout);
    int numConstraints = 0;
	// Collect the list of boolean nodes that contribute to the error
	// In this case, only the assertions matter
	for (int i = 0; i < dag->size(); i++) {
		bool_node* n = (*dag)[i];
		if (n->type == bool_node::ASSERT) {
			if (!((ASSERT_node*)n)->isHard()) {
				boolNodes.insert(i);
                numConstraints++;
			}
        } else {
            if (n->getOtype() == OutType::BOOL) {
                boolNodes.insert(i);
            }
        }
        if (Util::isSqrt(n)) {
            numConstraints++;
        }
	}
    
    numConstraints = numConstraints + 100; // buffer for additional input variables set
	
	// generate ctrls mapping
	set<int> ctrlNodeIds;
	vector<bool_node*>& ctrls = dag->getNodesByType(bool_node::CTRL);
	int ctr = 0;
	for (int i = 0; i < ctrls.size(); i++) {
		if (ctrls[i]->getOtype() == OutType::FLOAT) {
			ctrlMap[ctrls[i]->get_name()] = ctr++;
			ctrlNodeIds.insert(ctrls[i]->id);
		}
	}
    
    for (int i = 0; i < ctrls.size(); i++) {
        if (ctrls[i]->getOtype() == OutType::BOOL) {
            boolCtrlMap[ctrls[i]->get_name()] = ctr++;
        }
    }

    
	ncontrols = ctr;
	if (ncontrols == 0) {
		ncontrols = 1;
	}
	
	cout << "NControls: " << ncontrols << endl;
    
    GradUtil::tmp = gsl_vector_alloc(ncontrols);
    GradUtil::tmp1 = gsl_vector_alloc(ncontrols);
    GradUtil::tmp2 = gsl_vector_alloc(ncontrols);
    GradUtil::tmp3 = gsl_vector_alloc(ncontrols);
    GradUtil::tmpT = gsl_vector_alloc(ncontrols);
	
	state = gsl_vector_alloc(ncontrols);
	eval = new BoolAutoDiff(*dag, fm, ctrlMap, boolCtrlMap);
    seval = new SimpleEvaluator(*dag, fm, ctrlMap, boolCtrlMap);
	if (PARAMS->useSnopt) {
		opt = new SnoptWrapper(eval, dag, imap, ctrlMap, boolNodes, ncontrols, numConstraints);
	} else {
#ifndef _NOGSL
		opt = new GradientDescentWrapper(eval, dag, imap, ctrlMap, boolNodes, ncontrols);
#endif
	}
	cg = new SimpleConflictGenerator(imap, boolNodes);
    previousSAT = false;
    fullSAT = false;
    numConflictsAfterSAT = 0;
    clearLearnts = false;
    
    for (int i = 0; i < imap.size(); i++) {
        int nodeid = imap[i];
        if (nodeid < 0) continue;
        bool_node* node = (*dag)[nodeid];
        if (Util::hasArraccChild(node)) {
            nodesToSuggest.push_back(i);
        }
    }
}

SmoothSatHelper::~SmoothSatHelper(void) {
	delete GradUtil::tmp;
	delete GradUtil::tmp1;
	delete GradUtil::tmp2;
	delete GradUtil::tmp3;
	delete GradUtil::tmpT;
}

void SmoothSatHelper::setInputs(vector<vector<int>>& allInputs_, vector<int>& instanceIds_) {
	allInputs = allInputs_;
	instanceIds = instanceIds_;
    if (PARAMS->verbosity > 7 && !fullSAT) {
    cout << "I:";
    for (int i = 0; i < allInputs[0].size(); i++) {
        if (allInputs[0][i] == 0 || allInputs[0][i] == 1) {
            cout << imap[i] << "," << allInputs[0][i] << ";";
        }
    }
    cout << endl;
    }
}

void SmoothSatHelper::setState(gsl_vector* s) {
    gsl_vector_memcpy(state, s);
}

bool SmoothSatHelper::checkInputs(int rowid, int colid) {
    if (fullSAT) return false;
    if (imap[colid] == -1 || Util::hasArraccChild((*dag)[imap[colid]])) {
        return true;
    } else {
        return false;
    }
	return true;
}

bool SmoothSatHelper::validObjective() {
	// if all bool holes are set, we will be solving the full problem
	for (int i = 0; i < allInputs.size(); i++) {
		for (int j = 0; j < allInputs[i].size(); j++) {
			int nid = imap[j];
			if (nid < 0) continue;
			bool_node* n = (*dag)[nid];
			if (n->getOtype() == OutType::BOOL && n->type == bool_node::CTRL && allInputs[i][j] != 0 && allInputs[i][j] != 1) {
				return false;
			}
		}
	}
	return true;
}

bool SmoothSatHelper::checkCurrentSol() {
    const map<int, int>& nodeToInputMap = Util::getNodeToValMap(imap, allInputs[0]);
    GradUtil::BETA = -50;
    GradUtil::ALPHA = 50;
    return eval->checkAll(state, nodeToInputMap);
}

bool SmoothSatHelper::checkFullSAT() {
    gsl_vector* newState = GradUtil::tmp;
    gsl_vector_memcpy(newState, state);
    
    for (int i = 0; i < allInputs[0].size(); i++) {
        if (imap[i] < 0) continue;
        bool_node* n = (*dag)[imap[i]];
        if (n->type == bool_node::CTRL && n->getOtype() == OutType::BOOL) {
            if (allInputs[0][i] == 0 || allInputs[0][i] == 1) {
                gsl_vector_set(newState, boolCtrlMap[n->get_name()], allInputs[0][i]);
            }
        }
    }
    double error = 0.0;
    if (seval->check(newState, error)) {
        return true;
    } else {
        return false;
    }
}

bool SmoothSatHelper::checkSAT() {
    inputConflict = false;
    if (!previousSAT) {
        opt->randomizeCtrls(state, allInputs);
    }
    bool suppressPrint = PARAMS->verbosity > 7 ? false : true;
    
    bool sat = false;
    if (checkCurrentSol()) {
        sat = true;
    } else {
        if (!previousSAT) {
            bool satInputs = opt->optimizeForInputs(allInputs, state, suppressPrint);
            if (satInputs) {
                cout << "Inputs satisfiable" << endl;
                gsl_vector_memcpy(state, opt->getMinState());
            } else {
                cout << "Inputs not satisfiable" << endl;
                inputConflict = true;
                return false;
            }
        }
        
        sat = opt->optimize(allInputs, state, suppressPrint);
    }
    if (!previousSAT) {
        previousSAT = sat;
    }
    if (sat) {
        cout << "FOUND solution" << endl;
        clearLearnts = true;
        if (validObjective() && checkFullSAT()) {
            fullSAT = true;
            cout << "FULL SAT" << endl;
        }
        numConflictsAfterSAT = 0;
        gsl_vector_memcpy(state, opt->getMinState());
    } else {
        clearLearnts = false;
    }
    if (!sat && previousSAT) {
        numConflictsAfterSAT++;
        if (numConflictsAfterSAT > 5) {
            previousSAT = false;
        }
    }
    double objective = opt->getObjectiveVal();
    cout << "Objective found: " << objective << endl;
    
	return sat;
}

bool SmoothSatHelper::ignoreConflict() {
    if (previousSAT) return false;
    if (inputConflict) return false;
    int numSet = 0;
    for (int i = 0; i < allInputs[0].size(); i++) {
        if (imap[i] < 0) continue;
        //if (((*dag)[imap[i]])->hasFloatChild()) {
            if (allInputs[0][i] == 0 || allInputs[0][i] == 1) {
                numSet++;
            }
        //}
    }
    if (numSet < CONFLICT_CUTOFF) {
        return true;
    } else {
        return false;
    }
}

vector<tuple<int, int, int>> SmoothSatHelper::collectSatSuggestions() {
	vector<tuple<int, int, int>> suggestions;
    for (int i = 0; i < allInputs.size(); i++) {
        vector<tuple<double, int, int>> s = seval->run(state, imap);
        sort(s.begin(), s.end());
        bool first = true;
        //cout << ((*dag)[imap[get<1>(s[0])]])->lprint() << " " << get<2>(s[0]) << endl;
        reverse(s.begin(), s.end());
        for (int k = 0; k < s.size(); k++) {
            int idx = get<1>(s[k]);
            if (allInputs[i][idx] != 0 && allInputs[i][idx] != 1) {
                suggestions.push_back(make_tuple(i, idx, get<2>(s[k])));
            }
        }
    }
    int lastIdx = suggestions.size() - 1;
    cout << ((*dag)[imap[get<1>(suggestions[lastIdx])]])->lprint() << " " << get<2>(suggestions[lastIdx]) << endl;
	return suggestions;
}

set<int> getRelevantNodes(bool_node* n, SymbolicEvaluator* eval) {
    set<int> ids;
    set<int> visitedIds;
    vector<bool_node*> toVisit;
    toVisit.push_back(n);
    
    while(toVisit.size() > 0) {
        bool_node* node = toVisit.back();
        toVisit.pop_back();
        if (visitedIds.find(node->id) == visitedIds.end()) {
            visitedIds.insert(node->id);
            ids.insert(node->id);
            if (node->type == bool_node::ARRACC) {
                ARRACC_node* an = (ARRACC_node*) node;
                toVisit.push_back(an->mother);
                double dist = eval->getVal(an->mother);
                if (dist > 2.0) { //TODO: hardcoded
                    toVisit.push_back(an->multi_mother[1]);
                } else if (dist < 2.0) {
                    toVisit.push_back(an->multi_mother[0]);
                } else {
                    toVisit.push_back(an->multi_mother[0]);
                    toVisit.push_back(an->multi_mother[1]);
                }
            } else if (node->type == bool_node::OR) {
                double dist1 = eval->getVal(node->mother);
                double dist2 = eval->getVal(node->father);
                if (abs(dist1 - dist2) > 2.0) {
                    if (dist1 > dist2) toVisit.push_back(node->mother);
                    else toVisit.push_back(node->father);
                } else {
                    toVisit.push_back(node->mother);
                    toVisit.push_back(node->father);
                }
            } else if (node->type == bool_node::AND) {
                double dist1 = eval->getVal(node->mother);
                double dist2 = eval->getVal(node->father);
                if (abs(dist1 - dist2) > 2.0) {
                    if (dist1 > dist2) toVisit.push_back(node->father);
                    else toVisit.push_back(node->mother);
                } else {
                    toVisit.push_back(node->mother);
                    toVisit.push_back(node->father);
                }
            } else {
                // just add all parents
                const vector<bool_node*>& parents = node->parents();
                for (int i = 0; i < parents.size(); i++) {
                    toVisit.push_back(parents[i]);
                }
            }
        }
    }
    return ids;
}


vector<tuple<int, int, int>> SmoothSatHelper::collectUnsatSuggestions() {
    cout << Util::print(state) << endl;
    vector<tuple<int, int, int>> suggestions;
    if (nodesToSuggest.size() == 0) return suggestions;
    int randIdx = nodesToSuggest[rand() % (nodesToSuggest.size())];
    int randVal = rand() % 2;
    cout << "Suggesting: " << ((*dag)[imap[randIdx]])->lprint() << " " << randVal << endl;
    suggestions.push_back(make_tuple(0, randIdx, randVal));
    return suggestions;
    /*for (int i = 0; i < allInputs.size(); i++) {
        const map<int, int>& givenNodesMap = Util::getNodeToValMap(imap, allInputs[i]);
        map<int, int> nodeToInputMap;
        vector<tuple<double, int, int>> s = seval->run(state, imap);
        for (int k = 0; k < s.size(); k++) {
            int idx = get<1>(s[k]);
            nodeToInputMap[imap[idx]] = get<2>(s[k]);
        }
        GradUtil::BETA = -1;
        GradUtil::ALPHA = 1;
        eval->run(state, nodeToInputMap);
        
        // Collect influential asserts
        set<bool_node*> influentialNodes;
        for (BooleanDAG::iterator node_it = dag->begin(); node_it != dag->end(); node_it++) {
            bool_node* node = *node_it;
            if (node->type == bool_node::ASSERT) {
                Assert(eval->hasDist(node->mother), "weoqypq");
                float dist = eval->getVal(node->mother);
                if (dist < 0.01 || ((ASSERT_node*)node)->isHard()) {
                    cout << dist << " " << node->lprint() << " " << node->mother->lprint() << endl;
                    influentialNodes.insert(node->mother);
                }
            } else if (node->getOtype() == OutType::BOOL) {
                Assert(eval->hasDist(node), "uetuoqw");
                float dist = eval->getVal(node);
                auto it = givenNodesMap.find(node->id);
                if (it != givenNodesMap.end()) {
                    if ((it->second == 1 && dist < 0.01) || (it->second == 0 && dist > -0.01)) {
                        cout << "Input failed" << endl;
                        cout << dist << " " << node->lprint() << endl;
                        influentialNodes.insert(node);
                    }
                }
            } // TODO: currently not handling sqrt nodes.
        }
        
        vector<pair<double, bool_node*>> sg;
        double maxDist = GradUtil::MINVAL;
        bool_node* maxDistNode = NULL;
        //bool_node* dn = (*dag)[21839];
        //cout << "sdfquhweq " << dn->lprint() << " " << eval->getVal(dn) << endl;
        //cout << Util::print(eval->getGrad(dn)) << endl;
        for (auto it = influentialNodes.begin(); it != influentialNodes.end(); it++)  {
            cout << (*it)->lprint() << endl;
            const set<int>& nodes = getRelevantNodes(*it, eval);
            map<int, pair<double, bool_node*>> ctrlToLowestDist;
            for (auto it1 = nodes.begin(); it1 != nodes.end(); it1++) {
                bool_node* n = (*dag)[*it1];
                if (n->getOtype() == OutType::BOOL && Util::hasArraccChild(n)) {
                    if (givenNodesMap.find(n->id) != givenNodesMap.end()) continue; // already set
                    double dist = abs(eval->getVal(n));
                    if (dist < 2.0) continue; // distance is within the smoothing range TODO: hardcoded value
                    gsl_vector* grad = eval->getGrad(n);
                    for (int j = 0; j < grad->size; j++) {
                        if (abs(gsl_vector_get(grad, j)) > 0.1) {
                            auto it2 = ctrlToLowestDist.find(j);
                            //if (j == 8) {
                            //    cout << n->lprint() << " " << eval->getVal(n) << " " << eval->getVal(n->mother) << endl;
                            //    cout << Util::print(eval->getGrad(n)) << endl;
                            //}
                            if (it2 == ctrlToLowestDist.end()) {
                                ctrlToLowestDist[j] = make_pair(dist, n);
                            } else {
                                if (dist < it2->second.first) {
                                    it2->second = make_pair(dist, n);
                                }
                            }
                        }
                    }
                }
            }
            for (auto it3 = ctrlToLowestDist.begin(); it3 != ctrlToLowestDist.end(); it3++) {
                cout << it3->first << " " << it3->second.first << " " << it3->second.second->lprint() << endl;
                sg.push_back(it3->second);
            }
        }
        sort(sg.begin(), sg.end());
        //reverse(sg.begin(), sg.end());
        cout << "Suggestions: ";
        for (int j = 0; j < sg.size(); j++) {
            bool_node* n = sg[j].second;
            double dist = sg[j].first;
            if (j == sg.size() - 1) {
                cout << n->lprint() << " " << eval->getVal(n) << endl;
            }
            for (int idx = 0; idx < imap.size(); idx++) { // TODO: this can be optimized
                if (imap[idx] == n->id) {
                    //cout << n->lprint() << " " << idx << endl;
                    suggestions.push_back(make_tuple(i, idx, !(eval->getVal(n) > 0.0)));
                }
            }
        }
    }
    return suggestions;*/
}

vector<pair<int, int>> SmoothSatHelper::getConflicts(int rowid, int colid) {
	return cg->getConflicts(state, allInputs, instanceIds, rowid, colid);
}

void SmoothSatHelper::getControls(map<string, double>& ctrls) {
	for (auto it = ctrlMap.begin(); it != ctrlMap.end(); it++) {
		ctrls[it->first] = gsl_vector_get(state, it->second);
	}
}
