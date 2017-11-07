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
	eval = new BoolAutoDiff(*dag, fm, ctrlMap);
    seval = new SimpleEvaluator(*dag, fm, ctrlMap, boolCtrlMap);
	if (PARAMS->useSnopt) {
		opt = new SnoptWrapper(eval, dag, imap, ctrlMap, boolNodes, ncontrols, numConstraints);
	} else {
		opt = new GradientDescentWrapper(eval, dag, imap, ctrlMap, boolNodes, ncontrols);
	}
	cg = new SimpleConflictGenerator(imap, boolNodes);
    previousSAT = false;
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
    if (PARAMS->verbosity > 7) {
    for (int i = 0; i < allInputs[0].size(); i++) {
        if (allInputs[0][i] == 0 || allInputs[0][i] == 1) {
            cout << imap[i] << "," << allInputs[0][i] << ";";
        }
    }
    cout << endl;
    }
}

bool SmoothSatHelper::checkInputs(int rowid, int colid) {
    if (imap[colid] == -1) {
        if (PARAMS->verbosity > 7) {
            cout << "Setting dummy variable" << endl;
        }
    } else {
        if (PARAMS->verbosity > 7) {
            cout << "Setting " << (*dag)[imap[colid]]->lprint() << " to " << allInputs[rowid][colid] << endl;
        }
    }
	return true;
}

bool SmoothSatHelper::validObjective() {
	// if all bool holes are set, we will be solving the full problem
	for (int i = 0; i < allInputs.size(); i++) {
		for (int j = 0; j < allInputs[i].size(); j++) {
			int nid = imap[j];
			if (nid < 0) return true;
			bool_node* n = (*dag)[nid];
			if (n->getOtype() == OutType::BOOL && n->type == bool_node::CTRL && allInputs[i][j] != 0 && allInputs[i][j] != 1) {
				return false;
			}
		}
	}
	return true;
}

bool SmoothSatHelper::checkSAT() {
    if (!previousSAT) {
        opt->randomizeCtrls(state);
    }
    bool suppressPrint = PARAMS->verbosity > 7 ? false : true;
	bool sat = opt->optimize(allInputs, state, suppressPrint);
    if (!previousSAT) {
        previousSAT = sat;
    }
	if (validObjective()) { // check whether the current opt problem is valid for considering the objection (i.e. make sure it does not solve a part of the problem)
		double objective = opt->getObjectiveVal();
		cout << "Objective found: " << objective << endl;
	}
	if (sat) {
		state = opt->getMinState();
	}
	return sat;
}

bool SmoothSatHelper::ignoreConflict() {
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
        reverse(s.begin(), s.end());
        for (int k = 0; k < s.size(); k++) {
            int idx = get<1>(s[k]);
            if (allInputs[i][idx] == EMPTY) {
                suggestions.push_back(make_tuple(i, idx, get<2>(s[k])));
            }
        }
    }
	return suggestions;
}

vector<tuple<int, int, int>> SmoothSatHelper::collectUnsatSuggestions() {
    // No suggestions
    vector<tuple<int, int, int>> suggestions;
    for (int i = 0; i < allInputs.size(); i++) {
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
                    influentialNodes.insert(node->mother);
                }
            } else if (node->getOtype() == OutType::BOOL) {
                Assert(eval->hasDist(node), "uetuoqw");
                float dist = eval->getVal(node);
                auto it = givenNodesMap.find(node->id);
                if (it != givenNodesMap.end()) {
                    if ((it->second == 1 && dist < 0.01) || (it->second == 0 && dist > -0.01)) {
                        influentialNodes.insert(node);
                    }
                }
            } // TODO: currently not handling sqrt nodes.
        }
        
        
        double maxDist = GradUtil::MINVAL;
        bool_node* maxDistNode = NULL;
        for (auto it = influentialNodes.begin(); it != influentialNodes.end(); it++)  {
            const set<int>& nodes = Util::getRelevantNodes(*it);
            map<int, pair<double, bool_node*>> ctrlToLowestDist;
            for (auto it1 = nodes.begin(); it1 != nodes.end(); it1++) {
                bool_node* n = (*dag)[*it1];
                if (n->getOtype() == OutType::BOOL && Util::hasArraccChild(n)) {
                    if (givenNodesMap.find(n->id) != givenNodesMap.end()) continue; // already set
                    double dist = abs(eval->getVal(n));
                    if (dist < 2.0) continue; // distance is within the smoothing range
                    gsl_vector* grad = eval->getGrad(n);
                    for (int j = 0; j < grad->size; j++) {
                        if (abs(gsl_vector_get(grad, j)) > 0.1) {
                            auto it2 = ctrlToLowestDist.find(j);
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
                if (it3->second.first > maxDist) {
                    maxDist = it3->second.first;
                    maxDistNode = it3->second.second;
                }
            }
        }
        
        if (maxDistNode != NULL) {
            cout << "Suggesting to flip: " << maxDistNode->lprint() << " cur dist: " << maxDist << endl;
            for (int idx = 0; idx < imap.size(); idx++) {
                if (imap[idx] == maxDistNode->id) {
                    suggestions.push_back(make_tuple(i, idx, !(eval->getVal(maxDistNode) > 0.0)));
                }
            }
        }
    }
    return suggestions;
}

vector<pair<int, int>> SmoothSatHelper::getConflicts(int rowid, int colid) {
	return cg->getConflicts(state, allInputs, instanceIds, rowid, colid);
}

void SmoothSatHelper::getControls(map<string, double>& ctrls) {
	for (auto it = ctrlMap.begin(); it != ctrlMap.end(); it++) {
		ctrls[it->first] = gsl_vector_get(state, it->second);
	}
}
