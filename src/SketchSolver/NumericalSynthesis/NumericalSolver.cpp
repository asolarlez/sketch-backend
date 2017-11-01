#include "NumericalSolver.h"

gsl_vector* GDEvaluator::curGrad;
gsl_vector* SnoptEvaluator::state;
gsl_vector* SnoptEvaluator::grad;

NumericalSolver::NumericalSolver(FloatManager& _fm, BooleanDAG* _dag, map<int, int>& _imap, Lit _softConflictLit): Synthesizer(_fm), dag(_dag), imap(_imap) {
    softConflictLit = _softConflictLit;
    cout << "Special lit: " << toInt(softConflictLit) << " " << (toInt(~softConflictLit)) << endl;
    if (PARAMS->verbosity > 2) {
		cout << "NInputs: " << imap.size() << endl;
	}
	if (PARAMS->numericalSolverMode == "ONLY_SMOOTHING") {
		helper = new BoolApproxHelper(_fm, dag, imap);
	} else if (PARAMS->numericalSolverMode == "FULLY_SEPARATED") {
		helper = new BasicNumericalHelper(_fm, dag, imap);
    } else if (PARAMS->numericalSolverMode == "INTERACTIVE") {
        helper = new IteApproxNumericalHelper(_fm, dag, imap);
    } else if (PARAMS->numericalSolverMode == "SMOOTHING_SAT") {
        helper = new SmoothSatHelper(_fm, dag, imap);
    } else {
		Assert(false, "Error: specify which numerical helper to use for solver mode: " + PARAMS->numericalSolverMode);
	}
	//helper = new InequalityHelper(_fm, dag, imap);
	//debug();
    if (PARAMS->checkInput) {
        checkInput();
        exit(0);
    }
    counter = 0;
    
}

bool NumericalSolver::synthesis(int rowid, int colid, int val, int level, vec<Lit>& suggestions) {
	conflict.clear();
	vector<vector<int>> allInputs;
	vector<int> instanceIds;
	
	collectAllInputs(allInputs, instanceIds);
	helper->setInputs(allInputs, instanceIds);
	
	if (!helper->checkInputs(rowid, colid)) return true;

	if (PARAMS->verbosity > 7) {
        timer.restart();
		printInputs(allInputs);
		cout << "Col Id: " << colid << endl;
        cout << counter++ << endl;
	}
	suggestions.clear();
	bool sat = helper->checkSAT();
	if (sat || helper->ignoreConflict()) {
		helper->getControls(ctrlVals);
        if (sat) {
            const vector<tuple<int, int, int>>& s = helper->collectSuggestions(); // <instanceid, inputid, val>
            convertSuggestions(s, suggestions);
        }
        if (PARAMS->verbosity > 7) {
            timer.stop().print("Numerical solving time:");
        }
		return true;
	} else {
		if (PARAMS->verbosity > 7) {
			cout << "****************CONFLICT****************" << endl;
		}
		const vector<pair<int, int>>& c = helper->getConflicts(rowid, colid); // <instanceid, inputid>
		convertConflicts(c);
        if (PARAMS->verbosity > 7) {
            timer.stop().print("Numerical solving time:");
        }
		return false;
	}
}

bool_node* NumericalSolver::getExpression(DagOptim* dopt, const vector<bool_node*>& params) {
	// Add the appropriate expression from the dag after replacing inputs with params and ctrls with synthesized parameters
	BooleanDAG newdag = *(dag->clone());
	for (int i = 0; i < newdag.size(); i++) {
		if (newdag[i]->type == bool_node::CTRL) {
			// TODO: what to do with non float ctrls that are solved by the SAT solver??
			newdag.replace(i, dopt->getCnode(ctrlVals[newdag[i]->get_name()]));
		} else {
			if (newdag[i]->type != bool_node::DST && newdag[i]->type != bool_node::SRC) {
				bool_node* n = dopt->computeOptim(newdag[i]);
				if (n == newdag[i]) {
					dopt->addNode(n);
				}
				if (newdag[i] != n) {
					newdag.replace(i, n);
				}
				if (n->type == bool_node::ASSERT) {
					dopt->addAssert((ASSERT_node*)n);
				}
				
			}
		}
	}
	return dopt->getCnode(0);
}


void NumericalSolver::getConstraintsOnInputs(SolverHelper* dir, vector<Tvalue>& inputs) {
	return;
	map<string, multimap<double, int>> ctrlToInputIds;
	
	// First, group inputs based on ctrl names
	for (int i = 0; i < inputs.size(); i++) {
		if (imap[i] < 0) continue;
		bool_node* n = (*dag)[imap[i]];
		//cout << n->lprint() << " " << inputs[i] << endl;
		if (n->type == bool_node::LT) {
			bool_node* m = n->mother;
			bool_node* f = n->father;
			
			if (m->type == bool_node::CTRL && f->type == bool_node::CONST) {
				string name = m->get_name();
				double val = ((CONST_node*) f)->getFval();
				if (ctrlToInputIds.find(name) != ctrlToInputIds.end()) {
					ctrlToInputIds[name].insert(pair<double, int>(val, i)); // it is not possible to have to two nodes with the
				} else {
					multimap<double, int> ids;
					ids.insert(pair<double, int>(val, i));
					ctrlToInputIds[name] = ids;
				}
			}
			
			if (f->type == bool_node::CTRL && m->type == bool_node::CONST) {
				string name = f->get_name();
				double val = ((CONST_node*) m)->getFval();
				if (ctrlToInputIds.find(name) != ctrlToInputIds.end()) {
					ctrlToInputIds[name].insert(pair<double, int>(val, i));
				} else {
					multimap<double, int> ids;
					ids.insert(pair<double, int>(val, i));
					ctrlToInputIds[name] = ids;
				}
			}
		}
	}
	//cout << ctrlToInputIds.size() << endl;
	// Next, generate constraints for each pair of inputs that have the same ctrl
	for (auto it = ctrlToInputIds.begin(); it != ctrlToInputIds.end(); it++) {
		auto& ids_map = it->second;
		vector<int> ids;
		for (auto id_it = ids_map.begin(); id_it != ids_map.end(); id_it++) {
			//cout << id_it->first << ", ";
			ids.push_back(id_it->second);
		}
		//cout << endl;
		//cout << ids.size() << endl;
		for (int i = 0; i < ids.size() - 1; i++) {
			double t1;
			bool reverse1; // straight is ctrl < const, reverse is const < ctrl
			int a = inputs[ids[i]].getId();
			bool_node* n1 = (*dag)[imap[ids[i]]];
			if (n1->mother->type == bool_node::CTRL) {
				reverse1 = false;
				t1 = ((CONST_node*)n1->father)->getFval();
			} else {
				reverse1 = true;
				t1 = ((CONST_node*)n1->mother)->getFval();
			}
			
			int j = i+1;
			double t2;
			bool reverse2; // straight is ctrl < const, reverse is const < ctrl
			int b = inputs[ids[j]].getId();
			bool_node* n2 = (*dag)[imap[ids[j]]];
			if (n2->mother->type == bool_node::CTRL) {
				reverse2 = false;
				t2 = ((CONST_node*)n2->father)->getFval();
			} else {
				reverse2 = true;
				t2 = ((CONST_node*)n2->mother)->getFval();
			}
			
			if (!reverse1 && !reverse2) { // x < t1, x < t2
				if (t1 < t2) { // a = T => b = T
					dir->addHelperC(-a, b);
				}
				if (t1 == t2) { // a = b
					dir->addEquateClause(a, b);
				}
				if (t1 > t2) { // a = F => b = F
					dir->addHelperC(a, -b);
				}
			} else if (!reverse1 && reverse2) { // x < t1, x > t2
				if (t1 < t2) { // a = T => b = F
					dir->addHelperC(-a, -b);
				}
				if (t1 == t2) { // a = -b
					dir->addEquateClause(a, -b);
				}
				if (t1 > t2) { // a = F => b = T
					dir->addHelperC(a, b);
				}
			} else if (reverse1 && !reverse2) { // x > t1, x < t2
				if (t1 < t2) { // a = F => b = T
					dir->addHelperC(a, b);
				}
				if (t1 == t2) { // a = -b
					dir->addEquateClause(a, -b);
				}
				if (t1 > t2) { // a = T => b = F
					dir->addHelperC(-a, -b);
				}
			} else { // x > t1, x > t2
				if (t1 < t2) { // a = F => b = F
					dir->addHelperC(a, -b);
				}
				if (t1 == t2) { // a = b
					dir->addEquateClause(a, b);
				}
				if (t1 > t2) { // a = T => b = T
					dir->addHelperC(-a, b);
				}
			}
		}
		
	}
}

void NumericalSolver::collectAllInputs(vector<vector<int>>& allInputs, vector<int>& instanceIds) {
	for (int i = 0; i < inout->getNumInstances(); ++i) {
		vector<int> inputs;
		for (int j = 0; j < imap.size(); j++) { // TODO: in some cases (especially in fully separated mode), this looping can be bottleneck
			int val = inout->getVal(i, j);
			inputs.push_back(val);
		}
		allInputs.push_back(inputs);
		instanceIds.push_back(i);
	}
	Assert(allInputs.size() == instanceIds.size(), "This should not be possible");
}

void NumericalSolver::printInputs(vector<vector<int>>& allInputs) {
	for (int k = 0; k < allInputs.size(); k++) {
		cout << "Input: ";
		for (int i = 0; i < allInputs[k].size(); i++) {
			if (allInputs[0][i] == EMPTY) {
				cout << "2,";
			} else {
				cout << allInputs[0][i] << ",";
			}
		}
		cout << endl;
	}
}

void NumericalSolver::convertSuggestions(const vector<tuple<int, int, int>>& s, vec<Lit>& suggestions) {
	for (int k = 0; k < s.size(); k++) {
		int i = get<0>(s[k]);
		int j = get<1>(s[k]);
		int v = get<2>(s[k]);
		//cout << "Suggesting " << i << " " << j <<  " " << v << endl;
		suggestions.push(getLit(inout->valueid(i, j), v));
	}
}

void NumericalSolver::convertConflicts(const vector<pair<int, int>>& c) {
	for (int k = 0; k < c.size(); k++) {
		int i = c[k].first;
		int j = c[k].second;
		conflict.push(inout->valueid(i, j));
	}
    softConflict = true;
}

/* Only used for debugging */
void checkOpt(vector<vector<int>>& allInputs, gsl_vector* s, OptimizationWrapper* opt, int idx) {
    double i = -20.0;
    while (i < 20.0) {
        gsl_vector_set(s, idx, i);
        bool sat = opt->optimize(allInputs, s, true);
        cout << i << " " << sat << " " << opt->getObjectiveVal() << " " << gsl_vector_get(opt->getMinState(), idx);
        if (!sat || opt->getObjectiveVal() > 0.01) {
            cout << " LOCAL" << endl;
        } else {
            cout << endl;
        }
        i += 0.1;
    }
    
}


// Debug with a fixed input and/or controls
void NumericalSolver::debug() { // TODO: currently this is doing all kinds of debugging - should separate them
	vector<vector<int>> allInputs;
	vector<int> instanceIds;
	vector<int> inputs;
    int arr[1] = {2};
	for (int i = 0; i < imap.size(); i++) {
		//if (arr[i] == 2) {
			inputs.push_back(EMPTY);
		//} else {
		//	inputs.push_back(arr[i]);
		//}
	}
	allInputs.push_back(inputs);
	instanceIds.push_back(0);
	helper->setInputs(allInputs, instanceIds);
	printInputs(allInputs);
	
	// generate ctrls mapping
	map<string, int> ctrlMap;
	vector<bool_node*>& ctrls = dag->getNodesByType(bool_node::CTRL);
	int ctr = 0;
	for (int i = 0; i < ctrls.size(); i++) {
		if (ctrls[i]->getOtype() == OutType::FLOAT) {
			ctrlMap[ctrls[i]->get_name()] = ctr++;
		}
	}
	int ncontrols = ctr;
	if (ncontrols == 0) {
		ncontrols = 1;
	}
	cout << "NControls: " << ncontrols << endl;
    
    set<int> boolNodes; // This is dependent on different techniques
    for (int i = 0; i < dag->size(); i++) {
        bool_node* n = (*dag)[i];
        if (n->type == bool_node::ASSERT) {
            if (!((ASSERT_node*)n)->isHard()) {
                boolNodes.insert(i);
            }
        }
    }
	
	SymbolicEvaluator* eval = new BoolAutoDiff(*dag, fm, ctrlMap);
    OptimizationWrapper* opt = new SnoptWrapper(eval, dag, imap, ctrlMap, boolNodes, ncontrols, boolNodes.size());
	const map<int, int>& nodeValsMap = Util::getNodeToValMap(imap, allInputs[0]);
	gsl_vector* s = gsl_vector_alloc(ncontrols);
    double arr1[10] = {8.1312, -5.48764, 1.33356, 6.65935, -9.71224, -5.8, 8.76646, 10.1233, 11.9636, 11.1991};
	for (int i = 0; i < ncontrols; i++) {
		gsl_vector_set(s, i, arr1[i]);
	}
    
    map<string, int> boolCtrlMap;
    SimpleEvaluator* seval = new SimpleEvaluator(*dag, fm, ctrlMap, boolCtrlMap);
    
    vector<tuple<double, int, int>> suggestions = seval->run(s, imap);
    for (int k = 0; k < suggestions.size(); k++) {
        int idx = get<1>(suggestions[k]);
        cout << imap[idx] << "," << get<2>(suggestions[k]) << ";";
    }
    cout << endl;

    //opt->optimize(allInputs, s);
    /*for (int i = 0; i < ncontrols; i++) {
        cout << "Checking " << i << endl;
        checkOpt(allInputs, s, opt, i);
        gsl_vector_set(s, i, arr1[i]);
    }*/
    
    /*GradUtil::BETA = -1;
    GradUtil::ALPHA = 1;
    eval->run(s, nodeValsMap);
	//eval->printFull();
    gsl_vector* d = gsl_vector_alloc(ncontrols);
    for (BooleanDAG::iterator node_it = dag->begin(); node_it != dag->end(); node_it++) {
        bool_node* node = *node_it;
        if (node->type == bool_node::ASSERT) {
            float dist = eval->computeDist(node->mother, d);
            if (dist < 0.01 || ((ASSERT_node*)node)->isHard()) {
                cout << node->lprint() << endl;
                cout << "Dist: " << dist << endl;
                cout << "Grad: ";
                for (int i = 0; i < ncontrols; i++) {
                    cout << gsl_vector_get(d, i) << ", ";
                }
                cout << endl;
            }
        }
    }*/
    
    /*for (int i = 0; i < ncontrols; i++) {
		genData(s, i, eval, nodeValsMap);
		gsl_vector_set(s, i, arr1[i]);
	}*/
    exit(0);
}


set<int> NumericalSolver::getRelevantIds() {
    set<int> ids;
    bool_node* minNode = NULL;
    for (BooleanDAG::iterator node_it = dag->begin(); node_it != dag->end(); node_it++) {
        bool_node* node = (*node_it);
        if (node->type == bool_node::ASSERT) {
            if (((ASSERT_node*)node)->isHard()) {
                minNode = node;
                break;
            }
        }
    }
    Assert(minNode != NULL, "uqiep");
    
    set<int> visitedIds;
    vector<bool_node*> toVisit;
    toVisit.push_back(minNode);
    
    while(toVisit.size() > 0) {
        bool_node* node = toVisit.back();
        toVisit.pop_back();
        if (visitedIds.find(node->id) == visitedIds.end()) {
            visitedIds.insert(node->id);
            if (node->type == bool_node::ARRACC) {
                ids.insert(node->id);
                ids.insert(node->mother->id);
                ids.insert(((ARRACC_node*)node)->multi_mother[0]->id);
                ids.insert(((ARRACC_node*)node)->multi_mother[1]->id);
            }
            const vector<bool_node*>& parents = node->parents();
            for (int i = 0; i < parents.size(); i++) {
                toVisit.push_back(parents[i]);
            }
        }
    }
    return ids;
}

double NumericalSolver::getError(SymbolicEvaluator* eval, const map<int, int>& nodeValsMap, gsl_vector* d, bool useSnopt) {
    double err = 0.0;
    if (useSnopt) {
        bool isValid = true;
        for (BooleanDAG::iterator node_it = dag->begin(); node_it != dag->end(); node_it++) {
            bool_node* node = *node_it;
            if (node->type == bool_node::ASSERT) {
                if (((ASSERT_node*) node)->isHard()) {
                    if (eval->hasDist(node->mother)) {
                        err = eval->computeDist(node->mother, d);
                    }
                } else {
                    float dist = 0.0;
                    if (eval->hasDist(node->mother)) {
                        dist = eval->computeDist(node->mother, d);
                    }
                    if (dist < 0) {
                        isValid = false;
                    }
                }
                //err += eval->computeError(node->mother, 1, d);
            }
        }
        //cout << i << " " << err << " " << isValid << endl;
        //analyze(eval, d, idx, relevantIds);
        //if (!isValid) {
        //    err = 1000.0;
        //}
    } else {
        for (BooleanDAG::iterator node_it = dag->begin(); node_it != dag->end(); ++node_it) {
            bool_node* n = *node_it;
            if (n->type ==  bool_node::ASSERT) {
                err += eval->computeError(n->mother, 1, d);
            } else if (n->getOtype() == OutType::BOOL) {
                auto it = nodeValsMap.find(n->id);
                if (it != nodeValsMap.end()) {
                    int val = it->second;
                    err += eval->computeError(n, val, d);
                }
            }
        }
    }
    return err;
}


void NumericalSolver::genData(gsl_vector* state, int idx, SymbolicEvaluator* eval, const map<int, int>& nodeValsMap, bool useSnopt) {
	GradUtil::BETA = -100;
	GradUtil::ALPHA = 100;
	gsl_vector* d = gsl_vector_alloc(state->size);
	
	ofstream file("/Users/Jeevu/projects/symdiff/scripts/graphs/controllers/g" + to_string(idx) + ".txt");
    //const set<int>& relevantIds = getRelevantIds();
	{
		double i = -20.0;
		while (i < 20.0) {
			gsl_vector_set(state, idx, i);
			eval->run(state, nodeValsMap);
			gsl_vector_set_zero(d);
            double err = getError(eval, nodeValsMap, d, useSnopt);
            
			file << err << ";";
			i += 0.1;
		}
	}
	file << endl;
    file.close();
}

void NumericalSolver::genData2D(gsl_vector* state, int idx1, int idx2, SymbolicEvaluator* eval, const map<int, int>& nodeValsMap, bool useSnopt) {
    GradUtil::BETA = -100;
    GradUtil::ALPHA = 100;
    gsl_vector* d = gsl_vector_alloc(state->size);
    
    ofstream file("/Users/Jeevu/projects/symdiff/scripts/graphs/controllers/g" + to_string(idx1) + "_" + to_string(idx2) + ".txt");
    //const set<int>& relevantIds = getRelevantIds();
    {
        double i = -10.0;
        double j = -10.0;
        while (i < 10.0) {
            j = -10.0;
            while (j < 10.0) {
                gsl_vector_set(state, idx1, i);
                gsl_vector_set(state, idx2, j);
                eval->run(state, nodeValsMap);
                gsl_vector_set_zero(d);
                double err = getError(eval, nodeValsMap, d, useSnopt);
            
                file << err << ";";
                j += 0.1;
            }
            i += 0.1;
        }
    }
    file << endl;
    file.close();
}


void NumericalSolver::analyze(SymbolicEvaluator* eval, gsl_vector* d, int idx, const set<int>& nodeids) {
    for (BooleanDAG::iterator node_it = dag->begin(); node_it != dag->end(); node_it++) {
        bool_node* node = (*node_it);
        if (nodeids.find(node->id) == nodeids.end()) continue;
        string fname = "/Users/Jeevu/projects/symdiff/scripts/graphs/controllers/nodes" + to_string(idx);
        ofstream file(fname + "/" + node->lprint() + ".txt", std::ios::app);
        if (node->getOtype() == OutType::BOOL && eval->hasDist(node)) {
            //cout << node->lprint() << " dist" << endl;
            file << eval->computeDist(node, d) << ";";
        } else if (node->getOtype() == OutType::FLOAT && eval->hasVal(node)) {
            //cout << node->lprint() << " val" << endl;
            file << eval->computeVal(node, d) << ";";
        }
        file.close();
    }
}


void NumericalSolver::checkInput() {
    vector<vector<int>> allInputs;
    vector<int> instanceIds;
    vector<int> inputs;
    map<int, int> nodeToInputMap;
    string partialInput = PARAMS->partialInput;
    cout <<partialInput << endl;
    if (partialInput == "FULL") {
        map<string, int> ctrlMap;
        vector<bool_node*>& ctrls = dag->getNodesByType(bool_node::CTRL);
        int ctr = 0;
        for (int i = 0; i < ctrls.size(); i++) {
            if (ctrls[i]->getOtype() == OutType::FLOAT) {
                ctrlMap[ctrls[i]->get_name()] = ctr++;
            }
        }
        int ncontrols = ctr;
        if (ncontrols == 0) {
            ncontrols = 1;
        }
        cout << "NControls: " << ncontrols << endl;

        gsl_vector* s = gsl_vector_alloc(ncontrols);
        double arr1[10] = {8.1312, 5.48764, 1.33356, 6.65935, -9.71224, -5.8, 8.76646, 10.1233, 11.9636, 11.1991};
        for (int i = 0; i < ncontrols; i++) {
            gsl_vector_set(s, i, arr1[i]);
        }
                map<string, int> boolCtrlMap;
        SimpleEvaluator* seval = new SimpleEvaluator(*dag, fm, ctrlMap, boolCtrlMap);
        
        vector<tuple<double, int, int>> suggestions = seval->run(s, imap);
        for (int k = 0; k < suggestions.size(); k++) {
            int idx = get<1>(suggestions[k]);
            nodeToInputMap[imap[idx]] = get<2>(suggestions[k]);
        }
    } else {
        size_t pos = 0, found;
        while ((found = partialInput.find_first_of(";", pos)) != string::npos) {
            string p = partialInput.substr(pos, found - pos);
            int splitIdx = p.find(",");
            int nodeid = stoi(p.substr(0, splitIdx));
            int val = stoi(p.substr(splitIdx + 1, p.length() - splitIdx - 1));
            nodeToInputMap[nodeid] = val;
            pos = found + 1;
        }
    }
    
    for (int i = 0; i < imap.size(); i++) {
        int nodeid = imap[i];
        if (nodeToInputMap.find(nodeid) != nodeToInputMap.end()) {
            inputs.push_back(nodeToInputMap[nodeid]);
        } else {
            inputs.push_back(EMPTY);
        }
    }
    allInputs.push_back(inputs);
    instanceIds.push_back(0);
    helper->setInputs(allInputs, instanceIds);
    printInputs(allInputs);
    helper->checkSAT();
    
    /*
    // generate ctrls mapping
    map<string, int> ctrlMap;
    vector<bool_node*>& ctrls = dag->getNodesByType(bool_node::CTRL);
    int ctr = 0;
    for (int i = 0; i < ctrls.size(); i++) {
        if (ctrls[i]->getOtype() == OutType::FLOAT) {
            ctrlMap[ctrls[i]->get_name()] = ctr++;
        }
    }
    int ncontrols = ctr;
    if (ncontrols == 0) {
        ncontrols = 1;
    }
    cout << "NControls: " << ncontrols << endl;
    SymbolicEvaluator* eval = new BoolAutoDiff(*dag, fm, ctrlMap);
    const map<int, int>& nodeValsMap = Util::getNodeToValMap(imap, allInputs[0]);
    gsl_vector* s = gsl_vector_alloc(ncontrols);
    double arr1[10] = {-8.98569, -14.2144, 2.24176, 238.427, -183.134, -3, 10.2, 11.4458, 9.6, 7.2 };
    for (int i = 0; i < ncontrols; i++) {
        gsl_vector_set(s, i, arr1[i]);
    }
    GradUtil::BETA = -1;
    GradUtil::ALPHA = 1;
    eval->run(s, nodeValsMap);
    eval->printFull();*/
    /*if (ncontrols == 2) {
        genData2D(s, 0, 1, eval, nodeValsMap, false);
        gsl_vector_set(s, 0, arr1[0]);
        gsl_vector_set(s, 1, arr1[1]);
    }
    for (int i = 0; i < ncontrols; i++) {
        genData(s, i, eval, nodeValsMap, false);
        gsl_vector_set(s, i, arr1[i]);
    }*/
    
}
