#include "BoolApproxHelper.h"


BoolApproxHelper::BoolApproxHelper(FloatManager& _fm, BooleanDAG* _dag, map<int, int>& _imap): NumericalSolverHelper(_fm, _dag, _imap) {
	//dag->lprint(cout);
	
	// Collect the list of boolean nodes that contribute to the error
	// In this case, only the assertions matter
	for (int i = 0; i < dag->size(); i++) {
		bool_node* n = (*dag)[i];
		if (n->type == bool_node::ASSERT) {
			if (!((ASSERT_node*)n)->isHard()) {
				boolNodes.insert(i);
			}
		}
        if (Util::isSqrt(n)) {
            boolNodes.insert(i);
        }
        
	}
	
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
    if (PARAMS->relaxBoolHoles) {
        for (int i = 0; i < ctrls.size(); i++) {
            if (ctrls[i]->getOtype() == OutType::BOOL) {
                boolCtrlMap[ctrls[i]->get_name()] = ctr++;
            }
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
	if (PARAMS->useSnopt) {
		opt = new SnoptWrapper(eval, dag, imap, ctrlMap, boolNodes, ncontrols, boolNodes.size());
	} else {
		opt = new GradientDescentWrapper(eval, dag, imap, ctrlMap, boolNodes, ncontrols);
	}
	//opt->randomizeCtrls(state, allInputs);
	cg = new SimpleConflictGenerator(imap, boolNodes);
    done = false;
}

BoolApproxHelper::~BoolApproxHelper(void) {
	delete GradUtil::tmp;
	delete GradUtil::tmp1;
	delete GradUtil::tmp2;
	delete GradUtil::tmp3;
	delete GradUtil::tmpT;
}

void BoolApproxHelper::setInputs(vector<vector<int>>& allInputs_, vector<int>& instanceIds_) {
	allInputs = allInputs_;
	instanceIds = instanceIds_;
    cout << "I: ";
    for (int i = 0; i < allInputs[0].size(); i++) {
        if (allInputs[0][i] == 0 || allInputs[0][i] == 1) {
            cout << imap[i] << "," << allInputs[0][i] << ";";
        }
    }
    cout << endl;
}

bool BoolApproxHelper::checkInputs(int rowid, int colid) {
    if (done) return false;// TODO: hacky remove later
    if (PARAMS->relaxBoolHoles) return true;
    for (int i = 0; i < allInputs[0].size(); i++) {
        if (allInputs[0][i] != 0 && allInputs[0][i] != 1) {
            return false;
        }
    }
	return true;
}

bool BoolApproxHelper::validObjective() {
    return true;
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

bool BoolApproxHelper::checkSAT() {
    opt->randomizeCtrls(state, allInputs);
    bool suppressPrint = PARAMS->verbosity > 7 ? false : true;
	bool sat = opt->optimize(allInputs, state, suppressPrint);
	if (validObjective()) { // check whether the current opt problem is valid for considering the objection (i.e. make sure it does not solve a part of the problem)
		double objective = opt->getObjectiveVal();
		cout << "Objective found: " << objective << endl;
	}
	if (sat) {
		gsl_vector_memcpy(state, opt->getMinState());
        cout << "FOUND solution" << endl;
    }
    if (validObjective()) {
        // check if actually good solution // TODO: only debug code, not performance efficient
        int ctr = ncontrols;
        if (!PARAMS->relaxBoolHoles) {
            for (int i = 0; i < allInputs[0].size(); i++) {
                if (imap[i] < 0) continue;
                bool_node* n = (*dag)[imap[i]];
                if (n->type == bool_node::CTRL &&  n->getOtype() == OutType::BOOL) {
                    boolCtrlMap[n->get_name()] = ctr++;
                }
            }
        }
        gsl_vector* newctrls = gsl_vector_alloc(ctr);
        for (int i = 0; i < ncontrols; i++) {
            gsl_vector_set(newctrls, i, gsl_vector_get(state, i));
        }
        /*for (int i = 0; i < allInputs[0].size(); i++) {
         if (imap[i] < 0) continue;
         bool_node* n = (*dag)[imap[i]];
         if (n->type == bool_node::CTRL && n->getOtype() == OutType::BOOL) {
         gsl_vector_set(newctrls, boolCtrlMap[n->get_name()], allInputs[0][i]);
         }
         }*/
        cout << Util::print(newctrls) << endl;
        SimpleEvaluator* seval = new SimpleEvaluator(*dag, fm, ctrlMap, boolCtrlMap);
        double error = 0;
        if (seval->check(newctrls, error)) {
            cout << "FULL SAT" << endl;
        }
        cout << "ERROR: " << error << endl;
    }
	
    done = true;
    return true;
	//return sat;
}

bool BoolApproxHelper::ignoreConflict() {
    for (int i = 0; i < allInputs[0].size(); i++) {
        if (allInputs[0][i] != 0 && allInputs[0][i] != 1) {
            return true;
        }
    }
    return false;

}

vector<tuple<int, int, int>> BoolApproxHelper::collectSatSuggestions() {
	// No suggestions
	vector<tuple<int, int, int>> suggestions;
	return suggestions;
}

vector<tuple<int, int, int>> BoolApproxHelper::collectUnsatSuggestions() {
    // No suggestions
    vector<tuple<int, int, int>> suggestions;
    return suggestions;
}

vector<pair<int, int>> BoolApproxHelper::getConflicts(int rowid, int colid) {
	return cg->getConflicts(state, allInputs, instanceIds, rowid, colid);
}

void BoolApproxHelper::getControls(map<string, double>& ctrls) {
	for (auto it = ctrlMap.begin(); it != ctrlMap.end(); it++) {
		ctrls[it->first] = gsl_vector_get(state, it->second);
	}
}
