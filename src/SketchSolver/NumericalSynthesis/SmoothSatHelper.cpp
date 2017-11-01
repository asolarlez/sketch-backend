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
	
	GradUtil::tmp = gsl_vector_alloc(ncontrols);
	GradUtil::tmp1 = gsl_vector_alloc(ncontrols);
	GradUtil::tmp2 = gsl_vector_alloc(ncontrols);
	GradUtil::tmp3 = gsl_vector_alloc(ncontrols);
	GradUtil::tmpT = gsl_vector_alloc(ncontrols);
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

vector<tuple<int, int, int>> SmoothSatHelper::collectSuggestions() {
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

vector<pair<int, int>> SmoothSatHelper::getConflicts(int rowid, int colid) {
	return cg->getConflicts(state, allInputs, instanceIds, rowid, colid);
}

void SmoothSatHelper::getControls(map<string, double>& ctrls) {
	for (auto it = ctrlMap.begin(); it != ctrlMap.end(); it++) {
		ctrls[it->first] = gsl_vector_get(state, it->second);
	}
}
