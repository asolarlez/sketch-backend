#include "IteApproxNumericalHelper.h"


IteApproxNumericalHelper::IteApproxNumericalHelper(FloatManager& _fm, BooleanDAG* _dag, map<int, int>& _imap): NumericalSolverHelper(_fm, _dag, _imap) {
    for (int i = 0; i < _imap.size(); i++) {
        cout << i << " " << (*dag)[imap[i]]->lprint() << endl;
    }
	// Collect the list of boolean nodes which matters.
	for (int i = 0; i < dag->size(); i++) {
		// ignore boolean nodes that don't have either float children or float parents
		bool_node* n = (*dag)[i];
		if (n->getOtype() == OutType::BOOL && (n->hasFloatParent() || n->hasFloatChild())) {
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
    
    for (int i = 0; i < dag->size(); i++) {
        bool_node* n = (*dag)[i];
        if (n->getOtype() == OutType::FLOAT && n->type == bool_node::ARRACC) {
            extraCtrlMap[n->id] = ctr++;
        }
    }
	ncontrols = ctr;
	if (ncontrols == 0) {
		ncontrols = 1;
	}
	
	cout << "NControls: " << ncontrols << endl;
	
	state = gsl_vector_alloc(ncontrols);
	eval = new IteAutoDiff(*dag, fm, ctrlMap, extraCtrlMap);
	seval = new SimpleEvaluator(*dag, fm, ctrlMap, boolCtrlMap);
	if (PARAMS->useSnopt) {
		opt = new SnoptWrapper(eval, dag, imap, ctrlMap, boolNodes, ncontrols, boolNodes.size());
	} else {
		opt = new GradientDescentWrapper(eval, dag, imap, ctrlMap, boolNodes, ncontrols);
	}
	//cg = new ConflictGenerator(eval, imap, dag, ignoredBoolNodes, ctrlNodeIds);
	cg = new SimpleConflictGenerator(imap, boolNodes);
	opt->randomizeCtrls(state);

	
	GradUtil::tmp = gsl_vector_alloc(ncontrols);
	GradUtil::tmp1 = gsl_vector_alloc(ncontrols);
	GradUtil::tmp2 = gsl_vector_alloc(ncontrols);
	GradUtil::tmp3 = gsl_vector_alloc(ncontrols);
	GradUtil::tmpT = gsl_vector_alloc(ncontrols);
}

IteApproxNumericalHelper::~IteApproxNumericalHelper(void) {
	delete GradUtil::tmp;
	delete GradUtil::tmp1;
	delete GradUtil::tmp2;
	delete GradUtil::tmp3;
	delete GradUtil::tmpT;
}

void IteApproxNumericalHelper::setInputs(vector<vector<int>>& allInputs_, vector<int>& instanceIds_) {
	allInputs = allInputs_;
	instanceIds = instanceIds_;
}

bool IteApproxNumericalHelper::checkInputs(int rowid, int colid) {
	int nid = imap[colid];
	if (nid < 0) return true;
	if (boolNodes.find(nid) == boolNodes.end()) {
		return false;
	}
	if (PARAMS->useEager) {
		if (PARAMS->verbosity > 7) {
			cout << (*dag)[nid]->lprint() << endl;
			cout << allInputs[rowid][colid] << endl;
		}
		return true;
	} else {
		for (int i = 0; i < allInputs[0].size(); i++) { //TODO: not sensitive to multiple inputs
			if (boolNodes.find(imap[i]) != boolNodes.end() && allInputs[0][i] != 0 && allInputs[0][i] != 1) {
				return false;
			}
		}
	}
	return true;
}

bool IteApproxNumericalHelper::checkSAT() {
	bool sat = opt->optimize(allInputs, state);
	if (sat) {
		state = opt->getMinState();
	}
	return sat;
}

bool IteApproxNumericalHelper::ignoreConflict() {
	return false;
}

vector<tuple<int, int, int>> IteApproxNumericalHelper::collectSuggestions() {
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

vector<pair<int, int>> IteApproxNumericalHelper::getConflicts(int rowid, int colid) {
	return cg->getConflicts(state, allInputs, instanceIds, rowid, colid);
}

void IteApproxNumericalHelper::getControls(map<string, double>& ctrls) {
	for (auto it = ctrlMap.begin(); it != ctrlMap.end(); it++) {
		ctrls[it->first] = gsl_vector_get(state, it->second);
	}
}
