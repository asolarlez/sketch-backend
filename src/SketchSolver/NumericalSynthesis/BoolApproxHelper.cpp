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
	ncontrols = ctr;
	if (ncontrols == 0) {
		ncontrols = 1;
	}
	
	cout << "NControls: " << ncontrols << endl;
	
	state = gsl_vector_alloc(ncontrols);
	eval = new BoolAutoDiff(*dag, fm, ctrlMap);
	if (PARAMS->useSnopt) {
		opt = new SnoptWrapper(eval, dag, imap, ctrlMap, boolNodes, ncontrols, boolNodes.size());
	} else {
		opt = new GradientDescentWrapper(eval, dag, imap, ctrlMap, boolNodes, ncontrols);
	}
	opt->randomizeCtrls(state);
	cg = new SimpleConflictGenerator(imap, boolNodes);
	
	GradUtil::tmp = gsl_vector_alloc(ncontrols);
	GradUtil::tmp1 = gsl_vector_alloc(ncontrols);
	GradUtil::tmp2 = gsl_vector_alloc(ncontrols);
	GradUtil::tmp3 = gsl_vector_alloc(ncontrols);
	GradUtil::tmpT = gsl_vector_alloc(ncontrols);
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
}

bool BoolApproxHelper::checkInputs(int rowid, int colid) {
	return true;
}

bool BoolApproxHelper::validObjective() {
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
    bool suppressPrint = PARAMS->verbosity > 7 ? false : true;
	bool sat = opt->optimize(allInputs, state, suppressPrint);
	if (validObjective()) { // check whether the current opt problem is valid for considering the objection (i.e. make sure it does not solve a part of the problem)
		double objective = opt->getObjectiveVal();
		cout << "Objective found: " << objective << endl;
	}
	if (sat) {
		state = opt->getMinState();
	}
	return sat;
}

bool BoolApproxHelper::ignoreConflict() {
	return false;
}

vector<tuple<int, int, int>> BoolApproxHelper::collectSuggestions() {
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
