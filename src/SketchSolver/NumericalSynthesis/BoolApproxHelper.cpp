#include "BoolApproxHelper.h"


BoolApproxHelper::BoolApproxHelper(FloatManager& _fm, BooleanDAG* _dag, map<int, int>& _imap): NumericalSolverHelper(_fm, _dag, _imap) {
	dag->lprint(cout);
	
	// Collect the list of boolean nodes which can be ignored.
	// Basically all nodes that are not boolean holes can be ignored - is this correct? maybe we want to use the other boolean nodes as well to get rid of softmax/softmin approximation
	for (int i = 0; i < dag->size(); i++) {
		bool_node* n = (*dag)[i];
		if (n->getOtype() == OutType::BOOL && n->type == bool_node::CTRL) {
			boolNodes.insert(i);
		}
		if (n->type == bool_node::ASSERT) {
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
	opt = new SnoptWrapper(eval, dag, imap, ctrlMap, boolNodes, ncontrols);
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
	int nid = imap[colid];
	if (boolNodes.find(nid) == boolNodes.end()) {
		return false;
	}
	cout << (*dag)[nid]->lprint() << endl;
	cout << allInputs[rowid][colid] << endl;
	return true;
}

bool BoolApproxHelper::checkSAT() {
	bool sat = opt->optimize(allInputs, state);
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

void BoolApproxHelper::getControls(map<string, float>& ctrls) {
	for (auto it = ctrlMap.begin(); it != ctrlMap.end(); it++) {
		ctrls[it->first] = gsl_vector_get(state, it->second);
	}
}
