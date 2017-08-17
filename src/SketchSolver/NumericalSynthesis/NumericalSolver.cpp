#include "NumericalSolver.h"

gsl_vector* GDEvaluator::curGrad;

NumericalSolver::NumericalSolver(FloatManager& _fm, BooleanDAG* _dag, map<int, int>& _imap): Synthesizer(_fm), dag(_dag), imap(_imap) {
	cout << "NInputs: " << imap.size() << endl;
	helper = new InequalityHelper(_fm, dag, imap);
	//helper = new BasicNumericalHelper(_fm, dag, imap);
	//debug();
}

bool NumericalSolver::synthesis(int rowid, int colid, int val, int level, vec<Lit>& suggestions) {
	conflict.clear();
	vector<vector<int>> allInputs;
	vector<int> instanceIds;
	
	collectAllInputs(allInputs, instanceIds);
	helper->setInputs(allInputs, instanceIds);
	
	if (!helper->checkInputs(rowid, colid)) return true;
	
	printInputs(allInputs);
	cout << "Col Id: " << colid << endl;
	suggestions.clear();
	
	bool sat = helper->checkSAT();
	if (sat || helper->ignoreConflict()) {
		helper->getControls(ctrlVals);
		const vector<tuple<int, int, int>>& s = helper->collectSuggestions(); // <instanceid, inputid, val>
		convertSuggestions(s, suggestions);
		return true;
	} else {
		cout << "****************CONFLICT****************" << endl;
		const vector<pair<int, int>>& c = helper->getConflicts(rowid, colid); // <instanceid, inputid>
		convertConflicts(c);
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


void NumericalSolver::collectAllInputs(vector<vector<int>>& allInputs, vector<int>& instanceIds) {
	for (int i = 0; i < inout->getNumInstances(); ++i) {
		vector<int> inputs;
		for (int j = 0; j < imap.size(); j++) {
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
		//cout << "Suggesting " << i << " " << j <<  " " << k << endl;
		suggestions.push(getLit(inout->valueid(i, j), v));
	}
}

void NumericalSolver::convertConflicts(const vector<pair<int, int>>& c) {
	for (int k = 0; k < c.size(); k++) {
		int i = c[k].first;
		int j = c[k].second;
		conflict.push(inout->valueid(i, j));
	}
}

// Debug with a fixed input and/or controls
void NumericalSolver::debug() {
	vector<vector<int>> allInputs;
	vector<int> instanceIds;
	vector<int> inputs;
	int arr[1000] = {0,0,0,0,0,1,2,2,1,0,1,2,2,1,1,0,0,2,2,2,2,2,1,0,1,0,0,2,1,2,1,0,0,0,2,2,2,2,2,2,2,2,2,2,2,0,0,0,1,2,2,1,2,2,2,2,2,1,1,2,1,1,1,1,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,0,1,2,2,1,2,2,2,2,2,1,1,0,1,0,0,2,2,2,2,2,0,1,0,1,1,2,1,2,2,2,2,2,2,2,2,2,2,2,2,0,1,2,2,1,2,2,2,2,2,1,1,2,1,2,2,2,2,2,2,2,2,2,2,2,0,1,2,2,1,2,2,2,2,2,1,1,2,1,1,1,1,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,1,2,1,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,0,1,2,2,1,2,2,2,2,2,1,1,0,1,0,0,2,2,2,2,2,0,1,0,1,1,2,1,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,0,1,2,2,1,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,0,1,2,2,1,2,2,2,2,2,1,1,2,1,1,2,1,1,0,2,2,0,0,1,2,2,1,0,1,0,2,2,0,1,0,1,0,0,0,0,0,0,0,1,2,2,1,2,2,2,2,2,1,2,2,2,2,2,2,2,2,2,2,2,1,2,1,1,0,1,1,0,2,2,0,0,1,2,2,1,0,1,0,2,2,0,1,0,1,0,0,0,0,0,0,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,0,0,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,0,1,2,2,1,2,2,2,2,2,1,0,1,2,2,1,2,2,2,2,2,1,1,1,1,2,2,2,2,2,2,2,2,2,2,2,1,0,2,2,0,2,2,2,2,2,0,2,0,2,1,2,1,0,1,2};
	
	for (int i = 0; i < imap.size(); i++) {
		if (arr[i] == 2) {
			inputs.push_back(EMPTY);
		} else {
			inputs.push_back(arr[i]);
		}
	}
	allInputs.push_back(inputs);
	instanceIds.push_back(0);
	int colid = 438;
	int rowid = 0;
	
	helper->setInputs(allInputs, instanceIds);
	
	if (!helper->checkInputs(rowid, colid)) return;
	
	printInputs(allInputs);
	cout << "Col Id: " << colid << endl;
	int ncontrols = 16;
	gsl_vector* s = gsl_vector_alloc(ncontrols);
	float arr1[16] = {2.3, 44.6, 9.7, 50.7, 1.8, 41.9, -3, 17.2704, 9.1, 56.8, 6.6, 44.3, -9.7, 8.4, 0.2, 42.5334};
	for (int i = 0; i < ncontrols; i++) {
		gsl_vector_set(s, i, arr1[i]);
	}
	//helper->autodiff(s, 0);
	
	//helper->checkSAT();
	
}
