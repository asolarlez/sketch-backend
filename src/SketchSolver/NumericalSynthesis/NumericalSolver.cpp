#include "NumericalSolver.h"

gsl_vector* GDEvaluator::curGrad;
gsl_vector* SnoptEvaluator::state;
gsl_vector* SnoptEvaluator::grad;

NumericalSolver::NumericalSolver(FloatManager& _fm, BooleanDAG* _dag, map<int, int>& _imap): Synthesizer(_fm), dag(_dag), imap(_imap) {
	cout << "NInputs: " << imap.size() << endl;
	helper = new BoolApproxHelper(_fm, dag, imap);
	//helper = new InequalityHelper(_fm, dag, imap);
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


void NumericalSolver::getConstraintsOnInputs(SolverHelper* dir, vector<Tvalue>& inputs) {
	return;
	map<string, multimap<double, int>> ctrlToInputIds;
	
	// First, group inputs based on ctrl names
	for (int i = 0; i < inputs.size(); i++) {
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
	cout << ctrlToInputIds.size() << endl;
	// Next, generate constraints for each pair of inputs that have the same ctrl
	for (auto it = ctrlToInputIds.begin(); it != ctrlToInputIds.end(); it++) {
		auto& ids_map = it->second;
		vector<int> ids;
		for (auto id_it = ids_map.begin(); id_it != ids_map.end(); id_it++) {
			cout << id_it->first << ", ";
			ids.push_back(id_it->second);
		}
		cout << endl;
		cout << ids.size() << endl;
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
					cout << "Adding equate clause 1" << endl;
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
					cout << "Adding equate clause 2" << endl;
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
					cout << "Adding equate clause 3" << endl;
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
					cout << "Adding equate clause 4" << endl;
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
	int arr[1] = {1};
	for (int i = 0; i < imap.size(); i++) {
		if (arr[i] == 2) {
			inputs.push_back(EMPTY);
		} else {
			inputs.push_back(arr[i]);
		}
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
	
	SymbolicEvaluator* eval = new BoolAutoDiff(*dag, fm, ctrlMap);
	const map<int, int>& nodeValsMap = Util::getNodeToValMap(imap, allInputs[0]);
	gsl_vector* s = gsl_vector_alloc(ncontrols);
	double arr1[6] = {-2.17617, 11.673, -4.33473, 6.59534, -3.55217, 0.644097};
	for (int i = 0; i < ncontrols; i++) {
		gsl_vector_set(s, i, arr1[i]);
	}
	for (int i = 0; i < ncontrols; i++) {
		genData(s, i, eval, nodeValsMap);
		gsl_vector_set(s, i, arr1[i]);
	}
}

/*
// Debugging: prints out the data to generate graphs
void NumericalSolver::genData2D(int ncontrols) {
	GradUtil::BETA = -10;
	GradUtil::ALPHA = 10;
	gsl_vector* d = gsl_vector_alloc(ncontrols);
	gsl_vector* state = gsl_vector_alloc(ncontrols);
	ofstream file("/Users/Jeevu/projects/symdiff/scripts/graphs/sysid/g.txt");
	{
		double i = 0.0;
		double j = 0.0;
		while (i < 10.0) {
			j = 0.0;
			while (j < 10.0) {
				gsl_vector_set(state, 0, i);
				gsl_vector_set(state, 1, j);
				double err = helper->evalGD(state, d);
				cout << i << " " << j << " " << err << " " << gsl_vector_get(d, 0) << " " << gsl_vector_get(d, 1) << endl;
				file << err << ";";
				j+=0.1;
			}
			i += 0.1;
		}
	}
	file << endl;
}

void NumericalSolver::genData1D(int ncontrols) {
	GradUtil::BETA = -10;
	GradUtil::ALPHA = 10;
	gsl_vector* d = gsl_vector_alloc(ncontrols);
	gsl_vector* state = gsl_vector_alloc(ncontrols);
	ofstream file("/Users/Jeevu/projects/symdiff/scripts/graphs/sysid/g.txt");
	{
		double i = 0.0;
		while (i < 10.0) {
			gsl_vector_set(state, 0, i);
			double err = helper->evalGD(state, d);
			cout << i << " " << err << endl;
			file << err << ";";
			i += 0.01;
		}
	}
	file << endl;
}*/

void NumericalSolver::genData(gsl_vector* state, int idx, SymbolicEvaluator* eval, const map<int, int>& nodeValsMap) {
	GradUtil::BETA = -10;
	GradUtil::ALPHA = 10;
	gsl_vector* d = gsl_vector_alloc(state->size);
	
	ofstream file("/Users/Jeevu/projects/symdiff/scripts/graphs/sysid/g" + to_string(idx) + ".txt");
	{
		double i = -100.0;
		while (i < 100.0) {
			gsl_vector_set(state, idx, i);
			eval->run(state, nodeValsMap);
			gsl_vector_set_zero(d);
			double err = 0.0;
			for (BooleanDAG::iterator node_it = dag->begin(); node_it != dag->end(); node_it++) {
				bool_node* node = *node_it;
				if (node->type == bool_node::ASSERT) {
					double dist = eval->computeDist(node->mother, d);
					if (!eval->hasDist(node->mother)) {
						dist = 0;
					}
					err += dist;
				}
			}
			cout << i << " " << err << " " << gsl_vector_get(d, 0) << endl;
			file << err << ";";
			i += 0.1;
		}
	}
	file << endl;
}
