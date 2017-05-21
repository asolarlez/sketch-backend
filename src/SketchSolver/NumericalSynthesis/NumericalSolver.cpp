#include "NumericalSolver.h"

gsl_vector* GDEvaluator::curGrad;

NumericalSolver::NumericalSolver(FloatManager& _fm, BooleanDAG* _dag, map<int, int>& _imap) :Synthesizer(_fm), dag(_dag), imap(_imap) {
	ninputs = imap.size();
	
	// generate ctrls mapping
	vector<bool_node*>& ctrls = dag->getNodesByType(bool_node::CTRL);
	for (int i = 0; i < ctrls.size(); i++) {
		if (ctrls[i]->getOtype() == OutType::FLOAT) {
			ctrlMap[ctrls[i]->get_name()] = ctrlVals.size();
			ctrlVals.push_back(0.0);
		}
	}
	ncontrols = ctrlVals.size();
	cout << "Ninputs: " << ninputs << " Ncontrols: " << ncontrols << endl;
	if (ncontrols == 0) ncontrols = 1;
	gd = new GradientDescent(ncontrols);
	
	evalR = new RangeDiff(*dag, fm, ctrlMap);
	eval = new SimpleEvaluator(*dag, fm);
	evalG = new GlobalEvaluator(*dag, fm, ctrlMap);
	
	prevState = gsl_vector_alloc(ncontrols);
	for (int i = 0; i < ncontrols; i++) {
		gsl_vector_set(prevState, i, 0);
	}
	t = gsl_vector_alloc(ncontrols);
	minErrorSoFar = 1e50;
	
	GDEvaluator::init(ncontrols);
}


double NumericalSolver::evalLocal(const gsl_vector* state, gsl_vector* d, const vector<vector<int>>& allInputs) {
	for (int i = 0; i < ncontrols; i++ ) {
		gsl_vector_set(d, i, 0);
	}
	double error = 0;
	for (int i = 0; i < allInputs.size(); i++) {
		VarStore ctrlStore;
		map<int, int> inputValues; // maps node id to value set by the sat solver
		// Collect all inputs (assumes inputs are not floats)
		for (int j = 0; j < allInputs[i].size(); j++) {
			if (allInputs[i][j] != EMPTY) {
				inputValues[imap[j]] = allInputs[i][j];
			}
		}
		// Collect all controls (assumes controls are floats)
		for (auto it = ctrlMap.begin(); it != ctrlMap.end(); it++) {
			ctrlStore.setVarVal(it->first, fm.getIdx(gsl_vector_get(state,it->second)));
		}
		
		// Run automatic differentiation on ranges
		error += evalR->run(ctrlStore, inputValues, d);
		//evalR->print();
	}
	//cout << "State: " << gsl_vector_get(state, 0) << " " << gsl_vector_get(state, 1) << " " << gsl_vector_get(state, 2) << endl;
	//cout << "Error: " << error << endl;
	//cout << "Grad: " << gsl_vector_get(d, 0) << " " << gsl_vector_get(d, 1) << " " << gsl_vector_get(d, 2) << endl;
	return error;
}

double NumericalSolver::simpleEval(const gsl_vector* state, const vector<vector<int>>& allInputs) {
	double error = 0;
	for (int i = 0; i < allInputs.size(); i++) {
		VarStore ctrlStore;
		map<int, int> inputValues; // maps node id to value set by the sat solver
		// Collect all inputs (assumes inputs are not floats)
		for (int j = 0; j < allInputs[i].size(); j++) {
			if (allInputs[i][j] != EMPTY) {
				inputValues[imap[j]] = allInputs[i][j];
			}
		}
		// Collect all controls (assumes controls are floats)
		for (auto it = ctrlMap.begin(); it != ctrlMap.end(); it++) {
			ctrlStore.setVarVal(it->first, fm.getIdx(gsl_vector_get(state,it->second)));
		}
		
		// Run automatic differentiation on ranges
		error += eval->run1(ctrlStore, inputValues);
	}
	return error;
}

double NumericalSolver::evalAll(const gsl_vector* state, gsl_vector* d, const vector<vector<int>>& allInputs) {
	for (int i = 0; i < ncontrols; i++ ) {
		gsl_vector_set(d, i, 0);
	}
	double error = 0;
	for (int i = 0; i < allInputs.size(); i++) {
		VarStore ctrlStore;
		map<int, int> inputValues; // maps node id to value set by the sat solver
		// Collect all inputs (assumes inputs are not floats)
		for (int j = 0; j < allInputs[i].size(); j++) {
			if (allInputs[i][j] != EMPTY) {
				inputValues[imap[j]] = allInputs[i][j];
			}
		}
		// Collect all controls (assumes controls are floats)
		for (auto it = ctrlMap.begin(); it != ctrlMap.end(); it++) {
			ctrlStore.setVarVal(it->first, fm.getIdx(gsl_vector_get(state,it->second)));
		}
		
		// Run automatic differentiation on ranges
		error += evalG->run(ctrlStore, inputValues, d, state);
		//evalR->print();
	}
	//cout << "State: " << gsl_vector_get(state, 0) << " " << gsl_vector_get(state, 1) << " " << gsl_vector_get(state, 2) << endl;
	//cout << "Error: " << error << endl;
	//cout << "Grad: " << gsl_vector_get(d, 0) << " " << gsl_vector_get(d, 1) << " " << gsl_vector_get(d, 2) << endl;
	return error;
}

bool NumericalSolver::doIntervalProp(int instance, int inputid, int val, int level) {
	IntervalPropagator* iprop = propMap[instance];
	bool_node* node = getNodeForInput(inputid);
	float fval = (node->getOtype() == OutType::FLOAT) ? fm.getFloat(val) : (float) val;
#if IP_DEBUG
	cout << "IP: " << node->lprint() << " " << fval << endl;
#endif
	bool success = iprop->setInterval(*node, fval, fval, level);
	if (!success) {
		vector<bool_node*>& conflictNodes = iprop->conflictNodes;
#if IP_DEBUG
		cout << "IP conflict nodes" << endl;
		cout << conflictNodes.size() << endl;
#endif
		for (int i = 0 ; i < conflictNodes.size(); i++) {
			int iid = getInputForNode(conflictNodes[i]);
			if (iid >= 0) {
#if IP_DEBUG
				cout << conflictNodes[i]->lprint() << " "  << iid << endl;
#endif
				conflict.push(inout->valueid(instance, iid));
			}
		}
		cout << "***** " << "CONFLICT (IP)" << " ******" << endl;
		return false;
	}
	return true;
	
	// TODO: we can intersect the intervals for the ctrl nodes for all instances of interval propagators to detect further conflicts
	// TODO: use the intervals for ctrl nodes during the gradient descent
}

bool NumericalSolver::doGradientDescent(const vector<vector<int>>& allInputs, const vector<int>& conflictids, int instance, int inputid) {
	GDParameters* p = new GDParameters();
	p->allInputs = allInputs;
	p->ns = this;
	for (int i = 0; i < ncontrols; i++) {
		cout << gsl_vector_get(prevState, i) << ", ";
	}
	cout << endl;
	gd->init(GDEvaluator::f, GDEvaluator::df, GDEvaluator::fdf, p, prevState);
	double minError = gd->optimize();
	gsl_vector* curState = gd->getResults();
	if (minError < minErrorSoFar) {
		minErrorSoFar = minError;
		for (int i = 0; i < ncontrols; i++) {
			ctrlVals[i] = gsl_vector_get(curState, i);
		}
	}
	int numtries = 0;
	while (minError > threshold  && numtries < MAX_TRIES) {
		cout << "Retry attempt: " << (numtries+1) << endl;
		// redo gradient descent with random initial point
		for (int i = 0; i < ncontrols; i++) {
			float r = -10.0 + (rand()%200)/10.0; // random number between 0 to 10.0  TODO: don't hardcode
			gsl_vector_set(t, i, r);
			cout << r << ", ";
		}
		cout << endl;
		
		gd->init(GDEvaluator::f, GDEvaluator::df, GDEvaluator::fdf, p, t);
		minError = gd->optimize();
		curState = gd->getResults();
		if (minError < minErrorSoFar) {
			minErrorSoFar = minError;
			for (int i = 0; i < ncontrols; i++) {
				ctrlVals[i] = gsl_vector_get(curState, i);
			}
		}
		numtries++;
	}
	if (minError < minErrorSoFar) {
		minErrorSoFar = minError;
		for (int i = 0; i < ncontrols; i++) {
			ctrlVals[i] = gsl_vector_get(curState, i);
		}
	}
	//evalR->print();
	if (minError <= threshold) {
		prevState = curState;
		// Update the controls
		for (int i = 0; i < ncontrols; i++) {
			ctrlVals[i] = gsl_vector_get(curState, i);
		}
		return true;
	} else {
		cout << "******************* Found a conflict *******************" << endl;
		generateConflict(conflictids);
		for (int i = 0; i < ncontrols; i++) {
			float r = -10.0 + (rand()%200)/10.0; // random number between 0 to 10.0  TODO: don't hardcode
			gsl_vector_set(prevState, i, r);
		}

		return false;
	}
}

void NumericalSolver::generateConflict(const vector<int>& conflictids) {
	for(auto conflictId: conflictids) {
		for (int j = 0; j < ninputs; j++) {
			if (inout->getVal(conflictId, j) != EMPTY) {
				conflict.push(inout->valueid(conflictId, j));
			}
		}
	}
}

void NumericalSolver::collectSuggestions(vec<Lit>& suggestions, const vector<vector<int>>& allInputs, const vector<int>& conflictids) {
	gsl_vector* d = gsl_vector_alloc(ncontrols);
	for (int i = 0; i < ncontrols; i++) {
		gsl_vector_set(d, i, 0.0);
	}
	for (int i = 0; i < allInputs.size(); i++) {
		VarStore ctrlStore;
		// Collect all controls (assumes controls are floats)
		for (auto it = ctrlMap.begin(); it != ctrlMap.end(); it++) {
			ctrlStore.setVarVal(it->first, fm.getIdx(ctrlVals[it->second]));
		}
		vector<tuple<float, int, int>> s = eval->run(ctrlStore, imap);
		sort(s.begin(), s.end());
		reverse(s.begin(), s.end());
		for (int k = 0; k < s.size(); k++) {
			int idx = get<1>(s[k]);
			if (allInputs[i][idx] == EMPTY) {
				//cout << "Suggesting " << idx << " " << get<2>(s[k]) <<  " " << get<0>(s[k]) << endl;
				suggestions.push(getLit(inout->valueid(conflictids[i], idx), get<2>(s[k])));
			}
		}
	}
}

bool NumericalSolver::synthesis(int instance, int inputid, int val, int level, vec<Lit>& suggestions) {
	if (onlyOptimize){ // stop the solver after some retries
		if (counter >= 3) {
			cout << "Min error: " << minErrorSoFar << endl;
			return true;
		} else {
			counter++;
		}
	}
	conflict.clear();
	suggestions.clear();
	vector<vector<int>> allInputs;
	vector<int> conflictids;
	/*vector<int> inputs;
		int arr[100] = {1,2,2,2,2,0,2,2,2,2,0,2,2,2,2,0,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,0};
		for (int i = 0; i < ninputs; i++) {
	 if (arr[i] == 2) {
	 inputs.push_back(EMPTY);
	 } else {
	 inputs.push_back(arr[i]);
	 }
		}
		allInputs.push_back(inputs);*/
	collectAllInputs(allInputs, conflictids);
	if (allInputs.size() == 0) return true;
	printInputs(allInputs);
	
	/*gsl_vector* d = gsl_vector_alloc(ncontrols);
		gsl_vector* state = gsl_vector_alloc(ncontrols);
		gsl_vector_set(state, 0, 0);
		gsl_vector_set(state, 1, 0.5);
		cout << evalLocal(state, d, allInputs) << endl;*/
	//genData(allInputs);
	// First, do interval propagation to detect any conflicts
	//if (!doIntervalProp(instance, inputid, val, level)) {
	//		return false;
	//}
	// If no conflict is found, do gradient descent
	/*double ctrls[72] = {0.5, 6.6, -6.3, 5.5, -1.2, -6.6, 6.6, 1.7, -4.2, 4, 4.3, 2.3, -5.8, -5.2, -8.7, -1.6, -0.3, 9.1, 7.8, -7, 2.2, -6.6, -9.8, 3.4, 6, 0.4, 2.8, 0.9, 4.1, 0.9, 5.4, -0.7, 5.8, 1.2, -6.5, 8.1, -6.6, -7.8, 9.6, 7.7, 2.6, -4.1, -8.9, 5, 8.4, -0.9, 6.3, -9.9, -6.4, -4.6, 1.1, -2.2, -9, 2.3, 4.6, -8.7, -5.3, 5.7, -9.3, -7.4, -7.7, 1.1, 0, 5.1, 9, 4.4, -2.3, -4.1, 2.2, -3.1, 1, -0.6};
	
	for (int i = 0; i < ncontrols; i++) {
		gsl_vector_set(prevState, i, ctrls[i]);
	}*/
	
	bool sat = doGradientDescent(allInputs, conflictids, instance, inputid);
	if (sat) {
		collectSuggestions(suggestions, allInputs, conflictids);
	}
	return sat;
}

IntervalPropagator* NumericalSolver::createPropagator() {
	IntervalPropagator* iprop = new IntervalPropagator(*dag);
	
	// Initialize constants, assertions and ctrl nodes
	for (int i = 0; i < dag->size(); i++) {
		bool_node* n = (*dag)[i];
		if (n->type == bool_node::CONST) {
			float fval;
			if (n->getOtype() == OutType::FLOAT) {
				fval = ((CONST_node*) n)->getFval();
			} else {
				fval = ((CONST_node*) n)->getVal();
			}
			Assert(iprop->setInterval(*n, fval, fval, 0), "Setting constant interval failed");
		}
		if(n->type == bool_node::ASSERT) {
			// set the range of mother to 1
			Assert(iprop->setInterval(*n, 1, 1, 0), "Setting assert failed");
		}
		if (n->type == bool_node::CTRL) {
			if (n->getOtype() == OutType::FLOAT) {
				Assert(iprop->setInterval(*n, -32.0, 32.0, 0), "Setting ctrl range failed"); // TODO: Don't hardcode the range here
			}
		}
	}
	iprop->processAllNodes();
	return iprop;
}

bool_node* NumericalSolver::getExpression(DagOptim* dopt, const vector<bool_node*>& params) {
	// Add the appropriate expression from the dag after replacing inputs with params and ctrls with synthesized parameters
	BooleanDAG newdag = *(dag->clone());
	for (int i = 0; i < newdag.size(); i++) {
		if (newdag[i]->type == bool_node::CTRL) {
			// TODO: what to do with non float ctrls that are solved by the SAT solver??
			newdag.replace(i, dopt->getCnode(ctrlVals[ctrlMap[newdag[i]->get_name()]]));
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
