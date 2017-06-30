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
	
#if RELAX_BOOL
	for (int i = 0; i < ctrls.size(); i++) {
		if (ctrls[i]->getOtype() == OutType::BOOL) {
			boolCtrlMap[ctrls[i]->get_name()] = ctrlVals.size();
			ctrlVals.push_back(0.0);
		}
	}
#endif
	
	ncontrols = ctrlVals.size();
	cout << "Ninputs: " << ninputs << " Ncontrols: " << ncontrols << endl;
	for (int i = 0; i < ninputs; i++) {
		cout << "Input " << i << ": " << (*dag)[imap[i]]->lprint() << endl;
	}
	if (ncontrols == 0) {
		ncontrols = 1;
		ctrlVals.push_back(0.0);
	}
	gd = new GradientDescent(ncontrols);
	
	evalR = new RangeDiff(*dag, fm, ctrlMap);
	evalG = new GlobalEvaluator(*dag, fm, ctrlMap);
	evalA = new AutoDiff(*dag, fm, ctrlMap, boolCtrlMap);
	eval = new SimpleEvaluator(*dag, fm, ctrlMap, boolCtrlMap);

	
	prevState = gsl_vector_alloc(ncontrols);
	//gsl_vector_set_zero(prevState);
	for (int i = 0; i < ncontrols; i++) {
		float r = -10.0 + (rand()%200)/10.0; // random number between 0 to 10.0  TODO: don't hardcode
		gsl_vector_set(prevState, i, r);
	}
	t = gsl_vector_alloc(ncontrols);
	minErrorSoFar = 1e50;
	
	GDEvaluator::init(ncontrols);
	GradUtil::tmp = gsl_vector_alloc(ncontrols);
	GradUtil::tmp1 = gsl_vector_alloc(ncontrols);
	GradUtil::tmp2 = gsl_vector_alloc(ncontrols);
	GradUtil::tmp3 = gsl_vector_alloc(ncontrols);
	GradUtil::tmpT = gsl_vector_alloc(ncontrols);

}

NumericalSolver::~NumericalSolver(void) {
	delete GradUtil::tmp;
	delete GradUtil::tmp1;
	delete GradUtil::tmp2;
	delete GradUtil::tmp3;
	delete GradUtil::tmpT;
}

double NumericalSolver::evalAuto(const gsl_vector* state, gsl_vector* d, const vector<vector<int>>& allInputs) {
	for (int i = 0; i < ncontrols; i++ ) {
		gsl_vector_set(d, i, 0);
	}
	double error = 0;
	for (int i = 0; i < allInputs.size(); i++) {
		map<int, int> inputValues; // maps node id to value set by the sat solver
		// Collect all inputs (assumes inputs are not floats)
		for (int j = 0; j < allInputs[i].size(); j++) {
			if (allInputs[i][j] != EMPTY) {
				inputValues[imap[j]] = allInputs[i][j];
			}
		}
		
		// Run automatic differentiation on ranges
		error += evalA->run(state, inputValues, d);
		//evalR->print();
	}
	//evalR->print();
	
	/*cout << "State: ";
	 for (int i = 0; i < ncontrols; i++) {
		cout << gsl_vector_get(state, i) << ", ";
	 }
	 cout << endl;
	 cout << "Error: " << error << endl;
	 cout << "Grad: ";
	 for (int i = 0; i < ncontrols; i++) {
		cout << gsl_vector_get(d, i) << ", ";
	 }
	 cout << endl;*/
	return error;
}

double NumericalSolver::evalRange(const gsl_vector* state, gsl_vector* d, const vector<vector<int>>& allInputs) {
	for (int i = 0; i < ncontrols; i++ ) {
		gsl_vector_set(d, i, 0);
	}
	double error = 0;
	for (int i = 0; i < allInputs.size(); i++) {
		map<int, int> inputValues; // maps node id to value set by the sat solver
		// Collect all inputs (assumes inputs are not floats)
		for (int j = 0; j < allInputs[i].size(); j++) {
			if (allInputs[i][j] != EMPTY) {
				inputValues[imap[j]] = allInputs[i][j];
			}
		}
		
		// Run automatic differentiation on ranges
		error += evalR->run(state, inputValues, d);
		//evalR->print();
	}
	//evalR->print();

	/*cout << "State: ";
	for (int i = 0; i < ncontrols; i++) {
		cout << gsl_vector_get(state, i) << ", ";
	}
	cout << endl;
	cout << "Error: " << error << endl;
	cout << "Grad: ";
	for (int i = 0; i < ncontrols; i++) {
		cout << gsl_vector_get(d, i) << ", ";
	}
	cout << endl;*/
	return error;
}

double NumericalSolver::simpleEval(const gsl_vector* state, const vector<vector<int>>& allInputs) {
	double error = 0;
	for (int i = 0; i < allInputs.size(); i++) {
		map<int, int> inputValues; // maps node id to value set by the sat solver
		// Collect all inputs (assumes inputs are not floats)
		for (int j = 0; j < allInputs[i].size(); j++) {
			if (allInputs[i][j] != EMPTY) {
				inputValues[imap[j]] = allInputs[i][j];
			}
		}
		
		// Run straight forward evaluator
		error += eval->run1(state, inputValues);
	}
	return error;
}

double NumericalSolver::evalAll(const gsl_vector* state, gsl_vector* d, const vector<vector<int>>& allInputs) {
	for (int i = 0; i < ncontrols; i++ ) {
		gsl_vector_set(d, i, 0);
	}
	double error = 0;
	for (int i = 0; i < allInputs.size(); i++) {
		map<int, int> inputValues; // maps node id to value set by the sat solver
		// Collect all inputs (assumes inputs are not floats)
		for (int j = 0; j < allInputs[i].size(); j++) {
			if (allInputs[i][j] != EMPTY) {
				inputValues[imap[j]] = allInputs[i][j];
			}
		}
		
		// Run automatic differentiation on ranges
		error += evalG->run(state, inputValues, d);
		//evalR->print();
	}
	//cout << "State: " << gsl_vector_get(state, 0) << " " << gsl_vector_get(state, 1) << " " << gsl_vector_get(state, 2) << endl;
	//cout << "Error: " << error << endl;
	//cout << "Grad: " << gsl_vector_get(d, 0) << " " << gsl_vector_get(d, 1) << " " << gsl_vector_get(d, 2) << endl;
	return error;
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
		numtries++;
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
		//for (int i = 0; i < ncontrols; i++) {
		//	float r = -10.0 + (rand()%200)/10.0; // random number between 0 to 10.0  TODO: don't hardcode
		//	gsl_vector_set(prevState, i, r);
		//}

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
	gsl_vector* ctrls = gsl_vector_alloc(ncontrols);
	gsl_vector_set_zero(d);
	for (int i = 0; i < allInputs.size(); i++) {
		for (int k = 0; k < ncontrols; k++) {
			gsl_vector_set(ctrls, k, ctrlVals[k]);
		}
		vector<tuple<float, int, int>> s = eval->run(ctrls, imap);
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
	conflict.clear();
	suggestions.clear();
	vector<vector<int>> allInputs;
	vector<int> conflictids;
	/*vector<int> inputs;
	int arr[13] = {1,2,2,0,0,0,0,0,0,1,1,1,1,};
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
	
	//gsl_vector* d = gsl_vector_alloc(ncontrols);
	//gsl_vector* state = gsl_vector_alloc(ncontrols);
	//gsl_vector_set(state, 0, 0);
	//cout << evalLocal(state, d, allInputs) << endl;
	//cout << simpleEval(state, allInputs) << endl;
	/*if (ncontrols == 1) {
		genData1D(allInputs);
	} else if (ncontrols == 2) {
		genData2D(allInputs);
	}*/
	
	gsl_vector* d = gsl_vector_alloc(ncontrols);
	gsl_vector* state = gsl_vector_alloc(ncontrols);

	/*double ctrls[6] = {0.9, 1.83662, 1.16875, 3.7, -8.1, 4.3};
	
	for (int i = 0; i < ncontrols; i++) {
		gsl_vector_set(prevState, i, ctrls[i]);
	}
	GradUtil::BETA = -100;
	GradUtil::ALPHA = 100;
	cout << evalLocal(state, d, allInputs) << endl;
	//cout << simpleEval(state, allInputs) << endl;*/
	
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
