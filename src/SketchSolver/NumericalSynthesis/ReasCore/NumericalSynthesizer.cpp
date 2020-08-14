#include "NumericalSynthesizer.h"
#include <unordered_set>

gsl_vector* GDEvaluator::curGrad;
gsl_vector* GDEvaluator::grad;
ofstream GDEvaluator::file;
gsl_vector* SnoptEvaluator::state;
gsl_vector* SnoptEvaluator::grad;
ofstream SnoptEvaluator::file;
double SnoptEvaluator::prevVal;
int SnoptEvaluator::prevCount;
gsl_vector* MaxSnoptEvaluator::state;
gsl_vector* MaxSnoptEvaluator::grad;
ofstream MaxSnoptEvaluator::file;
double MaxSnoptEvaluator::prevVal;

gsl_vector* MaxEvaluator::state;
gsl_vector* MaxEvaluator::grad;
ofstream MaxEvaluator::file;

int GradUtil::counter;

using namespace std;

NumericalSynthesizer::NumericalSynthesizer(FloatManager& _fm, BooleanDAG* _dag, Interface* _interface) : dag(_dag), interf(_interface) {
	for (BooleanDAG::iterator node_it = dag->begin(); node_it != dag->end(); ++node_it) {
		bool_node* n = *node_it;
		set<int> ctrls;
		
		if (n->type == bool_node::CTRL && n->getOtype() == OutType::FLOAT) {
			ctrls.insert(n->id);
		}
		for (auto it = n->p_begin(); it != n->p_end(); ++it) {
			bool_node* parent = *it;
			vector<int>& parentCtrls = dependentCtrls[parent->id];
			ctrls.insert(parentCtrls.begin(), parentCtrls.end());
		}
		dependentCtrls.push_back(vector<int>(ctrls.begin(), ctrls.end()));
	}

	if (PARAMS->verbosity > 2) {
		cout << "NInputs: " << interf->size() << endl;
	}

	vector<bool_node*>& ctrlNodes = dag->getNodesByType(bool_node::CTRL);
	int ctr = 0;
	for (int i = 0; i < ctrlNodes.size(); i++) {
		if (ctrlNodes[i]->getOtype() == OutType::FLOAT) {
			ctrls[ctrlNodes[i]->get_name()] = ctr++;
		}
	}
	for (int i = 0; i < ctrlNodes.size(); i++) {
		if (ctrlNodes[i]->getOtype() == OutType::BOOL) {
			ctrls[ctrlNodes[i]->get_name()] = ctr++;
		}
	}
	int ncontrols = ctrls.size();
	// if ncontrols = 0, make it 1 just so numerical opt does not break
	if (ncontrols == 0) {
		ncontrols = 1;
	}
	state = gsl_vector_alloc(ncontrols);
	prevState = gsl_vector_alloc(ncontrols);
		
	doublereal* xlow = new doublereal[ncontrols];
	doublereal* xupp = new doublereal[ncontrols];
	xlow[0] = -20; 
	xupp[0] = 20;
	for (auto i = 0; i < ctrlNodes.size(); i++) {
		CTRL_node* cnode = (CTRL_node*)ctrlNodes[i];
		if (cnode->getOtype() == OutType::FLOAT) {
			int idx = ctrls[cnode->get_name()];
			xlow[idx] = cnode->hasRange ? cnode->low : -20.0;
			xupp[idx] = cnode->hasRange ? cnode->high : 20.0;
		}
		else if (cnode->getOtype() == OutType::BOOL) {
			int idx = ctrls[cnode->get_name()];
			xlow[idx] = 0;
			xupp[idx] = 1;
		}
	}

	int numConstraints = 0;
	for (int i = 0; i < dag->size(); i++) {
		bool_node* n = (*dag)[i];
		if (n->type == bool_node::ASSERT || Util::isSqrt(n)) {
			numConstraints++;
		}
		if (n->type == bool_node::CTRL && n->getOtype() == OutType::BOOL) {
			numConstraints++;
		}
	}
	numConstraints += 1000; // TODO: magic number

	SmoothEvaluators* smoothEval = new SmoothEvaluators(dag, interf, ctrls, ncontrols);
	smoothEval->addEvaluator(dag);

	ActualEvaluators* actualEval = new ActualEvaluators(dag, interf, ctrls);
	actualEval->addEvaluator(dag);

	OptimizationWrapper* opt;
	if (PARAMS->optMode == 0 || PARAMS->optMode == 2) {
		opt = new SnoptWrapper(smoothEval, ncontrols, xlow, xupp, numConstraints, PARAMS->optMode == 0);
	}
	else {
#ifndef _NOGSL
		opt = new GradientDescentWrapper(smoothEval, ncontrols, xlow, xupp);
#else
		opt = NULL;
#endif
	}

	MaxOptimizationWrapper* maxOpt = new MaxSolverWrapper(smoothEval, ncontrols, xlow, xupp, numConstraints);

	for (BooleanDAG::iterator node_it = dag->begin(); node_it != dag->end(); ++node_it) {
		bool_node* n = *node_it;
		set<int> inputs;
		
		for (auto it = n->p_begin(); it != n->p_end(); ++it) {
			bool_node* parent = *it;
			if (interf->isInput(parent->id)) {
				inputs.insert(parent->id);
			}
			else {
				vector<int>& parentInputs = dependentInputs[parent->id];
				inputs.insert(parentInputs.begin(), parentInputs.end());
			}
		}
		dependentInputs.push_back(vector<int>(inputs.begin(), inputs.end()));
	}

	for (auto it = interf->levelPredicates[0].begin(); it != interf->levelPredicates[0].end(); it++) {
		Predicate* p = *it;
		if (p->isBasic()) {
			BasicPredicate* bp = (BasicPredicate*)(p);
			if (dependentInputs[bp->nid].size() > 0) {
				bp->makeImpure();
			}
		}
		else if (p->isDiff()) {
			DiffPredicate* dp = (DiffPredicate*)(p);
			if (!dp->p1->isPure() || !dp->p2->isPure()) {
				dp->makeImpure();
			}
		}
	}
	// Need to do this twice because predicates maynot be topologically ordered
	for (auto it = interf->levelPredicates[0].begin(); it != interf->levelPredicates[0].end(); it++) {
		Predicate* p = *it;
		if (p->isDiff()) {
			DiffPredicate* dp = (DiffPredicate*)(p);
			if (!dp->p1->isPure() || !dp->p2->isPure()) {
				dp->makeImpure();
			}
		}
	}



	for (auto it = interf->levelPredicates[0].begin(); it != interf->levelPredicates[0].end(); it++) {
		cout << (*it)->print();
		if ((*it)->isPure()) {
			cout << " PURE" << endl;
		}
		else {
			cout << " IMPURE" << endl;
		}
	}


	debugger = new NumDebugger(dag, ctrls, interf, smoothEval, actualEval, opt);

	//sg = new SimpleSuggestionGenerator(dag, interf, ctrls);
	sg = new SuggestionGeneratorUsingMax(dag, _fm, interf, ctrls, opt, maxOpt, actualEval, smoothEval, ncontrols, dependentInputs, debugger);



	solver = new NumericalSolver(dag, ctrls, interf, smoothEval, actualEval, opt, dependentInputs, dependentCtrls, debugger);

	basicSampler = new BasicSampler(dag, interf, actualEval, ncontrols, xlow, xupp);
	boolBasedSampler = new BoolBasedSampler(dag, interf, actualEval, ncontrols, xlow, xupp, dependentInputs);

	//debugger->plotGraphs();
	//exit(0);
	if (PARAMS->numdebug) {
		//debugger->plotLinePredicate2d();
		//exit(0);
		//debugger->getPredicatesGraphs();
		//debugger->getGraphs(-1, 0);
		//MaxOptimizationWrapper* gd = new GradientDescentWrapper(smoothEval, ncontrols, xlow, xupp);
		//MaxOptimizationWrapper* snopt = new SnoptWrapper(smoothEval, ncontrols, xlow, xupp, numConstraints, true);
		//MaxOptimizationWrapper* custom = new MaxSolverWrapper(smoothEval, ncontrols, xlow, xupp, numConstraints);
		//debugger->runMaxAnalysis(snopt);
		//debugger->runOptimization2D();
		//debugger->checkWithValidSolutions();
		//exit(0);
		//debugger->checkSmoothing();
		//debugger->getPredicatesGraphs();
	}
}

bool NumericalSynthesizer::solve() {
	bool success = search();
	return success;
}

bool NumericalSynthesizer::searchWithOnlySmoothing() {
	bool sat = false;
	int counter = 0;
	while (true) {
		/*float arr[4] = {5.0, 3.0, 9.0, 2.0};
		for (int j = 0; j < state->size; j++) {
		gsl_vector_set(state, j, arr[j]);
		}*/
		basicSampler->sampleState(state);
		((BoolBasedSampler*)boolBasedSampler)->analyze(state);
		sat = solver->checkSAT(-1, state);
		gsl_vector_memcpy(state, solver->getResult());
		((BoolBasedSampler*)boolBasedSampler)->analyze(state);

		//debugger->distToSols(solver->getLocalState()->localSols[2]);
		//counter++;
		//if (counter >= 1) {
		//    break;
		//}
		if (sat) {
			cout << "Found solution" << endl;
			if (concretize()) {
				return true;
			}
		}
		GradUtil::counter++;
	}
	return false;
}

bool NumericalSynthesizer::searchWithBoolBasedSampling() {
	GradUtil::counter = 0;
	bool sat = false;

	while (true) {
		boolBasedSampler->sampleState(state);

		sat = solver->checkSAT(-1, state);
		gsl_vector_memcpy(prevState, state);
		gsl_vector_memcpy(state, solver->getResult());
		if (sat) {
			gsl_vector_memcpy(state, solver->getResult());

			cout << "Found solution" << endl;
			if (concretize()) {
				return true;
			}
		}
		else {
			cout << "Unsat" << endl;
			SClause* s = sg->getUnsatClause(state, prevState);
			cout << "Suggested clause: " << s->print() << endl;
			((BoolBasedSampler*)boolBasedSampler)->analyze(prevState, s);
			((BoolBasedSampler*)boolBasedSampler)->analyze(state, s);
			boolBasedSampler->addClause(s);
			GradUtil::counter++;
		}
	}
}


bool NumericalSynthesizer::searchWithBooleans() {
	GradUtil::counter = 0;
	bool sat = true;
	bool first = true;
	while (true) {
		if (GradUtil::counter >= 100) {
			return false;
		}
		sat = solver->checkSAT(-1, first ? NULL : state);
		first = false;
		gsl_vector_memcpy(state, solver->getResult());

		if (sat) {
			cout << "Found solution" << endl;
			if (concretize()) {
				return true;
			}
		}
		cout << "Unsat" << endl;
		const pair<int, int>& nodeVal = sg->getUnsatSuggestion(state);
		cout << "Suggested bool: " << nodeVal.first << " " << nodeVal.second << endl;
		interf->setInput(nodeVal.first, nodeVal.second);
		GradUtil::counter++;
	}
}

bool NumericalSynthesizer::searchWithPredicates() {
	GradUtil::counter = 0;
	int cc = 0;
	while (true) {
		//if (cc >= 1) {
		//    exit(0);
		//}
		int num_levels = interf->numLevels();
		bool sat = true;
		bool first = true;
		int level;
		int num_ignored = 0;
		for (level = num_levels - 1; level >= -1; level--) {
			sat = solver->checkSAT(level, first ? NULL : state);
			first = false;
			if (!sat) break;
			gsl_vector_memcpy(state, solver->getResult());
		}

		if (sat) {
			cout << "Found solution" << endl;
			if (concretize()) { // This may change the local state of the solver
				return true;
			}
			level = -1;
		}
		cout << "Unsat in level " << level << endl;
		debugger->distToSols(solver->getLocalState()->localSols[2]);
		if (level >= 0) {
			num_ignored++;
			cout << "Ignoring" << endl;
			if (num_ignored > 5) {
				cout << "TOO MANY UNSATS IN LEVEL 0" << endl;
			}
		}
		else {
			cc++;
			num_ignored = 0;
			IClause* c = sg->getConflictClause(level, solver->getLocalState());
			cout << "Suggested clause: " << c->print() << endl;
			debugger->checkWithValidSolutions(c);
			if (PARAMS->numdebug) {
				debugger->getGraphForClause(c, GradUtil::counter == 0);
			}
			interf->addClause(c, level + 1);
		}
		interf->removeOldClauses();
		GradUtil::counter++;
	}
}

bool NumericalSynthesizer::search() {
	if (PARAMS->numericalSolverMode == "ONLY_SMOOTHING") {
		return searchWithOnlySmoothing();
	}
	else if (PARAMS->numericalSolverMode == "SMOOTHING_SAT") {
		return searchWithBoolBasedSampling();
	}
	Assert(false, "NYI for solver mode " + PARAMS->numericalSolverMode);
	return true;
}

bool NumericalSynthesizer::simple_concretize() {
	return solver->checkFullSAT(state);
}

bool NumericalSynthesizer::search_concretize() {
	if (solver->checkFullSAT(state)) {
		return true;
	}
	const pair<int, int>& nodeVal = sg->getSuggestion(state);
	cout << "Suggested concretization: " << nodeVal.first << " " << nodeVal.second << endl;
	interf->setInput(nodeVal.first, nodeVal.second);
	int i = 0;
	while (true) {
		GradUtil::counter++;
		bool sat = solver->checkSAT(-1, state);
		if (sat) {
			gsl_vector_memcpy(state, solver->getResult());
			if (i > 30) {
				return true;
			}
			if (solver->checkFullSAT(state)) {
				return true;
			}
			else {
				const pair<int, int>& nodeVal = sg->getSuggestion(state);
				cout << "Suggested concretization: " << nodeVal.first << " " << nodeVal.second << endl;
				interf->setInput(nodeVal.first, nodeVal.second);
			}
		}
		else {
			cout << "CONFLICT" << endl;
			bool r = interf->clearLastLevel();
			if (!r) return false;
		}
		i++;
	}
}



bool NumericalSynthesizer::concretize() {
	//return search_concretize();
	solver->checkFullSAT(state);
	return true;
	//return simple_concretize();
}
