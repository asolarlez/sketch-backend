#include "NumericalSolver.h"


int SnoptEvaluator::counter;

NumericalSolver::NumericalSolver(BooleanDAG* _dag, map<string, int>& _ctrls, Interface* _interface, SymbolicEvaluator* _eval, OptimizationWrapper* _opt, const vector<vector<int>>& _dependentInputs, const vector<vector<int>>& _dependentCtrls, NumDebugger* _debugger): dag(_dag), ctrls(_ctrls), interf(_interface), eval(_eval), opt(_opt), dependentInputs(_dependentInputs), dependentCtrls(_dependentCtrls), debugger(_debugger) {
	ncontrols = ctrls.size();
    // if ncontrols = 0, make it 1 just so numerical opt does not break
	if (ncontrols == 0) {
		ncontrols = 1;
	}
	
	cout << "NControls: " << ncontrols << endl;
    
    GradUtil::allocateTempVectors(ncontrols);
	localState = new LocalState(ncontrols, 3);
    seval = new SimpleEvaluator(*dag, ctrls);
    minimizeNode = -1;

    GradUtil::counter = 0;
    
    for (int i = 0; i < dag->size(); i++) { // TODO: this should also be set by caller class
        bool_node* n = (*dag)[i];
        if (n->type == bool_node::ASSERT && ((ASSERT_node*)n)->isHard()) {
            minimizeNode = i;
        } else if (n->type == bool_node::ASSERT || Util::isSqrt(n)) {
            assertConstraints.insert(i);
        } else if (n->type == bool_node::CTRL && n->getOtype() == OutType::BOOL) {
            assertConstraints.insert(i);
        }
    }
}

NumericalSolver::~NumericalSolver(void) {
    GradUtil::clearTempVectors();
	delete(localState);
}

bool NumericalSolver::checkSAT(gsl_vector* initState) {
    cout << "Check SAT" << endl;
    interf->printInputs();
    if (initState != NULL) {
        cout << "Init state: " << Util::print(initState) << endl;
    }
    bool suppressPrint = PARAMS->verbosity > 7 ? false : true;
    bool sat = false;
    if (initState != NULL && checkCurrentSol(initState)) {
        cout << "Current sol passed" << endl;
        result = opt->getMinState();
        return true;
    } 
    if (PARAMS->numdebug) { 
        debugger->getGraphs(GradUtil::counter);
    }
    cout << "Running optimization " << GradUtil::counter << endl;
    set<int> allConstraints;
    allConstraints.insert(assertConstraints.begin(), assertConstraints.end());
    const set<int>& inputConstraints = interf->getInputConstraints();
    allConstraints.insert(inputConstraints.begin(), inputConstraints.end());
    //printGraphCmd("before");
    sat = opt->optimize(interf, initState, allConstraints, minimizeNode, suppressPrint, PARAMS->numTries, localState);
    GradUtil::counter++;
    if (sat) {
        result = opt->getMinState();
        cout << "Found solution" << endl;
        cout << Util::print(result) << endl;
        return true;
    } else {
        cout << "Local solution" << endl;
        localState->print();
        return false;
    }
}

bool NumericalSolver::checkCurrentSol(gsl_vector* state) {
    cout << "Checking current solution" << endl;
    GradUtil::BETA = -50; // TODO: magic numbers
    GradUtil::ALPHA = 50;
    eval->setInputs(interf); // TODO: since this is a pointer, we don't have to set it everytime
    eval->run(state);
    cout << Util::print(state) << endl;
    
    set<int> allConstraints; // TODO: this could be further optimized to check only those constraints whose eval changed since last iteration
    allConstraints.insert(assertConstraints.begin(), assertConstraints.end());
    const set<int>& inputConstraints = interf->getInputConstraints();
    allConstraints.insert(inputConstraints.begin(), inputConstraints.end());
    
    double error;
    if (minimizeNode >= 0) {
        error = eval->getErrorOnConstraint(minimizeNode);
        if (error > 0.01) {
            return false;
        }
    }
    for (auto it = allConstraints.begin(); it != allConstraints.end(); it++) {
        error = eval->getErrorOnConstraint(*it);
        if (error < 0.008) { // TODO: magic numbers
            cout << (*dag)[*it]->mother->lprint() << " " << error << endl;
            return false;
        }
    }
    return true;
}

bool NumericalSolver::checkFullSAT(gsl_vector* state) {
    cout << "Checking full SAT" << endl;
    cout << Util::print(state) << endl;
    seval->setInputs(interf); // TODO: can be folded into init function of seval
    seval->run(state);
    
    double error;
    if (minimizeNode >= 0) {
        error = seval->getErrorOnConstraint(minimizeNode);
        if (error > 0.01) {
            return false;
        }
    }
    for (auto it = assertConstraints.begin(); it != assertConstraints.end(); it++) {
        error = seval->getErrorOnConstraint(*it);
        if (error < 0.008) { // TODO: magic number
            return false;
        }
    }
    cout << "FULL SAT" << endl;
    return true;
}

bool NumericalSolver::initializeState() {
    bool suppressPrint = PARAMS->verbosity > 7 ? false : true;
    cout << "Initializing state" << endl;
    interf->printInputs();
    bool satInputs = opt->optimize(interf, NULL, interf->getInputConstraints(),  -1, suppressPrint, 5, NULL); // TODO: magic number
    if (satInputs) {
        cout << "Inputs satisfiable" << endl;
        result = opt->getMinState();
        cout << Util::print(result) << endl;
        return true;
    } else {
        cout << "Inputs not satisfiable" << endl;
        return false;
    }
}

gsl_vector* NumericalSolver::getResult() {
    return result;
}

LocalState* NumericalSolver::getLocalState() {
    return localState;
}

