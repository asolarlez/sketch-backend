#include "NumericalSolver.h"


int SnoptEvaluator::counter;

NumericalSolver::NumericalSolver(BooleanDAG* _dag, map<string, int>& _ctrls, Interface* _interface, SmoothEvaluators* _smoothEval, ActualEvaluators* _actualEval, OptimizationWrapper* _opt, const vector<vector<int>>& _dependentInputs, const vector<vector<int>>& _dependentCtrls, NumDebugger* _debugger): dag(_dag), ctrls(_ctrls), interf(_interface), smoothEval(_smoothEval), actualEval(_actualEval), opt(_opt), dependentInputs(_dependentInputs), dependentCtrls(_dependentCtrls), debugger(_debugger) {
	ncontrols = ctrls.size();
    // if ncontrols = 0, make it 1 just so numerical opt does not break
	if (ncontrols == 0) {
		ncontrols = 1;
	}
	
	cout << "NControls: " << ncontrols << endl;
    
    GradUtil::allocateTempVectors(ncontrols);
	localState = new LocalState(ncontrols, 3);
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

bool NumericalSolver::checkSAT(int level, gsl_vector* initState) {
    interf->printInputs();
    if (initState != NULL && PARAMS->verbosity > 3) {
        cout << "Init state: " << Util::print(initState) << endl;
    }
    bool suppressPrint = PARAMS->verbosity > 7 ? false : true;
    bool sat = false;
    
    if (false && PARAMS->numdebug) { 
        debugger->getGraphs(level, GradUtil::counter);
    }
    cout << "Running optimization " << GradUtil::counter << " Level: " << level << endl;
    sat = opt->optimize(interf, initState, assertConstraints, minimizeNode, suppressPrint, PARAMS->numTries, localState, level);
    if (sat) {
        result = opt->getMinState();
        cout << "Level " << level << " satisfiable" << endl;
        if (PARAMS->verbosity > 3) {
            cout << Util::print(result) << endl;
        }
        return true;
    } else {
        result = opt->getMinState();
        cout << "Level " << level << " unsatisfiable" << endl;
        if (PARAMS->verbosity > 3) {
            cout << "Local solution" << endl;
            localState->print();
        }
        return false;
    }
}

bool NumericalSolver::checkFullSAT(gsl_vector* state) {
    cout << "Checking full SAT" << endl;
    cout << Util::print(state) << endl;
    actualEval->run(state);
    
    double error;
    if (minimizeNode >= 0) {
        error = actualEval->getErrorOnConstraint(minimizeNode);
        if (error > 0.01) {
            return false;
        }
    }
    for (auto it = assertConstraints.begin(); it != assertConstraints.end(); it++) {
        error = actualEval->getErrorOnConstraint(*it);
        if (error < -0.01) { // TODO: magic number
            return false;
        }
    }
    cout << "FULL SAT" << endl;
    return true;
}

gsl_vector* NumericalSolver::getResult() {
    return result;
}

LocalState* NumericalSolver::getLocalState() {
    return localState;
}

