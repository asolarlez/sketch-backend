#include "NumericalSolver.h"


int SnoptEvaluator::counter;

NumericalSolver::NumericalSolver(BooleanDAG* _dag, map<string, int>& _ctrls, Interface* _interface, SymbolicEvaluator* _eval, OptimizationWrapper* _opt, ConflictGenerator* _cg, SuggestionGenerator* _sg): dag(_dag), ctrls(_ctrls), interface(_interface), eval(_eval), opt(_opt), cg(_cg), sg(_sg) {
	ncontrols = ctrls.size();
    // if ncontrols = 0, make it 1 just so numerical opt does not break
	if (ncontrols == 0) {
		ncontrols = 1;
	}
	
	cout << "NControls: " << ncontrols << endl;
    
    GradUtil::allocateTempVectors(ncontrols);
	
	state = gsl_vector_alloc(ncontrols);
	
    previousSAT = false;
    fullSAT = false;
    numConflictsAfterSAT = 0;
    inputConflict = false;
    
    seval = new SimpleEvaluator(*dag, ctrls);
    minimizeNode = -1;
    
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
}

void NumericalSolver::setState(gsl_vector* s) {
    gsl_vector_memcpy(state, s);
}

bool NumericalSolver::checkSAT() {
    inputConflict = false;
    bool suppressPrint = PARAMS->verbosity > 7 ? false : true;
    
    bool sat = false;
    if (!checkInputs()) {
        return true;
    }
    if (checkCurrentSol()) {
        cout << "Current sol passed" << endl;
        sat = true;
    } else {
        bool noInputs = interface->numSet() == 0;
        if (!previousSAT) {
            if (!noInputs && !initializeState(suppressPrint)) {
                return false;
            }
        }
        
        cout << "Running optimization" << endl;
        set<int> allConstraints;
        allConstraints.insert(assertConstraints.begin(), assertConstraints.end());
        const set<int>& inputConstraints = interface->getInputConstraints();
        allConstraints.insert(inputConstraints.begin(), inputConstraints.end());
        const set<int>& assertedInputConstraints = interface->getAssertedInputConstraints();
        allConstraints.insert(assertedInputConstraints.begin(), assertedInputConstraints.end());
        
        sat = opt->optimize(interface, state, allConstraints, minimizeNode, suppressPrint, PARAMS->numTries, noInputs); 
        if (sat || !previousSAT) {
            gsl_vector_memcpy(state, opt->getMinState());
        }
    }
    if (!previousSAT) {
        previousSAT = sat;
    }
    if (sat) {
        cout << "FOUND solution" << endl;
        printControls();
        if (checkFullSAT()) {
            fullSAT = true;
            cout << "FULL SAT" << endl;
        }
        numConflictsAfterSAT = 0;
    }
    
    if (!sat && previousSAT) {
        numConflictsAfterSAT++;
        if (numConflictsAfterSAT > 5) { // TODO: magic number
            previousSAT = false;
        }
    }
    double objective = opt->getObjectiveVal();
    cout << "Objective found: " << objective << endl;
    
    return sat;
}


bool NumericalSolver::checkInputs() {
    if (fullSAT) return false;
    return true;
}

bool NumericalSolver::checkCurrentSol() {
    cout << "Checking current solution" << endl;
    GradUtil::BETA = -50; // TODO: magic numbers
    GradUtil::ALPHA = 50;
    eval->setInputs(interface); // TODO: since this is a pointer, we don't have to set it everytime
    eval->run(state);
    cout << Util::print(state) << endl;
    
    set<int> allConstraints; // TODO: this could be further optimized to check only those constraints whose eval changed since last iteration
    allConstraints.insert(assertConstraints.begin(), assertConstraints.end());
    const set<int>& inputConstraints = interface->getInputConstraints();
    allConstraints.insert(inputConstraints.begin(), inputConstraints.end());
    const set<int>& assertedInputConstraints = interface->getAssertedInputConstraints();
    allConstraints.insert(assertedInputConstraints.begin(), assertedInputConstraints.end());
    
    double error;
    if (minimizeNode >= 0) {
        error = eval->getErrorOnConstraint(minimizeNode);
        if (error > 0.01) {
            return false;
        }
    }
    for (auto it = allConstraints.begin(); it != allConstraints.end(); it++) {
        error = eval->getErrorOnConstraint(*it);
        if (error < -0.01) { // TODO: magic numbers
            return false;
        }
    }
    return true;
}

bool NumericalSolver::checkFullSAT() {
    cout << "Checking full SAT" << endl;
    seval->setInputs(interface); // TODO: can be folded into init function of seval
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
        if (error < -0.01) { // TODO: magic number
            return false;
        }
    }
    return true;
}

bool NumericalSolver::initializeState(bool suppressPrint) {
    cout << "Initializing state" << endl;
    bool satInputs = opt->optimize(interface, state, interface->getInputConstraints(),  minimizeNode, suppressPrint, 5, true); // TODO: magic number
    if (satInputs) {
        cout << "Inputs satisfiable" << endl;
        gsl_vector_memcpy(state, opt->getMinState());
        return true;
    } else {
        cout << "Inputs not satisfiable" << endl;
        inputConflict = true;
        return false;
    }
}

bool NumericalSolver::ignoreConflict() {
    if (previousSAT) return false;
    if (inputConflict) return false;
    if (interface->numSet() < CONFLICT_CUTOFF) { 
        return true;
    } else {
        return false;
    }
}

vector<tuple<int, int, int>> NumericalSolver::collectSatSuggestions() {
    return sg->getSatSuggestions(state);
}


vector<tuple<int, int, int>> NumericalSolver::collectUnsatSuggestions() {
    return sg->getUnsatSuggestions(state);
}

void NumericalSolver::getConflicts(vector<pair<int, int>>& conflicts) {
    cg->getConflicts(conflicts);
}

void NumericalSolver::getControls(map<string, double>& ctrlVals) {
	for (auto it = ctrls.begin(); it != ctrls.end(); it++) {
		ctrlVals[it->first] = gsl_vector_get(state, it->second);
	}
}

void NumericalSolver::printControls() {
    cout << Util::print(state) << endl;
    
    for (auto it = ctrls.begin(); it != ctrls.end(); it++) {
        cout << it->first << "," << gsl_vector_get(state, it->second) << ";";
    }
    cout << endl;
}
