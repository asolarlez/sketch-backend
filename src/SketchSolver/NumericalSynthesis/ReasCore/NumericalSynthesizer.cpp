#include "NumericalSynthesizer.h"

gsl_vector* GDEvaluator::curGrad;
gsl_vector* GDEvaluator::grad;
ofstream GDEvaluator::file;
gsl_vector* SnoptEvaluator::state;
gsl_vector* SnoptEvaluator::grad;
ofstream SnoptEvaluator::file;
gsl_vector* MaxSnoptEvaluator::state;
gsl_vector* MaxSnoptEvaluator::grad;
ofstream MaxSnoptEvaluator::file;

int GradUtil::counter;

using namespace std;


NumericalSynthesizer::NumericalSynthesizer(FloatManager& _fm, BooleanDAG* _dag, Interface* _interface): dag(_dag), interf(_interface) {    
    for (BooleanDAG::iterator node_it = dag->begin(); node_it != dag->end(); ++node_it) {
        bool_node* n = *node_it;
        set<int> ctrls;
        const vector<bool_node*>& parents = n->parents();
        if (n->type == bool_node::CTRL && n->getOtype() == OutType::FLOAT) {
            ctrls.insert(n->id);
        }
        for (int i = 0; i < parents.size(); i++) {
            bool_node* parent = parents[i];
            vector<int>& parentCtrls = dependentCtrls[parent->id];
            ctrls.insert(parentCtrls.begin(), parentCtrls.end());
        }
        dependentCtrls.push_back(vector<int>(ctrls.begin(), ctrls.end()));
    }

    if (PARAMS->verbosity > 2) {
        cout << "NInputs: " << interf->size() << endl;
    }
    
    for (auto it = interf->varsMapping.begin(); it != interf->varsMapping.end(); it++) {
        cout << (*dag)[it->first]->lprint()  << "    " << "[" << Util::print(dependentCtrls[it->first]) << "]" << endl;
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
    
    doublereal* xlow = new doublereal[ncontrols];
    doublereal* xupp = new doublereal[ncontrols];
    
    for (int i = 0; i < ctrlNodes.size(); i++) {
        CTRL_node* cnode = (CTRL_node*) ctrlNodes[i];
        if (cnode->getOtype() == OutType::FLOAT) {
            int idx = ctrls[cnode->get_name()];
            xlow[idx] = cnode->hasRange ? cnode->low : -20.0;
            xupp[idx] = cnode->hasRange ? cnode->high : 20.0;
        } else if (cnode->getOtype() == OutType::BOOL) {
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
    numConstraints +=  1000; // TODO: magic number
    
    SymbolicEvaluator* eval = new BoolAutoDiff(*dag, ctrls);
    OptimizationWrapper* opt;
    if (PARAMS->useSnopt) {
        opt = new SnoptWrapper(eval, ncontrols, xlow, xupp, numConstraints);
    } else {
#ifndef _NOGSL
        opt = new GradientDescentWrapper(eval, ncontrols, xlow, xupp);
#else
        Assert(false, "NO GSL");
#endif
    }
    
    for (BooleanDAG::iterator node_it = dag->begin(); node_it != dag->end(); ++node_it) {
        bool_node* n = *node_it;
        set<int> inputs;
        const vector<bool_node*>& parents = n->parents();
        for (int i = 0; i < parents.size(); i++) {
            bool_node* parent = parents[i];
            if (interf->isInput(parent->id)) {
                inputs.insert(parent->id);
            } else {
                vector<int>& parentInputs = dependentInputs[parent->id];
                inputs.insert(parentInputs.begin(), parentInputs.end());
            }
        }
        dependentInputs.push_back(vector<int>(inputs.begin(), inputs.end()));
    }
    sg = new SimpleSuggestionGenerator(dag, interf, ctrls);
    //SuggestionGenerator* sg = new SuggestionGeneratorUsingMax(dag, interf, ctrls, opt);


    debugger = new NumDebugger(dag, ctrls, interf, eval, opt);
    
    solver = new NumericalSolver(dag, ctrls, interf, eval, opt, dependentInputs, dependentCtrls, debugger);

    
    //debugger->plotConditions();
    //debugger->doMaxOpt();
    //debugger->doOpt();
    //debugger->getGraphs(0);
    //exit(0);
}

bool NumericalSynthesizer::solve() {
    bool success = search();
    if (success) {
        success = concretize();
    }
    return success;
}

bool NumericalSynthesizer::searchWithOnlySmoothing() {
    bool sat = false;
    for (int i = 0; i < PARAMS->maxRestarts; i++) {
        sat = solver->checkSAT();
        if (sat) {
            gsl_vector_memcpy(state, solver->getResult());
            return true;
        }
    } 
    return false;
}

bool NumericalSynthesizer::searchWithPredicates() {
    bool sat = false;
    bool first = true;
    while(true) {
        sat = solver->checkSAT(first ? NULL : state);
        first = false;
        if (sat) {
            gsl_vector_memcpy(state, solver->getResult());
            return true;
        }
        else {
            if (interf->numSet() >= CONFLICT_THRESHOLD) {
                cout << "Reach conflict threshold -- restarting" << endl;
                interf->restartInputs();
                first = true;
                continue;
            } else {
                sg->initUnsatSuggestions(solver->getLocalState());
                bool foundValidPredicate = false;
                for (int i = 0; i < NUM_SUGGESTIONS_THRESHOLD; i++) {
                    const pair<int, int>& s = sg->getNextUnsatSuggestion();
                    cout << "Suggestion " << s.first << " " << s.second << endl;
                    if (s.first >= 0 && interf->tryInput(s.first, s.second)) {
                        bool hasInput = solver->initializeState();
                        if (hasInput) {
                            foundValidPredicate = true;
                            gsl_vector_memcpy(state, solver->getResult());
                            break;
                        } else {
                            interf->clearLastInput();
                        }
                    }
                }
                if (!foundValidPredicate) {
                    cout << "None of the suggestions worked -- restarting" << endl;
                    interf->restartInputs();
                    first = true;
                    continue;
                }
            }
        }
    }
}

bool NumericalSynthesizer::search() {
    if (PARAMS->numericalSolverMode == "ONLY_SMOOTHING") {
        return searchWithOnlySmoothing();
    } else if (PARAMS->numericalSolverMode == "SMOOTHING_SAT") {
        return searchWithPredicates();
    }
    Assert(false, "NYI for solver mode " + PARAMS->numericalSolverMode);
    return true;
}

bool NumericalSynthesizer::concretize() {
    return solver->checkFullSAT(state);
}



