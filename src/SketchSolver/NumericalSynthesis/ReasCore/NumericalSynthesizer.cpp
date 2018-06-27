#include "NumericalSynthesizer.h"

//gsl_vector* GDEvaluator::curGrad;
//gsl_vector* GDEvaluator::grad;
//ofstream GDEvaluator::file;
gsl_vector* SnoptEvaluator::state;
gsl_vector* SnoptEvaluator::grad;
ofstream SnoptEvaluator::file;
gsl_vector* MaxSnoptEvaluator::state;
gsl_vector* MaxSnoptEvaluator::grad;
ofstream MaxSnoptEvaluator::file;

gsl_vector* MaxEvaluator::state;
gsl_vector* MaxEvaluator::grad;
ofstream MaxEvaluator::file;

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
    
    SmoothEvaluators* smoothEval = new SmoothEvaluators(dag, interf, ctrls, ncontrols);
    smoothEval->addEvaluator(dag);

    ActualEvaluators* actualEval = new ActualEvaluators(dag, interf, ctrls);
    actualEval->addEvaluator(dag);

    OptimizationWrapper* opt;
    if (PARAMS->useSnopt) {
        opt = new SnoptWrapper(smoothEval, ncontrols, xlow, xupp, numConstraints);
    }  else {
        Assert(false, "No gradient descent wrapper");
    }
    
    MaxSolverWrapper* maxOpt = new MaxSolverWrapper(smoothEval, ncontrols, xlow, xupp, numConstraints);

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

    for (auto it = interf->levelPredicates[0].begin(); it != interf->levelPredicates[0].end(); it++) {
        Predicate* p = *it;
        if (p->isBasic()) {
            BasicPredicate* bp = (BasicPredicate*)(p);
            if (dependentInputs[bp->nid].size() > 0) {
                bp->makeImpure();
            }
        } else if (p->isDiff()) {
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
        } else {
            cout << " IMPURE" << endl;
        }
    }
    

    //sg = new SimpleSuggestionGenerator(dag, interf, ctrls);
    sg = new SuggestionGeneratorUsingMax(dag, _fm, interf, ctrls, opt, maxOpt, actualEval, smoothEval, ncontrols);


    debugger = new NumDebugger(dag, ctrls, interf, smoothEval, actualEval, opt);
    
    solver = new NumericalSolver(dag, ctrls, interf, smoothEval, actualEval, opt, dependentInputs, dependentCtrls, debugger);

    if (PARAMS->numdebug) {
        debugger->getPredicatesGraphs();
    }
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
        sat = solver->checkSAT(-1);
        if (sat) {
            gsl_vector_memcpy(state, solver->getResult());
            return true;
        }
        GradUtil::counter++;
    } 
    return false;
}


bool NumericalSynthesizer::searchWithPredicates() {
    while(true) {
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
            return true;
        } else {
            cout << "Unsat in level " << level << endl;
            if (level >= 0) {
                num_ignored++;
                cout << "Ignoring" << endl;
                if (num_ignored > 5) {
                    cout << "TOO MANY UNSATS IN LEVEL 0" << endl;
                }
            } else {
                num_ignored = 0;
                IClause* c = sg->getConflictClause(level, solver->getLocalState());
                cout << "Suggested clause: " << c->print() << endl;
                interf->addClause(c, level + 1);
            }
        }
        interf->removeOldClauses();
        GradUtil::counter++;
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



