#include "NumericalSynthesizer.h"

gsl_vector* GDEvaluator::curGrad;
gsl_vector* GDEvaluator::grad;
gsl_vector* SnoptEvaluator::state;
gsl_vector* SnoptEvaluator::grad;

using namespace std;


NumericalSynthesizer::NumericalSynthesizer(FloatManager& _fm, BooleanDAG* _dag, Interface* _interface, Lit _softConflictLit): Synthesizer(_fm), dag(_dag), interface(_interface) {
    softConflictLit = _softConflictLit;
    initialized = false;
    
    for (BooleanDAG::iterator node_it = dag->begin(); node_it != dag->end(); ++node_it) {
        bool_node* n = *node_it;
        set<int> ctrls;
        
        const vector<bool_node*>& parents = n->parents();
        if (n->type == bool_node::CTRL && n->getOtype() == OutType::FLOAT) {
            ctrls.insert(n->id);
        }
        for (int i = 0; i < parents.size(); i++) {
            bool_node* parent = parents[i];
            set<int>& parentCtrls = dependentCtrls[parent->id];
            ctrls.insert(parentCtrls.begin(), parentCtrls.end());
        }
        dependentCtrls.push_back(ctrls);
    }
}

void NumericalSynthesizer::init() {
    initialized = true;
    if (PARAMS->verbosity > 2) {
        cout << "NInputs: " << interface->size() << endl;
    }
    
    for (auto it = interface->varsMapping.begin(); it != interface->varsMapping.end(); it++) {
        cout << it->first << " -> " << (*dag)[it->second->nodeid]->lprint()  << "    " << "[" << Util::print(dependentCtrls[it->second->nodeid]) << "]" << endl;
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
    numConstraints += 100; // TODO: magic number
    
    SymbolicEvaluator* eval = new BoolAutoDiff(*dag, ctrls);
    OptimizationWrapper* opt;
    if (PARAMS->useSnopt) {
        opt = new SnoptWrapper(eval, ncontrols, xlow, xupp, numConstraints);
    } else {
        opt = new GradientDescentWrapper(eval, ncontrols, xlow, xupp);
    }
    ConflictGenerator* cg = new SimpleConflictGenerator(interface);
    SuggestionGenerator* sg = new SimpleSuggestionGenerator(dag, interface, ctrls);
    
    solver = new NumericalSolver(dag, ctrls, interface, eval, opt, cg, sg);
}

void NumericalSynthesizer::initSuggestions(vec<Lit>& suggestions) {
    init(); // TODO: there should be a better position for this
    interface->resetInputConstraints();
    cout << "Variables already set: "  << (interface->numSet()) <<  endl;
    cout << "Initializing suggestions" << endl;
    suggestions.clear();
    bool sat = solver->checkSAT();
    if (sat) {
        solver->getControls(ctrlVals);
    }
    if (sat) {
        if (!PARAMS->disableSatSuggestions) {
            const vector<tuple<int, int, int>>& s = solver->collectSatSuggestions(); // <instanceid, nodeid, val>
            convertSuggestions(s, suggestions);
        }
    } else {
        if (!PARAMS->disableUnsatSuggestions) {
            const vector<tuple<int, int, int>>& s = solver->collectUnsatSuggestions();
            convertSuggestions(s, suggestions);
        }
    }
}


bool NumericalSynthesizer::synthesis(vec<Lit>& suggestions) {
    suggestions.clear();
    conflicts.clear();
    if (!initialized) {
        return true;
    }
    //return true;
    cout << "Running numerical synthesis" << endl;
	if (PARAMS->verbosity > 7) {
        timer.restart();
	}
	
	bool sat = solver->checkSAT();
	if (sat || solver->ignoreConflict()) {
		solver->getControls(ctrlVals);
        if (sat) {
            if (!PARAMS->disableSatSuggestions) {
                const vector<tuple<int, int, int>>& s = solver->collectSatSuggestions(); // <instanceid, nodeid, val>
                convertSuggestions(s, suggestions);
            }
        } else {
            if (!PARAMS->disableUnsatSuggestions) {
                const vector<tuple<int, int, int>>& s = solver->collectUnsatSuggestions();
                convertSuggestions(s, suggestions);
            }
        }
        if (PARAMS->verbosity > 7) {
            timer.stop().print("Numerical solving time:");
        }
		return true;
	} else {
		if (PARAMS->verbosity > 7) {
			cout << "****************CONFLICT****************" << endl;
		}
		solver->getConflicts(conflicts); // <instanceid, nodeid>
        if (PARAMS->verbosity > 7) {
            timer.stop().print("Numerical solving time:");
        }
		return false;
	}
}

bool_node* NumericalSynthesizer::getExpression(DagOptim* dopt, const vector<bool_node*>& params) {
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


void NumericalSynthesizer::convertSuggestions(const vector<tuple<int, int, int>>& s, vec<Lit>& suggestions) {
	for (int k = 0; k < s.size(); k++) {
		int i = get<0>(s[k]);
		int j = get<1>(s[k]);
		int v = get<2>(s[k]);
        //cout << "Suggesting " << i << " " << j <<  " " << v << endl;
        Assert(i == 0, "Multiple instances is not yet supported");
		suggestions.push(interface->getLit(j, v));
	}
}


