#pragma once
#include <fstream>
#include <iostream>
#include <map>

#include "Sampler.h"
#include "GradUtil.h"
#include "ActualEvaluators.h"
#include "SnoptWrapper.h"

using namespace std;

class SClause {
	vector<pair<int, int>> clause;

public:
	void add(int assertId, int confId) {
		clause.push_back(make_pair(assertId, confId));
	}

	int size() {
		return clause.size();
	}

	pair<int, int>& getPair(int i) {
		return clause[i];
	}

	string print() {
        stringstream s;
        for (int i = 0; i < clause.size(); i++) {
            s << "(" << get<0>(clause[i]) << ":" << get<1>(clause[i]) << ") ";
        }
        return s.str();
    }
};

class BoolBasedSampler : public Sampler {
	Interface* interf;
	BooleanDAG* dag;
	ActualEvaluators* actualEval;
	doublereal* xlow;
    doublereal* xupp;
    int ncontrols;

	gsl_vector* tmp;
	int RANDOM_SEARCH = 10;

    set<int> assertConstraints; 
	int minimizeNode;

	vector<SClause*> clauses; 

	const vector<vector<int>>& dependentInputs;


public:

    BoolBasedSampler(BooleanDAG* dag_, Interface* interf_, ActualEvaluators* actualEval_, int ncontrols_,  doublereal* xlow_, doublereal* xupp_, const vector<vector<int>>& dependentInputs_): dag(dag_), interf(interf_), actualEval(actualEval_), ncontrols(ncontrols_), xlow(xlow_), xupp(xupp_), dependentInputs(dependentInputs_) {

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

	    tmp = gsl_vector_alloc(ncontrols);
    }

    void addClause(SClause* clause) {
    	clauses.push_back(clause);
    }

    void analyze(gsl_vector* state, SClause* s = NULL) {
        cout << "in analyze" << endl;
    	actualEval->run(state);
        bool do_print = false;  
        if (do_print) {
            cout << "BA" << GradUtil::counter << ",";
        }
    	for (auto it1 = assertConstraints.begin(); it1 != assertConstraints.end(); it1++) {
    		const vector<int>& inputs = dependentInputs[*it1];
            int minid = -1;
            double minerror  = 1e30;
            double d;
            for (auto it = inputs.begin(); it != inputs.end(); it++) {
                d = actualEval->dist(*it);
                if (d < minerror) {
                    minerror = d;
                    minid = *it;
                } 
            }
            if (minid != -1) {
                if (do_print) {
                    cout << "(" << *it1 << ":" << minid;
                    if (minerror < 0.0) {
                        cout << ":S";
                    } else if (minerror < 0.1) {
                        cout << ":B";
                    } else {
                        cout << ":U";
                    }
                }
                if (s != NULL) {
                    for (int k = 0; k < s->size(); k++) {
                        if (get<0>(s->getPair(k)) == *it1) {
                            if (do_print) {
                                cout << ":L";
                            }
                        }
                    }
                }
                if (do_print) {
                    cout << "),";
                }
            }
        }
        if (do_print) {
            cout << endl;
        }
    }

	virtual void sampleState(gsl_vector* state) { 
		tuple<int, double> best = make_tuple(0, 1e30);
        for (int i = 0; i < RANDOM_SEARCH; i++) {
            randomize(tmp);
            cout << "Trying new initialization in sampleState 2." << endl;
            static bool printDebugInfo = false;  // Initialize the debug flag
            if (printDebugInfo) {
                cout << "Trying: ";
                for (int j = 0; j < tmp->size; j++) {
                    cout << gsl_vector_get(tmp, j) << ", ";
                }
                cout << endl;
            }
            actualEval->run(tmp);
            double error = getError();
            cout << "Error: " << error << " ";

            int numClausesSatisfied = 0;

            for (int k = 0; k < clauses.size(); k++) {
            	if (isClauseSatisfied(clauses[k])) {
            		numClausesSatisfied++;
            	}
            }

            cout << "Num sat clauses: "<< numClausesSatisfied;
            if (numClausesSatisfied < clauses.size()) {
                cout << " CLAUSE_UNSAT";
            }
            cout << endl;

            if (numClausesSatisfied > get<0>(best)) {
            	get<0>(best) = numClausesSatisfied;
            	get<1>(best) = error;
            	gsl_vector_memcpy(state, tmp);
            } else if (numClausesSatisfied == get<0>(best)) {
            	if (error < get<1>(best)) {
            		get<1>(best) = error;
            		gsl_vector_memcpy(state, tmp);
            	}
            }
        }
    }
    
    void randomize(gsl_vector* state) {
        for (int i = 0; i < state->size; i++) {
            double low = xlow[i];
            double high = xupp[i];
            double r = low + (rand() % (int)((high - low) * 10.0))/10.0;
            gsl_vector_set(state, i, r);
        }
    }

    bool isClauseSatisfied(SClause* clause) { // Hacky
        bool allSat = true;
    	for (int i = 0; i < clause->size(); i++) {
    		const auto& t = clause->getPair(i);
    		int assertId = get<0>(t);
    		int confId = get<1>(t);

            if (actualEval->getErrorOnConstraint(assertId) < -0.01) {
                allSat = false;
            }

    		const vector<int>& inputs = dependentInputs[assertId];
            int minid = -1;
            double minerror  = 1e30;
            double d;
            for (auto it = inputs.begin(); it != inputs.end(); it++) {
                d = actualEval->dist(*it);
                if (d < minerror) {
                    minerror = d;
                    minid = *it;
                } 
            }
            if (minid != confId) {
            	return true;
            }
    	}
    	 return allSat;
    }

    double getError() {
        double error = 0.0;
        double e;
        if (minimizeNode >= 0) {
            e = actualEval->getErrorOnConstraint(minimizeNode);
            if (e > 0.0) {
                error += e; // TODO : change this
            }
        }
        for (auto it = assertConstraints.begin(); it != assertConstraints.end(); it++) {
            e = actualEval->getErrorOnConstraint(*it);
            if (e < 0.0) {
                error += -e;
            }
        }
        return error;
    }
};
