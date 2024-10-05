#pragma once
#include "Sampler.h"
#include "GradUtil.h"
#ifndef _NOGSL
#include <gsl/gsl_vector.h>
#else
#include "FakeGSL.h"
#endif
#include "ActualEvaluators.h"

class BasicSampler : public Sampler {
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

public:

    BasicSampler(BooleanDAG* dag_, Interface* interf_, ActualEvaluators* actualEval_, int ncontrols_,  doublereal* xlow_, doublereal* xupp_): dag(dag_), interf(interf_), actualEval(actualEval_), ncontrols(ncontrols_), xlow(xlow_), xupp(xupp_) {

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

	virtual void sampleState(gsl_vector* state) { 
		double best = GradUtil::MAXVAL;
        bool foundValid = false;
        for (int i = 0; i < RANDOM_SEARCH; i++) {
            randomize(tmp);
            cout << "Trying new initialization in sampleState 1." << endl;
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
            cout << "[sampleState] Error: " << error << " " <<endl;
            
            if (error < best) {
                best = error;
                gsl_vector_memcpy(state, tmp);
            }

            if(error == 0.0) {
                cout << "[sampleState] Found valid state." << endl;
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
