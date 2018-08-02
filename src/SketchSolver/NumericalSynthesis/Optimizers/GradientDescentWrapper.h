#pragma once
#include <fstream>
#include <iostream>
#include <vector>
#include <map>
#include <math.h>
#include "GradientDescent.h"
#include "OptimizationWrapper.h"
#include "SymbolicEvaluator.h"
#include "Util.h"
#include "Interface.h"
#include "MaxOptimizationWrapper.h"




using namespace std;


class GDParameters {
public:
	SmoothEvaluators* eval;
    const set<int>& assertConstraints;
    int minimizeNode;
    Interface* interf;
    double beta;
    double alpha;
    int level;
    bool maximize;
	
	GDParameters(SmoothEvaluators* eval_, const set<int>& assertConstraints_, int minimizeNode_, Interface* interf_, int level_, bool maximize_ = false): eval(eval_), assertConstraints(assertConstraints_), minimizeNode(minimizeNode_), interf(interf_), level(level_), maximize(maximize_) {}
};

class GDEvaluator {
	static gsl_vector* curGrad;
    static gsl_vector* grad;
public:
    static ofstream file;
	
	static void init(int size) {
		curGrad = gsl_vector_alloc(size);
        grad = gsl_vector_alloc(size);
	}
	
	static double f(const gsl_vector* x, void* params) {
        GDParameters* p = (GDParameters*) params;
		GradUtil::BETA = p->beta;
		GradUtil::ALPHA = p->alpha;
		gsl_vector_set_zero(curGrad);
		double error = 0;
        double e;
        p->eval->run(x);
        if (PARAMS->numdebug) {
            cout << Util::print(x) << endl;
            file << Util::print(x) << endl;
        }
        
        if (p->minimizeNode >= 0 && p->level == -1) {
            e = p->eval->getErrorOnConstraint(p->minimizeNode, grad);
            error += e;
            gsl_vector_add(curGrad, grad);
        }
        
        if (p->level == -1) {
            for (auto it = p->assertConstraints.begin(); it != p->assertConstraints.end(); it++) {
                e = p->eval->getErrorOnConstraint(*it, grad);
                if (e < 0.0) { // TODO: this is a sharp edge - we should smooth it
                    error += -e;
                    gsl_vector_scale(grad, -1.0);
                    gsl_vector_add(curGrad, grad);
                }
            }

            const set<int>& inputConstraints = p->interf->getInputConstraints();
            for (auto it = inputConstraints.begin(); it != inputConstraints.end(); it++) {
                e = p->eval->getErrorOnConstraint(*it, grad);
                if (e < 0.0) { // TODO: this is a sharp edge - we should smooth it
                    error += -e;
                    gsl_vector_scale(grad, -1.0);
                    gsl_vector_add(curGrad, grad);
                }
            }
        }

        for (int i = p->level; i < p->interf->numLevels(); i++) {
            if (i < 0) continue;
            for (auto it = p->interf->clauseLevels[i].begin(); it != p->interf->clauseLevels[i].end(); it++) {
                e = p->eval->getErrorOnClause(*it, grad);
                double scale = 1.0;
                if (i > p->level) scale = 100.0;
                if (e < 0.0) {
                    error += -e*scale;
                    gsl_vector_scale(grad, -scale);
                    gsl_vector_add(curGrad, grad);
                }
            }
        }
        if (p->maximize) {
            gsl_vector_scale(curGrad, -1.0);
            error = -error;
        }
		return error;
	}
	
	static void df(const gsl_vector* x, void* params, gsl_vector* d) {
		// just use the curGrad - assuming that this function is called immediately after calling eval_f with the same arguments
		gsl_vector_memcpy(d, curGrad);
	}
	
	static void fdf(const gsl_vector* x, void* params, double* v, gsl_vector* g) {
		*v = f(x, params);
		df(x, params, g);
	}
};

#ifndef _NOGSL

class GradientDescentWrapper: public OptimizationWrapper, public MaxOptimizationWrapper {
	GradientDescent* gd;
    int ncontrols;
    
    SmoothEvaluators* eval;
    gsl_vector* minState;
    gsl_vector* t;
    gsl_vector* t1;
    
    int RANDOM_SEARCH = 10;
    
    doublereal* xlow;
    doublereal* xupp;
    
    double minObjectiveVal;
    double threshold = 1e-2; // accuracy for minimizing the error
    timerclass timer;
	
public:
	GradientDescentWrapper(SmoothEvaluators* eval_, int ncontrols_, doublereal* xlow_, doublereal* xupp_): eval(eval_), ncontrols(ncontrols_), xlow(xlow_), xupp(xupp_) {
		GDEvaluator::init(ncontrols);
		gd = new GradientDescent(ncontrols);
		minState = gsl_vector_alloc(ncontrols);
		t = gsl_vector_alloc(ncontrols);
        t1 = gsl_vector_alloc(ncontrols);
	}

    virtual bool maximize(Interface* inputs, const gsl_vector* initState, const set<int>& assertConstraints, int minimizeNode, float beta, int level, int idx) { 
        GDParameters* p = new GDParameters(eval, assertConstraints, minimizeNode, inputs, level, true);
        gd->init(GDEvaluator::f, GDEvaluator::df, GDEvaluator::fdf, p);

        gsl_vector_memcpy(t, initState);

        p->beta = beta;
        p->alpha = -beta;
        if (PARAMS->numdebug) { 
            GDEvaluator::file.open("/afs/csail.mit.edu/u/j/jinala/symdiff/scripts/smoothing/maxdata/" + Util::benchName() + "_" + to_string(GradUtil::counter) + "_" + to_string(idx) + "_gdmax_"  + to_string(int(-beta)) + "_" + to_string(PARAMS->smoothingMode) + ".txt");
        }
        for (int i = 0; i < ncontrols; i++) {
            cout << gsl_vector_get(t, i) << ", ";
        }
                
        cout << endl;
        double obj = gd->optimize(t);
        gsl_vector_memcpy(t, gd->getResults());
            
        gsl_vector_memcpy(minState, gd->getResults());
        if (PARAMS->numdebug) {  
            GDEvaluator::file << Util::print(t) << endl;
            GDEvaluator::file.close();
        }
        return true;
    }
    
    virtual bool optimize(Interface* inputs, gsl_vector* initState, const set<int>& constraints, int minimizeNode, bool suppressPrint, int MAX_TRIES, LocalState* localState, int level = -1) {
        GDParameters* p = new GDParameters(eval, constraints, minimizeNode, inputs, level);
        gd->init(GDEvaluator::f, GDEvaluator::df, GDEvaluator::fdf, p);
        
        double betas[3] = {-1, -10, -50};
        double alphas[3] = {1, 10, 50};
        
        if (initState != NULL) {
            gsl_vector_memcpy(t, initState);
        } else {
            randomizeCtrls(t, inputs, constraints, minimizeNode);
        }
        
        
        int numtries = 0;
        minObjectiveVal = 1e50;
        double obj;
        while (minObjectiveVal > threshold && numtries < MAX_TRIES) {
            if (!suppressPrint) {
                cout << "Attempt: " << (numtries + 1) << endl;
            }
            
            for (int i = 0; i < 3; i++) {
                if (!suppressPrint) {
                    cout << "Beta: " << betas[i] << " Alpha: " << alphas[i] << endl;
                }
                if (PARAMS->numdebug) { 
                    GDEvaluator::file.open("/afs/csail.mit.edu/u/j/jinala/symdiff/scripts/smoothing/optdata/" + Util::benchName() + "_" + to_string(GradUtil::counter) + "_gdopt_"  + to_string(int(alphas[i])) + "_" + to_string(PARAMS->smoothingMode) + ".txt");
                }
                p->beta = betas[i];
                p->alpha = alphas[i];
                if (!suppressPrint) {
                    for (int i = 0; i < ncontrols; i++) {
                        cout << gsl_vector_get(t, i) << ", ";
                    }
                }
                cout << endl;
                obj = gd->optimize(t);
                gsl_vector_memcpy(t, gd->getResults());
                if (PARAMS->numdebug) {
                    GDEvaluator::file << Util::print(t) << endl;
                    GDEvaluator::file << timer.stop().get_cur_ms() << endl;
                    GDEvaluator::file.close();
                }
                    
                if (localState != NULL) {
                    gsl_vector_memcpy(localState->localSols[i], t); // TODO: this does not work with multiple retries and we want best local state so far. 
                }
            }
            if (numtries == 0) {
                gsl_vector_memcpy(minState, gd->getResults());
            }
            if (obj < minObjectiveVal) {
                minObjectiveVal = obj;
                gsl_vector_memcpy(minState, gd->getResults());
            }
            numtries++;
            if (minObjectiveVal > threshold && numtries < MAX_TRIES) {
                randomizeCtrls(t, inputs, constraints, minimizeNode);
            }
        }
        return minObjectiveVal < threshold;
    }
    
    virtual gsl_vector* getMinState() {
        return minState;
    }
    
    virtual double getObjectiveVal() {
        return minObjectiveVal;
    }
    double getError(const set<int>& constraints, int minimizeNode) {
        double error = 0.0;
        double e;
        if (minimizeNode >= 0) {
            error += eval->getErrorOnConstraint(minimizeNode);
        }
        for (auto it = constraints.begin(); it != constraints.end(); it++) {
            e = eval->getErrorOnConstraint(*it);
            if (e < 0.0) {
                error += -e;
            }
        }
        return error;
    }
    

    virtual void randomizeCtrls(gsl_vector* state, Interface* inputs, const set<int>& constraints, int minimizeNode) {
        double best = GradUtil::MAXVAL;
        bool foundValid = false;
        for (int i = 0; i < RANDOM_SEARCH; i++) {
            randomize(t1);
            cout << "Trying: ";
            for (int j = 0; j < t1->size; j++) {
                cout << gsl_vector_get(t1, j) << ", ";
            }
            GradUtil::BETA = -50;
            GradUtil::ALPHA = 50;
            eval->run(t1);
            double error = getError(constraints, minimizeNode);
            cout << "Error: " << error << " ";
            bool valid = true;
            double e;
            for (int i = 0; i < inputs->numLevels(); i++) {
                for (auto it = inputs->clauseLevels[i].begin(); it != inputs->clauseLevels[i].end(); it++) {
                    e = eval->getErrorOnClause(*it);
                    if (e < 0.0) {
                        valid = false;
                        goto done;
                    }
                }
            }
            done:
            if (valid) {
                cout << "valid" << endl;
            } else {
                cout << "invalid" << endl;
            }
            if (foundValid) {
                if (valid && error < best) {
                    best = error;
                    gsl_vector_memcpy(state, t1);
                }
            } else {
                if (valid) {
                    foundValid = true;
                    best = error;
                    gsl_vector_memcpy(state, t1);
                } else if (error < best) {
                    best = error;
                    gsl_vector_memcpy(state, t1);
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
		
	
};

#endif
