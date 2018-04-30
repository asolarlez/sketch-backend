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




using namespace std;


class GDParameters {
public:
	SymbolicEvaluator* eval;
    const set<int>& boolNodes;
    int minimizeNode;
    double beta;
	double alpha;
	
	GDParameters(SymbolicEvaluator* eval_, const set<int>& boolNodes_, int minimizeNode_): eval(eval_), boolNodes(boolNodes_), minimizeNode(minimizeNode_) {}
};

class GDEvaluator {
	static gsl_vector* curGrad;
    static gsl_vector* grad;
public:
	
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
        
        if (p->minimizeNode >= 0) {
            e = p->eval->getErrorOnConstraint(p->minimizeNode, grad);
            error += e;
            gsl_vector_add(curGrad, grad);
        }
        
        for (auto it = p->boolNodes.begin(); it != p->boolNodes.end(); it++) {
            e = p->eval->getErrorOnConstraint(*it, grad);
            if (e < 0.0) { // TODO: this is a sharp edge - we should smooth it
                error += -e;
                gsl_vector_scale(grad, -1.0);
                gsl_vector_add(curGrad, grad);
            }
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

class GradientDescentWrapper: public OptimizationWrapper {
	GradientDescent* gd;
    int ncontrols;
    
    SymbolicEvaluator* eval;
    gsl_vector* minState;
    gsl_vector* t;
    gsl_vector* t1;
    
    int RANDOM_SEARCH = 10;
    
    doublereal* xlow;
    doublereal* xupp;
    
    double minObjectiveVal;
    double threshold = 1e-2; // accuracy for minimizing the error
	
public:
	GradientDescentWrapper(SymbolicEvaluator* eval_, int ncontrols_, doublereal* xlow_, doublereal* xupp_): eval(eval_), ncontrols(ncontrols_), xlow(xlow_), xupp(xupp_) {
		GDEvaluator::init(ncontrols);
		gd = new GradientDescent(ncontrols);
		minState = gsl_vector_alloc(ncontrols);
		t = gsl_vector_alloc(ncontrols);
        t1 = gsl_vector_alloc(ncontrols);
	}
    
    virtual bool optimize(Interface* inputs, gsl_vector* initState, const set<int>& constraints, int minimizeNode, bool suppressPrint = false, int MAX_TRIES = PARAMS->numTries, bool initRandomize = false) {
        eval->setInputs(inputs);
        GDParameters* p = new GDParameters(eval, constraints, minimizeNode);
        gd->init(GDEvaluator::f, GDEvaluator::df, GDEvaluator::fdf, p);
        
        double betas[3] = {-1, -10, -50};
        double alphas[3] = {1, 10, 50};
        
        gsl_vector_memcpy(t, initState);
        if (initRandomize) {
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

    virtual void randomizeCtrls(gsl_vector* state, Interface* inputs, const set<int>& constraints, int minimizeNode) {
        double best = GradUtil::MAXVAL;
        eval->setInputs(inputs);
        for (int i = 0; i < RANDOM_SEARCH; i++) {
            randomize(t1);
            cout << "Trying: ";
            for (int j = 0; j < t1->size; j++) {
                cout << gsl_vector_get(t1, j) << ", ";
            }
            GradUtil::BETA = -1;
            GradUtil::ALPHA = 1;
            eval->run(t1);
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
            cout << "Error: " << error << endl;
            if (error < best) {
                best = error;
                gsl_vector_memcpy(state, t1);
            }
        }
    }
	
    void randomize(gsl_vector* state) {
        for (int i = 0; i < state->size; i++) {
            double low = xlow[i];
            if (low < -100.0) low = -100.0;
            double high = xupp[i];
            if (high > 100.0) high = 100.0;
            double r = low + (rand() % (int)((high - low) * 10.0))/10.0;
            gsl_vector_set(state, i, r);
        }
    }
		
	
};

#endif
