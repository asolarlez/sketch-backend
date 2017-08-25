#pragma once
#include <fstream>
#include <iostream>
#include <vector>
#include <map>
#include <math.h>
#include "GradientDescent.h"
#include "OptimizationWrapper.h"
#include "NumericalSolverHelper.h"

using namespace std;


class GDParameters {
public:
	NumericalSolverHelper* ns;
	double beta;
	double alpha;
};

class GDEvaluator {
	static gsl_vector* curGrad;
public:
	
	static void init(int size) {
		curGrad = gsl_vector_alloc(size);
	}
	
	static double f(const gsl_vector* x, void* params) {
		GDParameters* p = (GDParameters*) params;
		GradUtil::BETA = p->beta;
		GradUtil::ALPHA = p->alpha;
		return p->ns->evalGD(x, curGrad);
	}
	
	static void df(const gsl_vector* x, void* params, gsl_vector* d) {
		// just use the curGrad - assuming that this function is called immediately after calling eval_f with the same arguments
		gsl_vector_memcpy(d, curGrad);
	}
	
	static void fdf(const gsl_vector* x, void* params, double* f, gsl_vector* df) {
		GDParameters* p = (GDParameters*) params;
		GradUtil::BETA = p->beta;
		GradUtil::ALPHA = p->alpha;
		*f = p->ns->evalGD(x, df);
	}
	
	
};

class GradientDescentWrapper: public OptimizationWrapper {
	
	GradientDescent* gd;
	float threshold = 1e-5; // accuracy for minimizing the error
	int MAX_TRIES = 9; // Number retries of GD algorithm for each iteration
	float minErrorSoFar;
	gsl_vector* minState;
	gsl_vector* t;
	int ncontrols;
	
public:
	GradientDescentWrapper(int ncontrols_): ncontrols(ncontrols_) {
		GDEvaluator::init(ncontrols);
		gd = new GradientDescent(ncontrols);
		minState = gsl_vector_alloc(ncontrols);
		t = gsl_vector_alloc(ncontrols);
	}
	
	bool optimize(NumericalSolverHelper* ns, gsl_vector* initState) {
		minErrorSoFar = 1e50;
		
		GDParameters* p = new GDParameters();
		p->ns = ns;
		gd->init(GDEvaluator::f, GDEvaluator::df, GDEvaluator::fdf, p, initState);
		double minError = gd->optimize();
		gsl_vector* curState = gd->getResults();
		if (minError < minErrorSoFar) {
			minErrorSoFar = minError;
			gsl_vector_memcpy(minState, curState);
		}
		int numtries = 0;
		while (minError > threshold && numtries < MAX_TRIES) {
			cout << "Retry attempt: " << (numtries + 1) << endl;
			// redo gradient descent with random initial point
			ns->randomizeCtrls(t);
			for (int i = 0; i < ncontrols; i++) {
				cout << gsl_vector_get(t, i) << ", ";
			}
			cout << endl;
			
			gd->init(GDEvaluator::f, GDEvaluator::df, GDEvaluator::fdf, p, t);
			minError = gd->optimize();
			curState = gd->getResults();
			if (minError < minErrorSoFar) {
				minErrorSoFar = minError;
				gsl_vector_memcpy(minState, curState);
			}
			numtries++;
		}
		return minError <= threshold;
	}
	gsl_vector* getMinState() {
		return minState;
	}
};
