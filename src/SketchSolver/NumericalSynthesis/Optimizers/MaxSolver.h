#pragma once

#ifndef _NOGSL
#include <gsl/gsl_vector.h>
#include <gsl/gsl_errno.h>
#include <gsl/gsl_blas.h>
#else
#include "FakeGSL.h"
#endif

#include <vector>

#include <fstream>
#include <iostream>
#include "BasicError.h"
//#define _NOSNOPT 1
#ifndef _NOSNOPT
#include "Snopt.h"
#else
#include "CustomSolver.h"
#endif


using namespace std;
typedef int (* MAX_SOLVER_DF_TYPE)(integer*, doublereal*, integer*, doublereal*, integer*, doublereal*, char*, integer*);

class MaxSolver {	
	integer n; integer neF; integer lenG;
	
	integer *iGfun;
	integer *jGvar;
	doublereal* G;
	
	doublereal *x;
	doublereal *xlow;
	doublereal *xupp;
	
	doublereal *F;
	doublereal *Flow;
	doublereal *Fupp;
	
	gsl_vector* result;
	double objectiveVal;

	MAX_SOLVER_DF_TYPE df;
	char* workspace;

	doublereal obj;
	doublereal* grad;
	doublereal* oldx;

	doublereal stepSize = 0.1;

	int maxSteps = 5000;
	
public:
	MaxSolver(integer n_, integer neF_): n(n_), neF(neF_){
		lenG = n*neF;
		iGfun = new integer[lenG];
		jGvar = new integer[lenG];
		G = new doublereal[lenG];

		grad = new doublereal[n];
		
		x = new doublereal[n];
		xlow = new doublereal[n];
		xupp = new doublereal[n];
		
		F = new doublereal[neF];
		Flow = new doublereal[neF];
		Fupp = new doublereal[neF];
				
		oldx = new doublereal[n];
		result = gsl_vector_alloc(n);
	}
	
	~MaxSolver() {
		delete []iGfun; delete []jGvar;
		delete []x; delete []xlow; delete []xupp;
		delete []F; delete []Flow; delete []Fupp;
	}
	
	void init(char* workspace, integer nef_, MAX_SOLVER_DF_TYPE _df, doublereal *xlow_, doublereal *xupp_, doublereal *Flow_, doublereal *Fupp_);

	bool optimize(const gsl_vector* initState, const gsl_vector* initDir, bool suppressPrint = false);
	
	gsl_vector* getResults() {
		return result;
	}
	
	double getObjectiveVal() {
		return objectiveVal;
	}

	bool inLimits();
	
};


