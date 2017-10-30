#pragma once
#include <fstream>
#include <iostream>
#include "BasicError.h"
#include <gsl/gsl_vector.h>
#include "snopt.hh"
#include "snoptProblem.hh"

using namespace std;
typedef int (* SNOPT_DF_TYPE)(integer*,	integer*, doublereal*, integer*, integer*, doublereal*, integer*, integer*, doublereal*, char*, integer*, integer*, integer*, doublereal*, integer*);

class SnoptSolver {
	snoptProblem snoptProb;
	
	integer n; integer neF; integer lenA; integer lenG;
	
	integer *iAfun;
	integer *jAvar;
	doublereal *A;
	
	integer *iGfun;
	integer *jGvar;
	
	doublereal *x;
	doublereal *xlow;
	doublereal *xupp;
	doublereal *xmul;
	integer *xstate;
	
	doublereal *F;
	doublereal *Flow;
	doublereal *Fupp;
	doublereal *Fmul;
	integer *Fstate;
	
	integer nxnames;
	integer nFnames = 1;
	char *xnames;
	char *Fnames;
	
	gsl_vector* result;
	double objectiveVal;
	
	integer Cold = 0, Basis = 1, Warm = 2;
public:
	SnoptSolver(integer n_, integer neF_, integer lenA_): n(n_), neF(neF_), lenA(lenA_) {
		iAfun = new integer[lenA];
		jAvar = new integer[lenA];
		A = new doublereal[lenA];
		
		lenG = n*neF;
		iGfun = new integer[lenG];
		jGvar = new integer[lenG];
		
		x = new doublereal[n];
		xlow = new doublereal[n];
		xupp = new doublereal[n];
		xmul = new doublereal[n];
		xstate = new integer[n];
		
		F = new doublereal[neF];
		Flow = new doublereal[neF];
		Fupp = new doublereal[neF];
		Fmul = new doublereal[neF];
		Fstate = new integer[neF];
		
		nxnames = 1;
		nFnames = 1;
		xnames = new char[nxnames*8];
		Fnames = new char[nFnames*8];
		
		snoptProb.setXNames(xnames, nxnames);
		snoptProb.setFNames(Fnames, nFnames);
		
		result = gsl_vector_alloc(n);
	}
	
	~SnoptSolver() {
		delete []iAfun; delete []jAvar; delete []A;
		delete []iGfun; delete []jGvar;
		delete []x; delete []xlow; delete []xupp;
		delete []xmul; delete []xstate;
		delete []F; delete []Flow; delete []Fupp;
		delete []Fmul; delete []Fstate;
		delete []xnames; delete []Fnames;
	}
	
	void init(char* workspace, integer nef_, SNOPT_DF_TYPE df, integer ObjRow, doublereal ObjAdd, doublereal *xlow_, doublereal *xupp_, doublereal *Flow_, doublereal *Fupp_);
	bool optimize(gsl_vector* initState, bool suppressPrint = false);
	
	gsl_vector* getResults() {
		return result;
	}
	
	double getObjectiveVal() {
		return objectiveVal;
	}
	
};
