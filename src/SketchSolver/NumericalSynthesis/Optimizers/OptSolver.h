#pragma once


#ifdef _NOSNOPT
typedef int integer;
typedef double doublereal;
#else 
#include "snopt.hh"
#include "snoptProblem.hh"
#endif

typedef float(*floatfun)(float);

typedef int(DFT)(integer*, integer *, doublereal [],
	integer    *, integer *, doublereal [],
	integer    *, integer *, doublereal [],
	char      *, integer *,
	integer    [], integer *,
	doublereal [], integer *);

class OptSolver {
public:
	virtual void init(char* workspace, integer nef_, DFT df, integer ObjRow, doublereal ObjAdd, doublereal *xlow_, doublereal *xupp_, doublereal *Flow_, doublereal *Fupp_) = 0;
	virtual bool optimize(gsl_vector* initState, bool suppressPrint = false) = 0;
	
	virtual gsl_vector* getResults() = 0;
	
	virtual double getObjectiveVal() = 0;
};
