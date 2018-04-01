#pragma once
#include <gsl/gsl_vector.h>
#include "NumericalSolver.h"

class OptimizationWrapper {
public:
	virtual bool optimize(Interface* inputs, gsl_vector* initState, const set<int>& constraints, bool suppressPrint = false, int MAX_TRIES = 1) = 0;
	virtual gsl_vector* getMinState() = 0;
	virtual void randomizeCtrls(gsl_vector* x, Interface* inputs, set<int>& constraints) = 0;
	virtual double getObjectiveVal() = 0;
};
