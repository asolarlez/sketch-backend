#pragma once

#include "NumericalSolver.h"


class gsl_vector;

class OptimizationWrapper {
public:
	virtual bool optimize(Interface* inputs, gsl_vector* initState, const set<int>& constraints, int minimizeNode, bool suppressPrint = false, int MAX_TRIES = 1, bool initRandomize = false) = 0;
	virtual gsl_vector* getMinState() = 0;
	virtual void randomizeCtrls(gsl_vector* x, Interface* inputs, const set<int>& constraints, int minimizeNode) = 0;
	virtual double getObjectiveVal() = 0;
};
