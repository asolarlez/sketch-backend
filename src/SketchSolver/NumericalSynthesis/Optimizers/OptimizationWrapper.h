#pragma once
#include <gsl/gsl_vector.h>
#include "NumericalSolverHelper.h"

class OptimizationWrapper {
public:
	virtual bool optimize(vector<vector<int>>& allInputs, gsl_vector* initState) = 0;
	virtual gsl_vector* getMinState() = 0;
	virtual void randomizeCtrls(gsl_vector* x) = 0;
	virtual double getObjectiveVal() = 0;
};
