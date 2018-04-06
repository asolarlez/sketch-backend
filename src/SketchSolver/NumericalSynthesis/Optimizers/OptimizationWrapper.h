#pragma once

#include "NumericalSolverHelper.h"


class gsl_vector;

class OptimizationWrapper {
public:
	virtual bool optimize(vector<vector<int>>& allInputs, gsl_vector* initState, bool suppressPrint = false) = 0;
    virtual bool optimizeForInputs(vector<vector<int>>& allInputs, gsl_vector* initState, bool suppressPrint = false) = 0;
	virtual gsl_vector* getMinState() = 0;
	virtual void randomizeCtrls(gsl_vector* x, vector<vector<int>>&) = 0;
	virtual double getObjectiveVal() = 0;
};
