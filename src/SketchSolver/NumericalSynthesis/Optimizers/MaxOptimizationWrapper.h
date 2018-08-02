#pragma once

#ifndef _NOGSL
#include <gsl/gsl_vector.h>
#endif

class MaxOptimizationWrapper {
public:
	virtual gsl_vector* getMinState() = 0;
	virtual double getObjectiveVal() = 0;
	virtual bool maximize(Interface* inputs, const gsl_vector* initState, const set<int>& assertConstraints, int minimizeNode, float beta, int level, int id) = 0;
};
