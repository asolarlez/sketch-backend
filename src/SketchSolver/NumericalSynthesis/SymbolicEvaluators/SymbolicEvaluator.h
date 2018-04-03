#pragma once
#include <gsl/gsl_vector.h>
#include <map>
#include <set>
#include "Interface.h"

class SymbolicEvaluator {
public:
    virtual void setInputs(Interface* inputValues_p);
    
    virtual void run(const gsl_vector* ctrls_p, const set<int>& nodesSubset);
    virtual void run(const gsl_vector* ctrls_p);
    
    virtual double getErrorOnConstraint(int nodeid, gsl_vector* grad);
    virtual double getErrorOnConstraint(int nodeid);
    
	virtual void print() = 0;
	virtual void printFull() = 0;

};
