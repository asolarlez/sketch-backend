#pragma once



#include <map>
#include <set>
#include "Interface.h"

class gsl_vector;


class SymbolicEvaluator {
public:
    virtual void setInputs(Interface* inputValues_p) = 0;
    
    virtual void run(const gsl_vector* ctrls_p, const set<int>& nodesSubset) = 0;
    virtual void run(const gsl_vector* ctrls_p) = 0;
    
    virtual double getErrorOnConstraint(int nodeid, gsl_vector* grad) = 0;
    virtual double getErrorOnConstraint(int nodeid) = 0;
    
	virtual void print() = 0;
	virtual void printFull() = 0;

};
