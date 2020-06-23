#pragma once



#include <map>
#include <set>


#ifdef _NOGSL
class gsl_vector;
#else
#include <gsl/gsl_vector.h>
#endif

class Interface;

class SymbolicEvaluator {
	double otherError = 1;
public:
	virtual void setInputs(Interface* inputValues_p) = 0;
    
    virtual void run(const gsl_vector* ctrls_p, const set<int>& nodesSubset) = 0;
    virtual void run(const gsl_vector* ctrls_p) = 0;
    
    virtual double getErrorOnConstraint(int nodeid, gsl_vector* grad) = 0;
    virtual double getErrorOnConstraint(int nodeid) = 0;

    virtual double getErrorForAsserts(const set<int>& nodeid, gsl_vector* grad) = 0;
	virtual double getErrorForAssert(int assertId, gsl_vector* grad)=0;

    virtual double dist(int nid) = 0;
    virtual double dist(int nid, gsl_vector* grad) = 0;


    
	virtual void print() = 0;
	virtual void printFull() = 0;

	void resetOtherError() {
		otherError = 1;
	}

	void setOtherError(double error) {
		otherError = error;
	}

	double getOtherError() {
		return otherError;
	}

	bool isSetOtherError() {
		return otherError != 1;
	}
};
