#pragma once
#include <gsl/gsl_vector.h>
#include <map>
#include <set>

class SymbolicEvaluator {
public:
	virtual void run(const gsl_vector* ctrls_p, const map<int, int>& inputValues_p) = 0;
	virtual double errorGD(gsl_vector* d) = 0;
	virtual bool check(bool_node* n, int expected) = 0;
	//virtual set<int> getConflicts(int nid) = 0;
};
