#pragma once


#include <set>
#ifndef _NOGSL
#include <gsl/gsl_vector.h>
#endif
using namespace std;

class Interface;

#ifdef _NOGSL
class gsl_vector;
#endif


class LocalState {
public:
	vector<gsl_vector*> localSols; // local sol for different betas
	vector<double> errors;

	LocalState(int ncontrols, int nbetas) {
		for (int i = 0; i < nbetas; i++) {
			localSols.push_back(gsl_vector_alloc(ncontrols));
		}
		errors.resize(nbetas);
	}
	~LocalState() {
		for (int i = 0; i < localSols.size(); i++) {
			gsl_vector_free(localSols[i]);
		}
	}

	void print() {
		cout << "beta = -1:  " << Util::print(localSols[0]) << endl;
		cout << "beta = -10: " << Util::print(localSols[1]) << endl;
		cout << "beta = -50: " << Util::print(localSols[2]) << endl;
	}
};

class OptimizationWrapper {
public:
	virtual bool optimize(Interface* inputs, gsl_vector* initState, const set<int>& constraints, int minimizeNode, bool suppressPrint, int MAX_TRIES, LocalState* localState, int level = -1) = 0;
	virtual gsl_vector* getMinState() = 0;
	virtual void randomizeCtrls(gsl_vector* x, Interface* inputs, const set<int>& constraints, int minimizeNode) = 0;
	virtual double getObjectiveVal() = 0;
	virtual bool maximize(Interface* inputs, const gsl_vector* initState, const set<int>& assertConstraints, int minimizeNode, float beta, Predicate* pred, int predVal, int level) = 0;
};
