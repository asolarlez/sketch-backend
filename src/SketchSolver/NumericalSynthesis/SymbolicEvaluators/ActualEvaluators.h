#pragma once
#include "BooleanDAG.h"
#include "Interface.h"
#include "SimpleEvaluator.h"


class ActualEvaluators {
	BooleanDAG* mainDag;
	map<BooleanDAG*, SimpleEvaluator*> evals;
	map<string, int>& ctrls;
	Interface* interf;
public:
	ActualEvaluators(BooleanDAG* dag, Interface* _interf, map<string, int>& _ctrls): mainDag(dag), ctrls(_ctrls), interf(_interf) {}
	~ActualEvaluators() {
		for (auto it = evals.begin(); it != evals.end(); it++) {
			delete it->second;
		}
	}

	void addEvaluator(BooleanDAG* dag) {
		Assert(evals.find(dag) == evals.end(), "This should not be possible");
		SimpleEvaluator* se = new SimpleEvaluator(*dag, ctrls);
		se->setInputs(interf);
		evals[dag] = se;
	}

	void removeEvaluator(BooleanDAG* dag) {
		delete(evals[dag]);
		evals.erase(dag);
	}

	SimpleEvaluator* getEvaluator(BooleanDAG* dag) {
		return evals[dag];
	}

	void run(const gsl_vector* state) {
		for (auto it = evals.begin(); it != evals.end(); it++) {
			it->second->run(state);
		}
	}

	double getErrorOnConstraint(int nodeid) {
		return evals[mainDag]->getErrorOnConstraint(nodeid);
	}

	double dist(int nodeid) {
		return evals[mainDag]->dist(nodeid);
	}

	double getErrorOnClause(IClause* clause) {
		double error = -1e30;

    	for (int i = 0; i < clause->size(); i++) {
        	Predicate* p = clause->getPredicate(i);
        	int val = clause->getVal(i);
        	double dist = p->evaluate(this);
        	double e;
        	if (val == 1) {
        		e = dist;
        	} else {
        		e = -dist;
        	}
        	if (e > error) {
        		error = e;
        	}
    	}
    	return error;
	}

	double computeError(BooleanDAG* dag, int nodeid) {
		return evals[dag]->dist(nodeid);
	}

	double dist(Predicate* p) {
		return p->evaluate(this);
	}

};