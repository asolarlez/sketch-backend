#pragma once
#include "BooleanDAG.h"
#include "Interface.h"
#include "SimpleEvaluator.h"
#include "SimpleGradEvaluator.h"


class ActualEvaluators {
	BooleanDAG* mainDag;
	map<BooleanDAG*, SimpleGradEvaluator*> evals;
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
		SimpleGradEvaluator* se = new SimpleGradEvaluator(*dag, ctrls);
		se->setInputs(interf);
		evals[dag] = se;
	}

	void removeEvaluator(BooleanDAG* dag) {
		delete(evals[dag]);
		evals.erase(dag);
	}

	SimpleGradEvaluator* getEvaluator(BooleanDAG* dag) {
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

	double grad_norm(int nodeid) {
		return evals[mainDag]->grad_norm(nodeid);
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
        	//cout << p->print() << ":" << e << " ";
        	if (e > error) {
        		error = e;
        	}
    	}
    	//cout << endl;
    	return error;
	}

	double computeError(BooleanDAG* dag, int nodeid) {
		return evals[dag]->dist(nodeid);
	}

	double computeErrorWithGrad(BooleanDAG* dag, int nodeid, gsl_vector* grad) {
		return evals[dag]->dist(nodeid, grad);
	}

	double grad_norm(BooleanDAG* dag, int nodeid) {
		return evals[dag]->grad_norm(nodeid);
	}
	const gsl_vector* grad(BooleanDAG* dag, int nodeid) {
		return evals[dag]->grad(nodeid);
	}

	double dist(Predicate* p) {
		return p->evaluate(this);
	}

	double grad_norm(Predicate* p) {
		return p->grad_norm(this);
	}

	const gsl_vector* grad(Predicate* p) {
		return p->grad(this);
	}

};