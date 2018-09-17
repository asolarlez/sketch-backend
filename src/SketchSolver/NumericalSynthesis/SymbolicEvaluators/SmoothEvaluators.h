#pragma once
#include "BooleanDAG.h"
#include "Interface.h"
#include "BoolAutoDiff.h"
#include "SmoothAutoDiff.h"
#include "KLocalityAutoDiff.h"


class SmoothEvaluators {
	BooleanDAG* mainDag;
	map<BooleanDAG*, SymbolicEvaluator*> evals;
	map<string, int>& ctrls;
	Interface* interf;
	int MAX_CLAUSE_VALS = 30;
	vector<gsl_vector*> clauseGrads;
public:
	SmoothEvaluators(BooleanDAG* dag, Interface* _interf, map<string, int>& _ctrls, int ncontrols): mainDag(dag), ctrls(_ctrls), interf(_interf) {
		for (int i = 0; i < MAX_CLAUSE_VALS; i++) {
        	clauseGrads.push_back(gsl_vector_alloc(ncontrols));
    	}
	}

	~SmoothEvaluators() {
		for (auto it = evals.begin(); it != evals.end(); it++) {
			delete it->second;
		}
		for (int i = 0; i < clauseGrads.size(); i++) {
        	gsl_vector_free(clauseGrads[i]);
    	}
	}

	void addEvaluator(BooleanDAG* dag) {
		Assert(evals.find(dag) == evals.end(), "This should not be possible");
		SymbolicEvaluator* se;
		if (PARAMS->smoothingMode == 0) {
			se = new BoolAutoDiff(*dag, ctrls);
		} else if (PARAMS->smoothingMode == 1) {
			se = new SmoothAutoDiff(*dag, ctrls);
		} else {
			se = new KLocalityAutoDiff(*dag, ctrls);
		}
		se->setInputs(interf);
		evals[dag] = se;
	}

	void removeEvaluator(BooleanDAG* dag) {
		delete(evals[dag]);
		evals.erase(dag);
	}

	void run(const gsl_vector* state) {
		for (auto it = evals.begin(); it != evals.end(); it++) {
			it->second->run(state);
		}
	}

	void run(BooleanDAG* dag, const gsl_vector* state) {
		evals[dag]->run(state);
	}

	double getErrorOnConstraint(int nodeid) {
		return evals[mainDag]->getErrorOnConstraint(nodeid);
	}

	double getErrorOnConstraint(int nodeid, gsl_vector* grad) {
		return evals[mainDag]->getErrorOnConstraint(nodeid, grad);
	}

	double getErrorOnPredicate(Predicate* p, int val) {
		double dist = p->evaluate(this);
		double error;
		if (val == 1) {
			error = dist;
		} else {
			error = -dist;
		}
		return error;
	}

	double getErrorOnPredicate(Predicate* p, int val, gsl_vector* grad) {
		double dist = p->evaluate(grad, this);
		double error;
		if (val == 1) {
			error = dist;
		} else {
			error = -dist;
			gsl_vector_scale(grad, -1);
		}
		return error;
	}

	double getErrorOnClause(IClause* clause) {
		vector<double> vals;
    	for (int i = 0; i < clause->size(); i++) {
        	Predicate* p = clause->getPredicate(i);
        	int val = clause->getVal(i);
        	double error = getErrorOnPredicate(p, val);
        	vals.push_back(error);
    	}
    	return GradUtil::findMax(vals);
	}

	double getErrorOnClause(IClause* clause, gsl_vector* grad) {
		GradUtil::default_grad(grad);
    	Assert(clause->size() <= MAX_CLAUSE_VALS, "Clause size exceeded limit");

    	vector<double> vals;
    	vector<gsl_vector*> grads;
    	for (int i = 0; i < clause->size(); i++) {
        	Predicate* p = clause->getPredicate(i);
        	int val = clause->getVal(i);
        	double error = getErrorOnPredicate(p, val, clauseGrads[i]);
        	vals.push_back(error);
        	grads.push_back(clauseGrads[i]);
    	}
    	return GradUtil::findMax(vals, grads, grad);
	}

	double computeErrorWithGrad(BooleanDAG* dag, int nodeid, gsl_vector* grad) {
		return evals[dag]->dist(nodeid, grad);
	}

	double computeError(BooleanDAG* dag, int nodeid) {
		return evals[dag]->dist(nodeid);
	}

	double dist(Predicate* p) {
		return p->evaluate(this);
	}

	string getAssertMsg(int nodeid) {
		ASSERT_node* an = (ASSERT_node*)((*mainDag)[nodeid]);
		string msg = an->getMsg();
		return Util::split(msg, " ").back();
	}

	
};