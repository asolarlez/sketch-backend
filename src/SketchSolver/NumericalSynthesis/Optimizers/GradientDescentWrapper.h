#pragma once
#include <fstream>
#include <iostream>
#include <vector>
#include <map>
#include <math.h>
#include "NumericalSolverHelper.h"
#include "GradientDescent.h"
#include "OptimizationWrapper.h"
#include "SymbolicEvaluator.h"
#include "BooleanNodes.h"
#include "BooleanDAG.h"
#include "Util.h"

using namespace std;


class GDParameters {
public:
	SymbolicEvaluator* eval;
	BooleanDAG* dag;
	vector<vector<int>>& allInputs;
	map<int, int>& imap;
	double beta;
	double alpha;
	
	GDParameters(SymbolicEvaluator* eval_, BooleanDAG* dag_, vector<vector<int>>& allInputs_, map<int, int>& imap_): eval(eval_), dag(dag_), allInputs(allInputs_), imap(imap_) {}
};

class GDEvaluator {
	static gsl_vector* curGrad;
public:
	
	static void init(int size) {
		curGrad = gsl_vector_alloc(size);
	}
	
	static double f(const gsl_vector* x, void* params) {
		GDParameters* p = (GDParameters*) params;
		GradUtil::BETA = p->beta;
		GradUtil::ALPHA = p->alpha;
		gsl_vector_set_zero(curGrad);
		float error = 0;
		for (int i = 0; i < p->allInputs.size(); i++) {
			const map<int ,int>& nodeValsMap = Util::getNodeToValMap(p->imap, p->allInputs[i]);
			p->eval->run(x, nodeValsMap);
			for (BooleanDAG::iterator node_it = p->dag->begin(); node_it != p->dag->end(); ++node_it) {
				bool_node* n = *node_it;
				if (n->type ==  bool_node::ASSERT) {
					error += p->eval->computeError(n->mother, 1, curGrad);
				}
				if (n->getOtype() == OutType::BOOL) {
					auto it = nodeValsMap.find(n->id);
					if (it != nodeValsMap.end()) {
						int val = it->second;
						error == p->eval->computeError(n, val, curGrad);
					}
				}
			}
		}
		return error;
	}
	
	static void df(const gsl_vector* x, void* params, gsl_vector* d) {
		// just use the curGrad - assuming that this function is called immediately after calling eval_f with the same arguments
		gsl_vector_memcpy(d, curGrad);
	}
	
	static void fdf(const gsl_vector* x, void* params, double* v, gsl_vector* g) {
		*v = f(x, params);
		df(x, params, g);
	}
};

class GradientDescentWrapper: public OptimizationWrapper {
	NumericalSolverHelper* ns;
	GradientDescent* gd;
	float threshold = 1e-5; // accuracy for minimizing the error
	int MAX_TRIES = 9; // Number retries of GD algorithm for each iteration
	float minErrorSoFar;
	gsl_vector* minState;
	gsl_vector* t;
	int ncontrols;
	SymbolicEvaluator* eval;
	BooleanDAG* dag;
	map<int, int>& imap;
	
public:
	GradientDescentWrapper(NumericalSolverHelper* ns_, SymbolicEvaluator* eval_, BooleanDAG* dag_, map<int, int>& imap_, int ncontrols_): ns(ns_), eval(eval_), dag(dag_), imap(imap_), ncontrols(ncontrols_) {
		GDEvaluator::init(ncontrols);
		gd = new GradientDescent(ncontrols);
		minState = gsl_vector_alloc(ncontrols);
		t = gsl_vector_alloc(ncontrols);
	}
	
	virtual bool optimize(vector<vector<int>>& allInputs, gsl_vector* initState) {
		minErrorSoFar = 1e50;
		
		GDParameters* p = new GDParameters(eval, dag, allInputs, imap);
		gd->init(GDEvaluator::f, GDEvaluator::df, GDEvaluator::fdf, p);
		
		double betas[4] = {-1, -10, -50, -100};
		double alphas[4] = {1, 10, 50, 100};
		
		gsl_vector_memcpy(t, initState);

		
		double minError = 1e10;
		int numtries = 0;
		while (minError > threshold && numtries < MAX_TRIES) {
			cout << "Attempt: " << (numtries + 1) << endl;
			
			for (int i = 0; i < 4; i++) {
				cout << "Beta: " << betas[i] << " Alpha: " << alphas[i] << endl;
				p->beta = betas[i];
				p->alpha = alphas[i];
				for (int i = 0; i < ncontrols; i++) {
					cout << gsl_vector_get(t, i) << ", ";
				}
				minError = gd->optimize(t);
				gsl_vector_memcpy(t, gd->getResults());
			}
			if (minError < minErrorSoFar) {
				minErrorSoFar = minError;
				gsl_vector_memcpy(minState, gd->getResults());
			}
			numtries++;
			ns->randomizeCtrls(t);
		}
		return minError <= threshold;
	}
	
	virtual gsl_vector* getMinState() {
		return minState;
	}
};
