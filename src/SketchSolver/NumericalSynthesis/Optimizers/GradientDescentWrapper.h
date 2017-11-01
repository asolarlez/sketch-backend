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
	set<int>& boolNodes;
	double beta;
	double alpha;
	
	GDParameters(SymbolicEvaluator* eval_, BooleanDAG* dag_, vector<vector<int>>& allInputs_, map<int, int>& imap_, set<int>& boolNodes_): eval(eval_), dag(dag_), allInputs(allInputs_), imap(imap_), boolNodes(boolNodes_) {}
};

class GDEvaluator {
	static gsl_vector* curGrad;
public:
	
	static void init(int size) {
		curGrad = gsl_vector_alloc(size);
	}
	
	static double f(const gsl_vector* x, void* params) {
        Assert(false, "Does not handle sqrt constraints yet");
		GDParameters* p = (GDParameters*) params;
		GradUtil::BETA = p->beta;
		GradUtil::ALPHA = p->alpha;
		gsl_vector_set_zero(curGrad);
		double error = 0;
		for (int i = 0; i < p->allInputs.size(); i++) {
			const map<int ,int>& nodeValsMap = Util::getNodeToValMap(p->imap, p->allInputs[i]);
			p->eval->run(x, nodeValsMap);
			for (BooleanDAG::iterator node_it = p->dag->begin(); node_it != p->dag->end(); ++node_it) {
				bool_node* n = *node_it;
				if (p->boolNodes.find(n->id) != p->boolNodes.end()) {
					if (n->type ==  bool_node::ASSERT) {
						error += p->eval->computeError(n->mother, 1, curGrad);
					} else if (n->getOtype() == OutType::BOOL) {
						auto it = nodeValsMap.find(n->id);
						if (it != nodeValsMap.end()) {
							int val = it->second;
							error += p->eval->computeError(n, val, curGrad);
						}
					}
				}
			}
		}
		//cout << gsl_vector_get(x, 0) << " " << error << " " << gsl_vector_get(curGrad, 0) << endl;
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
	GradientDescent* gd;
	double threshold = 1e-2; // accuracy for minimizing the error
	int MAX_TRIES = 1; // Number retries of GD algorithm for each iteration
	double minErrorSoFar;
	gsl_vector* minState;
	gsl_vector* t;
	int ncontrols;
	SymbolicEvaluator* eval;
	BooleanDAG* dag;
	map<int, int>& imap;
	map<string, int>& ctrlMap;
	set<int>& boolNodes;
	
public:
	GradientDescentWrapper(SymbolicEvaluator* eval_, BooleanDAG* dag_, map<int, int>& imap_, map<string, int>& ctrlMap_, set<int>& boolNodes_, int ncontrols_): eval(eval_), dag(dag_), imap(imap_), ctrlMap(ctrlMap_), boolNodes(boolNodes_), ncontrols(ncontrols_) {
		GDEvaluator::init(ncontrols);
		gd = new GradientDescent(ncontrols);
		minState = gsl_vector_alloc(ncontrols);
		t = gsl_vector_alloc(ncontrols);
	}
	
	virtual void randomizeCtrls(gsl_vector* state) {
		vector<bool_node*>& ctrls = dag->getNodesByType(bool_node::CTRL);
		int counter = 0;
		for (int i = 0; i < ctrls.size(); i++) {
			if (ctrlMap.find(ctrls[i]->get_name()) != ctrlMap.end()) {
				int idx = ctrlMap[ctrls[i]->get_name()];
				CTRL_node* cnode = (CTRL_node*) ctrls[i];
				double low = cnode->hasRange ? cnode->low : -20.0;
				double high = cnode->hasRange ? cnode->high : 20.0;
				double r = low + (rand()% (int)((high - low) * 10.0))/10.0;
				gsl_vector_set(state, idx, r);
				counter++;
			}
		}
        for (; counter < ncontrols; counter++) {
            double r = -10.0 + (rand() % 200)/10.0;
            gsl_vector_set(state, counter, r);
        }
	}
	
	virtual bool optimize(vector<vector<int>>& allInputs, gsl_vector* initState, bool suppressPrint = false) {
		minErrorSoFar = 1e50;
		
		GDParameters* p = new GDParameters(eval, dag, allInputs, imap, boolNodes);
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
				cout << endl;
				minError = gd->optimize(t);
				gsl_vector_memcpy(t, gd->getResults());
			}
			if (minError < minErrorSoFar) {
				minErrorSoFar = minError;
				gsl_vector_memcpy(minState, gd->getResults());
			}
			numtries++;
			randomizeCtrls(t);
		}
		return minError <= threshold;
	}
	
	virtual gsl_vector* getMinState() {
		return minState;
	}
	
	virtual double getObjectiveVal() {
		return minErrorSoFar;
	}
};
