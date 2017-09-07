#pragma once
#include <iostream>
#include <vector>
#include <map>
#include "Snopt.h"
#include "OptimizationWrapper.h"
#include "NumericalSolverHelper.h"
#include "SymbolicEvaluator.h"
#include "BooleanNodes.h"
#include "BooleanDAG.h"
#include "Util.h"

using namespace std;


class SnoptParameters {
public:
	SymbolicEvaluator* eval;
	BooleanDAG* dag;
	vector<vector<int>>& allInputs;
	map<int, int>& imap;
	set<int>& boolNodes;
	double beta;
	double alpha;
	
	SnoptParameters(SymbolicEvaluator* eval_, BooleanDAG* dag_, vector<vector<int>>& allInputs_, map<int, int>& imap_, set<int>& boolNodes_): eval(eval_), dag(dag_), allInputs(allInputs_), imap(imap_), boolNodes(boolNodes_) {}
};

class SnoptEvaluator {
	static gsl_vector* state;
	static gsl_vector* grad;
public:
	static void init(int size) {
		state = gsl_vector_alloc(size);
		grad = gsl_vector_alloc(size);
	}
	
	static int df(integer    *Status, integer *n,    doublereal x[],
								integer    *needF,  integer *neF,  doublereal F[],
								integer    *needG,  integer *neG,  doublereal G[],
								char      *cu,			 integer *lencu,
								integer    iu[],    integer *leniu,
								doublereal ru[],    integer *lenru) {
		SnoptParameters* p = (SnoptParameters*) cu;
		GradUtil::BETA = p->beta;
		GradUtil::ALPHA = p->alpha;
		
		for (int i =0 ; i < *n; i++) {
			gsl_vector_set(state, i, x[i]);
		}
		int fcounter = 0;
		int gcounter = 0;
		F[fcounter++] = 0; // objective
		for (int j = 0; j < *n; j++) { // objective gradients
			G[gcounter++] = 0;
		}
		for (int i = 0; i < p->allInputs.size(); i++) {
			const map<int, int>& nodeValsMap = Util::getNodeToValMap(p->imap, p->allInputs[i]);
			p->eval->run(state, nodeValsMap);
			for (BooleanDAG::iterator node_it = p->dag->begin(); node_it != p->dag->end(); node_it++) {
				bool_node* node = *node_it;
				if (p->boolNodes.find(node->id) != p->boolNodes.end()) {
					if (node->type == bool_node::ASSERT) {
						F[fcounter++] = p->eval->computeDist(node->mother, grad);
						for (int j = 0; j < *n; j++) {
							G[gcounter++] = gsl_vector_get(grad, j);
						}
					}
					if (node->getOtype() == OutType::BOOL) {
						auto it = nodeValsMap.find(node->id);
						if (it != nodeValsMap.end()) {
							int val = it->second;
							double dist = p->eval->computeDist(node, grad);
							if (val == 0) {
								dist = -dist;
								gsl_vector_scale(grad, -1.0);
							}
							F[fcounter++] = dist;
							for (int j = 0; j < *n; j++) {
								G[gcounter++] = gsl_vector_get(grad, j);
							}
						} else {
							F[fcounter++] = 1000;
							for (int j = 0; j < *n; j++) {
								G[gcounter++] = 0;
							}
						}
					}
				}
			}
		}
		return 0;
	}

};

class SnoptWrapper: public OptimizationWrapper {
	SnoptSolver* snoptSolver;
	integer n; integer neF; integer lenA;
	
	NumericalSolverHelper* ns;
	SymbolicEvaluator* eval;
	BooleanDAG* dag;
	map<int, int>& imap;
	map<string, int>& ctrlMap;
	set<int>& boolNodes;
	gsl_vector* minState;
	gsl_vector* t;
	
	int MAX_TRIES = 3;
	
	doublereal* x;
	doublereal* xlow;
	doublereal* xupp;
	
	doublereal* Flow;
	doublereal* Fupp;
	
	
	void getxranges() {
		vector<bool_node*>& ctrls = dag->getNodesByType(bool_node::CTRL);
		int counter = 0;
		for (int i = 0; i < ctrls.size(); i++) {
			if (ctrlMap.find(ctrls[i]->get_name()) != ctrlMap.end()) {
				int idx = ctrlMap[ctrls[i]->get_name()];
				CTRL_node* cnode = (CTRL_node*) ctrls[i];
				xlow[idx] = cnode->hasRange ? cnode->low : -1e20;
				xupp[idx] = cnode->hasRange ? cnode->high : 1e20;
				counter++;
			}
		}
		
		if (counter != n) {
			Assert(n == 1, "Missing initialization of some variables");
			// this can happen if there are no actual controls
			xlow[0] = -1e20;
			xupp[0] = 1e20;
		}
	}

	void getFranges() {
		Flow[0] = -1e20;
		Fupp[0] = 1e20;
		for (int i = 1; i < neF; i++) {
			Flow[i] = 0;
			Fupp[i] = 1e20;
		}
	}
	
public:
	SnoptWrapper(SymbolicEvaluator* eval_, BooleanDAG* dag_, map<int, int>& imap_, map<string, int>& ctrlMap_, set<int>& boolNodes_, int ncontrols_): eval(eval_), dag(dag_), imap(imap_), ctrlMap(ctrlMap_), boolNodes(boolNodes_), n(ncontrols_)  {
		SnoptEvaluator::init(n);
		neF = boolNodes.size() + 1;
		lenA = 10;
		snoptSolver = new SnoptSolver(n, neF, lenA);
		minState = gsl_vector_alloc(n);
		t = gsl_vector_alloc(n);
		
		x = new doublereal[n];
		xlow = new doublereal[n];
		xupp = new doublereal[n];
		
		Flow = new doublereal[neF];
		Fupp = new doublereal[neF];
		
		getxranges();
		getFranges();
	}
	
	virtual bool optimize(vector<vector<int>>& allInputs, gsl_vector* initState) {
		
		SnoptParameters* p = new SnoptParameters(eval, dag, allInputs, imap, boolNodes);
		snoptSolver->init((char *) p, neF, SnoptEvaluator::df, 0, 0.0, xlow, xupp, Flow, Fupp);
		
		double betas[4] = {-1, -10, -50, -100};
		double alphas[4] = {1, 10, 50, 100};
		
		gsl_vector_memcpy(t, initState);
		
		bool solved = false;
		int numtries = 0;
		while (!solved && numtries < MAX_TRIES) {
			cout << "Attempt: " << (numtries + 1) << endl;
			
			for (int i = 0; i < 4; i++) {
				cout << "Beta: " << betas[i] << " Alpha: " << alphas[i] << endl;
				p->beta = betas[i];
				p->alpha = alphas[i];
				for (int i = 0; i < n; i++) {
					cout << gsl_vector_get(t, i) << ", ";
				}
				cout << endl;
				solved = snoptSolver->optimize(t);
				gsl_vector_memcpy(t, snoptSolver->getResults());
			}
			
			if (solved) {
				gsl_vector_memcpy(minState, snoptSolver->getResults());
			}
			numtries++;
			randomizeCtrls(t);
		}
		return solved;
	}
	
	virtual gsl_vector* getMinState() {
		return minState;
	}
	
	virtual void randomizeCtrls(gsl_vector* state) {
		vector<bool_node*>& ctrls = dag->getNodesByType(bool_node::CTRL);
		int counter = 0;
		for (int i = 0; i < ctrls.size(); i++) {
			if (ctrlMap.find(ctrls[i]->get_name()) != ctrlMap.end()) {
				int idx = ctrlMap[ctrls[i]->get_name()];
				CTRL_node* cnode = (CTRL_node*) ctrls[i];
				double low = cnode->hasRange ? cnode->low : -10.0;
				double high = cnode->hasRange ? cnode->high : 10.0;
				float r = low + (rand()% (int)((high - low) * 10.0))/10.0;
				gsl_vector_set(state, idx, r);
				counter++;
			}
		}
		
		if (counter != n) {
			Assert(n == 1, "Missing initialization of some variables");
			// this can happen if there are no actual controls
			float r = -10.0 + (rand() % 200)/10.0;
			gsl_vector_set(state, 0, r);
		}
	}

};
