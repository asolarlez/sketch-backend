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
    static int counter;
	static void init(int size) {
		state = gsl_vector_alloc(size);
		grad = gsl_vector_alloc(size);
        //tmpGrad = gsl_vector_alloc(size);
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
        //cout << counter++ << endl;
		//cout << "x: ";
		for (int i =0 ; i < *n; i++) {
			gsl_vector_set(state, i, x[i]);
           // cout << x[i] << " ";
		}
		//cout << endl;
		int fcounter = 0;
		int gcounter = 0;
		F[fcounter++] = 0; // objective
		for (int j = 0; j < *n; j++) { // objective gradients
			G[gcounter++] = 0;
		}
        //double error = 0;
        //GradUtil::default_grad(tmpGrad);
        
        int bcounter = 0;
        for (int i = 0; i < p->allInputs.size(); i++) {
            const map<int, int>& nodeValsMap = Util::getNodeToValMap(p->imap, p->allInputs[i]);
            p->eval->run(state, nodeValsMap);
            if (PARAMS->useSnoptUnconstrained) {
                double error = 0.0;
                GradUtil::default_grad(grad);
                for (BooleanDAG::iterator node_it = p->dag->begin(); node_it != p->dag->end(); ++node_it) {
                    bool_node* n = *node_it;
                    if (p->boolNodes.find(n->id) != p->boolNodes.end() || Util::isSqrt(n)) {
                        if (Util::isSqrt(n)) {
                            error += p->eval->computeSqrtError(n, grad);
                        } else if (n->type ==  bool_node::ASSERT) {
                            error += p->eval->computeError(n->mother, 1, grad);
                        } else if (n->getOtype() == OutType::BOOL) {
                            auto it = nodeValsMap.find(n->id);
                            if (it != nodeValsMap.end()) {
                                int val = it->second;
                                error += p->eval->computeError(n, val, grad);
                            }
                        }
                    }
                }
                F[0] = error;
                for (int j = 0; j < *n; j++) {
                    G[j] = gsl_vector_get(grad, j);
                }
            } else {
                //p->eval->print();
                for (BooleanDAG::iterator node_it = p->dag->begin(); node_it != p->dag->end(); node_it++) {
                    bool_node* node = *node_it;
                    if (p->boolNodes.find(node->id) != p->boolNodes.end() || (Util::isSqrt(node)) || (node->type == bool_node::ASSERT && ((ASSERT_node*)node)->isHard())) {
                        if (Util::isSqrt(node)) {
                            double dist = 1000;
                            if (p->eval->hasSqrtDist(node)) {
                                dist = p->eval->computeSqrtDist(node, grad);
                            } else {
                                GradUtil::default_grad(grad);
                            }
                            F[fcounter++] = dist;
                            for (int j = 0; j < *n; j++) {
                                G[gcounter++] = gsl_vector_get(grad, j);
                            }
                        } else if (node->type == bool_node::ASSERT) {
                            int fidx;
                            int gidx;
                            if (((ASSERT_node*)node)->isHard()) {
                                fidx = 0;
                                gidx = 0;
                            } else {
                                fidx = fcounter++;
                                gidx = gcounter;
                                gcounter += *n;
                            }
                            double dist = 0;
                            if (p->eval->hasDist(node->mother)) {
                                dist = p->eval->computeDist(node->mother, grad);
                            } else {
                                dist = (fidx == 0) ? 0: 1000;
                                GradUtil::default_grad(grad);
                            }
                            F[fidx] = dist;
                            //cout << node->mother->lprint() << " " << dist << endl;
                            for (int j = 0; j < *n; j++) {
                                G[gidx + j] = gsl_vector_get(grad, j);
                            }
                        } else if (node->getOtype() == OutType::BOOL) {
                            auto it = nodeValsMap.find(node->id);
                            if (it != nodeValsMap.end()) {
                                int val = it->second;
                                double dist = 0;
                                if (p->eval->hasDist(node)) {
                                    dist = p->eval->computeDist(node, grad);
                                } else {
                                    dist = (val == 0) ? -1000 : 1000;
                                    GradUtil::default_grad(grad);
                                }
                                if (val == 0) {
                                    dist = -dist;
                                    gsl_vector_scale(grad, -1.0);
                                }
                                F[fcounter++] = dist;
                                for (int j = 0; j < *n; j++) {
                                    G[gcounter++] = gsl_vector_get(grad, j);
                                }
                            } else {
                                //F[fcounter++] = 1000;
                                //for (int j = 0; j < *n; j++) {
                                //	G[gcounter++] = 0;
                                //}
                            }
                        }
                    }
                }
            }
        }
        //cout << F[0] << endl;
        //cout << fcounter << " " << *neF << endl;
        for (int i = fcounter; i < *neF; i++) {
            F[i] = 1000;
            for (int j = 0; j < *n; j++) {
                G[gcounter++] = gsl_vector_get(grad, j);
            }
        }
        Assert(fcounter <= *neF, "dfqeurq");
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
	
	int MAX_TRIES = 1;
	
	doublereal* x;
	doublereal* xlow;
	doublereal* xupp;
	
	doublereal* Flow;
	doublereal* Fupp;
	
	double minObjectiveVal;
	double threshold = 0.01;
	
	
	void getxranges() {
		vector<bool_node*>& ctrls = dag->getNodesByType(bool_node::CTRL);
		int counter = 0;
		for (int i = 0; i < ctrls.size(); i++) {
			if (ctrlMap.find(ctrls[i]->get_name()) != ctrlMap.end()) {
				int idx = ctrlMap[ctrls[i]->get_name()];
				CTRL_node* cnode = (CTRL_node*) ctrls[i];
				xlow[idx] = cnode->hasRange ? cnode->low : -20.0;
				xupp[idx] = cnode->hasRange ? cnode->high : 20.0;
				counter++;
			}
		}
        for (; counter < n; counter++) {
            xlow[counter] = 0;
            xupp[counter] = 1;
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
	SnoptWrapper(SymbolicEvaluator* eval_, BooleanDAG* dag_, map<int, int>& imap_, map<string, int>& ctrlMap_, set<int>& boolNodes_, int ncontrols_, int numConstraints_): eval(eval_), dag(dag_), imap(imap_), ctrlMap(ctrlMap_), boolNodes(boolNodes_), n(ncontrols_)  {
		SnoptEvaluator::init(n);
        if (PARAMS->useSnoptUnconstrained) {
            neF = 1;
        } else {
            neF = numConstraints_ + 1;
        }
		lenA = 10; // we don't use this currently
		
		cout << "nef: " << neF << endl;
		cout << "n: " << n << endl;
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
	
	virtual bool optimize(vector<vector<int>>& allInputs, gsl_vector* initState, bool suppressPrint = false) {
        /*// select random subset of bool nodes if we don't have the budget for all constraints
        // TODO: this should be done in a cegis like loop - otherwise it is not sound
        vector<int> boolConstraints;
        vector<int> sqrtConstraints;
        const map<int, int>& nodeValsMap = Util::getNodeToValMap(imap, allInputs[0]);
        for (BooleanDAG::iterator node_it = dag->begin(); node_it != dag->end(); node_it++) {
            bool_node* node = *node_it;
            if (boolNodes.find(node->id) != boolNodes.end()) {
                if (node->type == bool_node::ASSERT) {
                    boolConstraints.push_back(node->id);
                } else if (node->getOtype() == OutType::BOOL) {
                    auto it = nodeValsMap.find(node->id);
                    if (it != nodeValsMap.end()) {
                        boolConstraints.push_back(node->id);
                    }
                }
            }
            if (Util::isSqrt(node)) {
                sqrtConstraints.push_back(node->id);
            }
        }
        cout << "Sqrt constraints: " << sqrtConstraints.size() << endl;
        Assert(neF - 1 >= sqrtConstraints.size(), "Number of constraints is less than what is required for sqrt constraints");
        int numToRemove = 0;
        if (!PARAMS->useSnoptUnconstrained && boolConstraints.size() + sqrtConstraints.size() > neF - 1) {
            numToRemove = boolConstraints.size() + sqrtConstraints.size() - neF + 1;
        }
        cout << "Removing " << numToRemove << " constraints" << endl;
        while (numToRemove > 0) {
            int oldsize = boolConstraints.size();
            int randIdx = rand() % (boolConstraints.size());
            boolConstraints.erase(boolConstraints.begin() + randIdx);
            if (boolConstraints.size() != oldsize - 1) {
                cout << "Did not delete " << randIdx << " size " << oldsize << endl;
                Assert(false, "Dfqwerq");
            }
            numToRemove--;
        }
        cout << boolConstraints.size() << endl;
        //cout << "Selected constraints: ";
        //for (int i = 0; i < boolConstraints.size(); i++) {
        //    cout << boolConstraints[i] << ";";
        //}
        cout << endl;
        
        set<int> boolConstraintsSet(boolConstraints.begin(), boolConstraints.end());*/
        
        // start the snopt solving
		SnoptParameters* p = new SnoptParameters(eval, dag, allInputs, imap, boolNodes);
		snoptSolver->init((char *) p, neF, SnoptEvaluator::df, 0, 0.0, xlow, xupp, Flow, Fupp);
		
		double betas[4] = {-1, -10, -100, -1000};
		double alphas[4] = {1, 10, 100, 1000};
		
		gsl_vector_memcpy(t, initState);
		
		double obj;
		int numtries = 0;
		minObjectiveVal = 1e50;
		bool solved;
		while (minObjectiveVal > threshold && numtries < MAX_TRIES) {
            if (!suppressPrint){
                cout << "Attempt: " << (numtries + 1) << endl;
            }
			
			for (int i = 0; i < 4; i++) {
                if (!suppressPrint) {
                    cout << "Beta: " << betas[i] << " Alpha: " << alphas[i] << endl;
                }
				p->beta = betas[i];
				p->alpha = alphas[i];
                if (!suppressPrint) {
                    for (int i = 0; i < n; i++) {
                        cout << gsl_vector_get(t, i) << ", ";
                    }
                    cout << endl;
                }
                SnoptEvaluator::counter = 0;
				solved = snoptSolver->optimize(t, suppressPrint);
				obj = snoptSolver->getObjectiveVal();
				gsl_vector_memcpy(t, snoptSolver->getResults());
			}
			
			if (solved && obj < minObjectiveVal) {
				gsl_vector_memcpy(minState, snoptSolver->getResults());
				minObjectiveVal = obj;
			}
			numtries++;
			randomizeCtrls(t);
		}
		//eval->print();
		//eval->printFull();
		return minObjectiveVal < threshold;
	}
	
	virtual gsl_vector* getMinState() {
		return minState;
	}
	
	virtual double getObjectiveVal() {
		return minObjectiveVal;
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
		
        for (; counter < n; counter++) {
            double low = 0.0;
            double high = 1.0;
            double r = low + (rand() % (int)((high - low) * 10.0))/10.0;
            gsl_vector_set(state, counter, r);
        }
	}

};
