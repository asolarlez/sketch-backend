#pragma once
#include <iostream>
#include <vector>
#include <map>

#include "MaxSolver.h"

#include "MaxOptimizationWrapper.h"
#include "SmoothEvaluators.h"
#include "Util.h"
#include "Interface.h"
#include "GradUtil.h"
#include "CommandLineArgs.h"



using namespace std;


class MaxParameters {
public:
    SmoothEvaluators* eval;
    Interface* interf;
    const set<int>& assertConstraints;
    int minimizeNode;
    double beta;
    double alpha;
    int level;
    
    MaxParameters(SmoothEvaluators* eval_, Interface* interf_, const set<int>& assertConstraints_, int minimizeNode_, int level_): eval(eval_), interf(interf_), assertConstraints(assertConstraints_), minimizeNode(minimizeNode_), level(level_) {}
};

class MaxEvaluator {
    static gsl_vector* state;
    static gsl_vector* grad;
public:
    static ofstream file;
    static void init(int size) {
        state = gsl_vector_alloc(size);
        grad = gsl_vector_alloc(size);
    }
    
    static int df(integer *n,    doublereal x[],
                  integer *neF,  doublereal F[],
                  integer *neG,  doublereal G[],
                  char      *cu, integer* nasserts) {
        MaxParameters* p = (MaxParameters*) cu;
        GradUtil::BETA = p->beta;
        GradUtil::ALPHA = p->alpha;
        //cout << counter++ << endl;
        //cout << "x: ";
        for (int i =0 ; i < *n; i++) {
            gsl_vector_set(state, i, x[i]);
            // cout << x[i] << " ";
        }
        if (PARAMS->numdebug)  { 
            cout << Util::print(state) << endl;
            file << Util::print(state) << endl;
        }
        //cout << endl;
        p->eval->run(state);

        int fcounter = 0;
        int gcounter = 0;
        F[0] = 0;
        for (int j = 0; j < *n; j++) {
            G[j] = 0;
        }
        if (p->level < 0) {
            if (p->minimizeNode >= 0) {
                double dist = p->eval->getErrorOnConstraint(p->minimizeNode, grad);
                F[0] += dist;
                for (int j = 0; j < *n; j++) {
                    G[j] += gsl_vector_get(grad, j);
                }
            }
            for (auto it = p->assertConstraints.begin(); it != p->  assertConstraints.end(); it++) {
                double dist = p->eval->getErrorOnConstraint(*it, grad);
                if (dist < 0.0) {
                    F[0] += -dist;
                    for (int j = 0; j < *n; j++) { 
                        G[j] += -gsl_vector_get(grad, j);
                    }
                }
            }
        } else {
            for (auto it = p->interf->clauseLevels[p->level].begin(); it != p->interf->clauseLevels[p->level].end(); it++) {
                double dist = p->eval->getErrorOnClause(*it, grad);
                if (dist < 0.0) {
                    F[0] += -dist;
                    for (int j = 0; j < *n; j++) { 
                        G[j] += -gsl_vector_get(grad, j);
                    }
                }
            }
        }
        fcounter = 1;
        gcounter = *n;
       
        

        /*for (int i = 0; i < fcounter; i++) {
            cout << F[i] << " ";
        }
        cout << endl;

        for (int i = 0; i < fcounter; i++) {
            cout << G[i] << " ";
        }
        cout << endl;*/

        //cout << "Fcounter: " << fcounter << " " << *neF << endl;
        for (int i = fcounter; i < *neF; i++) {
            F[i] = 1000.0;
            for (int j = 0; j < *n; j++) {
                G[gcounter++] = 0.0;
            }
        }
        Assert(fcounter <= *neF, "dfqeurq");
        return 0;
    }
    
};



class MaxSolverWrapper : public MaxOptimizationWrapper {
	MaxSolver* maxSolver;
    integer n; integer neF; 
    
    SmoothEvaluators* eval;
    gsl_vector* minState;
    gsl_vector* t;
    gsl_vector* t1;
    
    int RANDOM_SEARCH = 10;
        
    doublereal* x;
    doublereal* xlow;
    doublereal* xupp;
    
    doublereal* Flow;
    doublereal* Fupp;
    
    double minObjectiveVal;
    double threshold = 0.01;
    
    void getFranges() {
        Flow[0] = -1e20;
        Fupp[0] = 1e20;
        for (int i = 1; i < neF; i++) {
            Flow[i] = 0.01;
            Fupp[i] = 1e20;
        }
    }

    set<int> emptyConstraints;

    string dir;
    
public:
    MaxSolverWrapper(SmoothEvaluators* eval_, int ncontrols_,  doublereal* xlow_, doublereal* xupp_, int numConstraints_): eval(eval_), n(ncontrols_), xlow(xlow_), xupp(xupp_) {
        MaxEvaluator::init(n);
        neF = numConstraints_ + 1;
        
        cout << "nef: " << neF << endl;
        cout << "n: " << n << endl;

		maxSolver = new MaxSolver(n, neF);
        
        minState = gsl_vector_alloc(n);
        t = gsl_vector_alloc(n);
        t1 = gsl_vector_alloc(n);
        
        x = new doublereal[n];
        
        Flow = new doublereal[neF];
        Fupp = new doublereal[neF];
        getFranges();
        if (n == 1) {
            dir = "1Dfull/";
        } else {
            dir = "2Dfull/";
        }
    }
   	virtual bool maximize(Interface* inputs, const gsl_vector* initState, const gsl_vector* initDir, const set<int>& assertConstraints, int minimizeNode, float beta, int level, int idx) { 
        MaxParameters* p = new MaxParameters(eval, inputs, assertConstraints, minimizeNode, level);
        maxSolver->init((char *) p, neF, MaxEvaluator::df, xlow, xupp, Flow, Fupp);
        gsl_vector_memcpy(t, initState);

        p->beta = beta;
        p->alpha = -beta;
        if (PARAMS->numdebug) { 
            MaxEvaluator::file.open("/afs/csail.mit.edu/u/j/jinala/symdiff/scripts/smoothing/" + dir + Util::benchName() + "_" + to_string(GradUtil::counter) + "_" + to_string(idx) + "_custommax_"  + to_string(int(-beta)) + "_" + to_string(PARAMS->smoothingMode) + ".txt");
        }

        bool solved = maxSolver->optimize(t, initDir);
        double obj = maxSolver->getObjectiveVal();
        gsl_vector_memcpy(minState, maxSolver->getResults());
        cout << "Max objective found: " << obj << endl;
        if (PARAMS->numdebug) {
            MaxEvaluator::file << Util::print(minState) << endl;
            MaxEvaluator::file.close();
        }
        return solved;
        
    }
    
    virtual gsl_vector* getMinState() {
        return minState;
    }
    
    virtual double getObjectiveVal() {
        return minObjectiveVal;
    }
};


