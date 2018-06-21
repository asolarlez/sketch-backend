#pragma once
#include <iostream>
#include <vector>
#include <map>

#ifndef _NOSNOPT
#include "Snopt.h"
#else
#include "CustomSolver.h"
#endif

#include "OptimizationWrapper.h"
#include "SymbolicEvaluator.h"
#include "Util.h"
#include "Interface.h"
#include "GradUtil.h"
#include "CommandLineArgs.h"



using namespace std;


class SnoptParameters {
public:
    SymbolicEvaluator* eval;
    const set<int>& boolNodes;
    int minimizeNode;
    double beta;
    double alpha;
    
    SnoptParameters(SymbolicEvaluator* eval_, const set<int>& boolNodes_, int minimizeNode_): eval(eval_), boolNodes(boolNodes_), minimizeNode(minimizeNode_) {}
};

class SnoptEvaluator {
    static gsl_vector* state;
    static gsl_vector* grad;
public:
    static int counter;
    static ofstream file;
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
        if (p->minimizeNode < 0) {
            F[fcounter++] = 0; // objective
            for (int j = 0; j < *n; j++) { // objective gradients
                G[gcounter++] = 0;
            }
        } else {
            double dist = p->eval->getErrorOnConstraint(p->minimizeNode, grad);
            F[fcounter++] = dist;
            for (int j = 0; j < *n; j++) {
                G[gcounter++] = gsl_vector_get(grad, j);
            }
        }
        for (auto it = p->boolNodes.begin(); it != p->boolNodes.end(); it++) {
            double dist = p->eval->getErrorOnConstraint(*it, grad);
            F[fcounter++] = dist;
            for (int j = 0; j < *n; j++) {
                G[gcounter++] = gsl_vector_get(grad, j);
            }
        }

        //cout << "Fcounter: " << fcounter << " " << *neF << endl;
        for (int i = fcounter; i < *neF; i++) {
            F[i] = 1000.0;
            for (int j = 0; j < *n; j++) {
                G[gcounter++] = gsl_vector_get(grad, j);
            }
        }
        Assert(fcounter <= *neF, "dfqeurq");
        return 0;
    }
    
};


class MaxSnoptParameters {
public:
    SymbolicEvaluator* eval;
    const set<int>& inputConstraints;
    const set<int>& assertConstraints;
    int minimizeNode;
    int condNode;
    int condVal;
    double beta;
    double alpha;
    
    MaxSnoptParameters(SymbolicEvaluator* eval_, const set<int>& inputConstraints_, const set<int>& assertConstraints_, int minimizeNode_, int condNode_, int condVal_): eval(eval_), inputConstraints(inputConstraints_), assertConstraints(assertConstraints_), minimizeNode(minimizeNode_), condNode(condNode_), condVal(condVal_) {}
};

class MaxSnoptEvaluator {
    static gsl_vector* state;
    static gsl_vector* grad;
public:
    static ofstream file;
    static void init(int size) {
        state = gsl_vector_alloc(size);
        grad = gsl_vector_alloc(size);
    }
    
    static int df(integer    *Status, integer *n,    doublereal x[],
                  integer    *needF,  integer *neF,  doublereal F[],
                  integer    *needG,  integer *neG,  doublereal G[],
                  char      *cu,             integer *lencu,
                  integer    iu[],    integer *leniu,
                  doublereal ru[],    integer *lenru) {
        MaxSnoptParameters* p = (MaxSnoptParameters*) cu;
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
        F[fcounter] = 0;
        for (int j = 0; j < *n; j++) {
            G[gcounter++] = 0;
        }
        fcounter = 0;
        gcounter = 0;
        if (p->minimizeNode >= 0) {
            double dist = p->eval->getErrorOnConstraint(p->minimizeNode, grad);
            F[fcounter] += dist;
            for (int j = 0; j < *n; j++) {
                G[gcounter++] += gsl_vector_get(grad, j);
            }
        }
        for (auto it = p->assertConstraints.begin(); it != p->assertConstraints.end(); it++) {
            fcounter = 0;
            gcounter = 0;
            double dist = p->eval->getErrorOnConstraint(*it, grad);
            if (dist < 0.0) {
                F[fcounter] += -dist;
                for (int j = 0; j < *n; j++) { 
                    G[gcounter++] += -gsl_vector_get(grad, j);
                }
            }
        }
        fcounter = 0;
        gcounter = 0;
        F[fcounter] = -F[fcounter];
        for (int j = 0; j < *n; j++) {
            G[j] = -G[j];
            gcounter++;
        }
        fcounter++;
        for (auto it = p->inputConstraints.begin(); it != p->inputConstraints.end(); it++) {
            double dist = p->eval->getErrorOnConstraint(*it, grad);
            F[fcounter++] = dist;
            for (int j = 0; j < *n; j++) {
                G[gcounter++] = gsl_vector_get(grad, j);
            }
        }
        if (p->condNode >= 0) {
            double dist = p->eval->getErrorOnConstraint(p->condNode, p->condVal, grad);
            F[fcounter++] = dist;
            for (int j  = 0; j < *n; j++) {
                G[gcounter++] = gsl_vector_get(grad, j);
            }
        }

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
                G[gcounter++] = gsl_vector_get(grad, j);
            }
        }
        Assert(fcounter <= *neF, "dfqeurq");
        return 0;
    }
    
};



class SnoptWrapper: public OptimizationWrapper {
#ifndef _NOSNOPT
    SnoptSolver* snoptSolver;
#else
	NotSnoptSolver* snoptSolver;
#endif
    integer n; integer neF; integer lenA;
    
    SymbolicEvaluator* eval;
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
    
public:
    SnoptWrapper(SymbolicEvaluator* eval_, int ncontrols_,  doublereal* xlow_, doublereal* xupp_, int numConstraints_): eval(eval_), n(ncontrols_), xlow(xlow_), xupp(xupp_) {
        SnoptEvaluator::init(n);
        MaxSnoptEvaluator::init(n);
        neF = numConstraints_ + 1;
        lenA = 10; // we don't use this currently
        
        cout << "nef: " << neF << endl;
        cout << "n: " << n << endl;

#ifndef _NOSNOPT
		snoptSolver = new SnoptSolver(n, neF, lenA);
#else
		snoptSolver = new NotSnoptSolver(n, neF, lenA);
#endif

        
        minState = gsl_vector_alloc(n);
        t = gsl_vector_alloc(n);
        t1 = gsl_vector_alloc(n);
        
        x = new doublereal[n];
        
        Flow = new doublereal[neF];
        Fupp = new doublereal[neF];
        getFranges();
    }
    virtual bool maximize(Interface* inputs, const gsl_vector* initState, const set<int>& assertConstraints, int minimizeNode, float beta, int condNode, int condVal) { 
        eval->setInputs(inputs);
        MaxSnoptParameters* p = new MaxSnoptParameters(eval, inputs->getInputConstraints(), assertConstraints, minimizeNode, condNode, condVal);
        snoptSolver->init((char *) p, neF, MaxSnoptEvaluator::df, 0, 0.0, xlow, xupp, Flow, Fupp);

        gsl_vector_memcpy(t, initState);

        p->beta = beta;
        p->alpha = -beta;
        if (PARAMS->numdebug) { 
            MaxSnoptEvaluator::file.open("/afs/csail.mit.edu/u/j/jinala/symdiff/popl_scripts/data/max_" + to_string(GradUtil::counter - 1) + "_" + to_string(condNode) + "_" + to_string(int(-beta)) + ".txt");
        }

        bool solved = snoptSolver->optimize(t);
        double obj = snoptSolver->getObjectiveVal();
        gsl_vector_memcpy(minState, snoptSolver->getResults());
        cout << "Max objective found: " << obj << endl;
        if (PARAMS->numdebug) {
            MaxSnoptEvaluator::file << Util::print(minState) << endl;
            MaxSnoptEvaluator::file.close();
        }
        return solved;
        
    }
    virtual bool optimize(Interface* inputs, gsl_vector* initState, const set<int>& constraints, int minimizeNode, bool suppressPrint, int MAX_TRIES, LocalState* localState) {
        Assert(neF > constraints.size(), "Increase neF");
        eval->setInputs(inputs);
        // start the snopt solving
        SnoptParameters* p = new SnoptParameters(eval, constraints, minimizeNode);
        snoptSolver->init((char *) p, neF, SnoptEvaluator::df, 0, 0.0, xlow, xupp, Flow, Fupp);
        
        double betas[3] = {-1, -10, -50};
        double alphas[3] = {1, 10, 50};
        
        if (initState != NULL) {
            gsl_vector_memcpy(t, initState);
        } else {
            randomizeCtrls(t, inputs, constraints, minimizeNode);
        }
        
        double obj;
        int numtries = 0;
        minObjectiveVal = 1e50;
        bool solved;
        while (minObjectiveVal > threshold && numtries < MAX_TRIES) {
            if (!suppressPrint){
                cout << "Attempt: " << (numtries + 1) << endl;
            }
            
            for (int i = 0; i < 3; i++) {
                if (!suppressPrint) {
                    cout << "Beta: " << betas[i] << " Alpha: " << alphas[i] << endl;
                }
                if (PARAMS->numdebug) { 
                    SnoptEvaluator::file.open("/afs/csail.mit.edu/u/j/jinala/symdiff/popl_scripts/data/opt_" + to_string(GradUtil::counter) + "_" + to_string(int(alphas[i])) + ".txt");
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
                if (PARAMS->numdebug) {
                    SnoptEvaluator::file << Util::print(t) << endl;
                    SnoptEvaluator::file.close();
                }
                if (localState != NULL) {
                    gsl_vector_memcpy(localState->localSols[i], t); // TODO: this does not work with multiple retries and we want best local state so far. 
                }

            }
            if (numtries == 0) {
                gsl_vector_memcpy(minState, snoptSolver->getResults());
            }
            if (solved && obj < minObjectiveVal) {
                gsl_vector_memcpy(minState, snoptSolver->getResults());
                minObjectiveVal = obj;
            }
            numtries++;
            if (minObjectiveVal > threshold && numtries < MAX_TRIES) {
                randomizeCtrls(t, inputs, constraints, minimizeNode);
            }
        }
        return minObjectiveVal < threshold;
    }
    
    virtual gsl_vector* getMinState() {
        return minState;
    }
    
    virtual double getObjectiveVal() {
        return minObjectiveVal;
    }
    
    virtual void randomizeCtrls(gsl_vector* state, Interface* inputs, const set<int>& constraints, int minimizeNode) {
        double best = GradUtil::MAXVAL;
        eval->setInputs(inputs);
        for (int i = 0; i < RANDOM_SEARCH; i++) {
            randomize(t1);
            cout << "Trying: ";
            for (int j = 0; j < t1->size; j++) {
                cout << gsl_vector_get(t1, j) << ", ";
            }
            GradUtil::BETA = -1;
            GradUtil::ALPHA = 1;
            eval->run(t1);
            double error = 0.0;
            double e;
            if (minimizeNode >= 0) {
                error += eval->getErrorOnConstraint(minimizeNode);
            }
            for (auto it = constraints.begin(); it != constraints.end(); it++) {
                e = eval->getErrorOnConstraint(*it);
                if (e < 0.0) {
                    error += -e;
                }
            }
            cout << "Error: " << error << endl;
            if (error < best) {
                best = error;
                gsl_vector_memcpy(state, t1);
            }
        }
    }
    
    void randomize(gsl_vector* state) {
        for (int i = 0; i < state->size; i++) {
            double low = xlow[i];
            if (low < -100.0) low = -100.0;
            double high = xupp[i];
            if (high > 100.0) high = 100.0;
            double r = low + (rand() % (int)((high - low) * 10.0))/10.0;
            gsl_vector_set(state, i, r);
        }
    }
    
};


