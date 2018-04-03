#pragma once
#include <iostream>
#include <vector>
#include <map>
#include "Snopt.h"
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
    double beta;
    double alpha;
    
    SnoptParameters(SymbolicEvaluator* eval_, const set<int>& boolNodes_): eval(eval_), boolNodes(boolNodes_) {}
};

class SnoptEvaluator {
    static gsl_vector* state;
    static gsl_vector* grad;
public:
    static int counter;
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
        p->eval->run(state);
        //p->eval->print();
        for (auto it = p->boolNodes.begin(); it != p->boolNodes.end(); it++) {
            double dist = p->eval->getErrorOnConstraint(*it, grad); // TODO: deal with minimize
            F[fcounter++] = dist;
            for (int j = 0; j < *n; j++) {
                G[gcounter++] = gsl_vector_get(grad, j);
            }
        }
        
        
        //cout << "Fcounter: " << fcounter << " " << *neF << endl;
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
    
    SymbolicEvaluator* eval;
    gsl_vector* minState;
    gsl_vector* t;
    gsl_vector* t1;
    
    int RANDOM_SEARCH = 10;
    double RANDOM_TARGET = 100;
        
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
            Flow[i] = 0;
            Fupp[i] = 1e20;
        }
    }
    
public:
    SnoptWrapper(SymbolicEvaluator* eval_, int ncontrols_,  doublereal* xlow_, doublereal* xupp_, int numConstraints_): eval(eval_), n(ncontrols_), xlow(xlow_), xupp(xupp_) {
        SnoptEvaluator::init(n);
        neF = numConstraints_ + 1;
        lenA = 10; // we don't use this currently
        
        cout << "nef: " << neF << endl;
        cout << "n: " << n << endl;
        snoptSolver = new SnoptSolver(n, neF, lenA);
        minState = gsl_vector_alloc(n);
        t = gsl_vector_alloc(n);
        t1 = gsl_vector_alloc(n);
        
        x = new doublereal[n];
        
        Flow = new doublereal[neF];
        Fupp = new doublereal[neF];
        getFranges();
    }
    
    virtual bool optimize(Interface* inputs, gsl_vector* initState, const set<int>& constraints, bool suppressPrint = false, int MAX_TRIES = PARAMS->numTries) {
        Assert(neF > constraints.size(), "Increase neF");
        eval->setInputs(inputs);
        // start the snopt solving
        SnoptParameters* p = new SnoptParameters(eval, constraints);
        snoptSolver->init((char *) p, neF, SnoptEvaluator::df, 0, 0.0, xlow, xupp, Flow, Fupp);
        
        double betas[3] = {-1, -10, -50};
        double alphas[3] = {1, 10, 50};
        
        gsl_vector_memcpy(t, initState);
        
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
            if (numtries == 0) {
                gsl_vector_memcpy(minState, snoptSolver->getResults());
            }
            if (solved && obj < minObjectiveVal) {
                gsl_vector_memcpy(minState, snoptSolver->getResults());
                minObjectiveVal = obj;
            }
            numtries++;
            if (numtries < MAX_TRIES) {
                randomizeCtrls(t, inputs, constraints);
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
    
    virtual void randomizeCtrls(gsl_vector* state, Interface* inputs, const set<int>& constraints) {
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
            for (auto it = constraints.begin(); it != constraints.end(); it++) {
                error += eval->getErrorOnConstraint(*it);
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
