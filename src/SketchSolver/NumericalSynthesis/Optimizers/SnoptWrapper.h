#pragma once
#include <iostream>
#include <vector>
#include <map>


#include "OptSolver.h"
#ifndef _NOSNOPT
#include "Snopt.h"
#include "CustomSolver.h"
#else
#include "CustomSolver.h"
#endif

#include "OptimizationWrapper.h"
#include "SmoothEvaluators.h"
#include "Util.h"
#include "Interface.h"
#include "GradUtil.h"
#include "CommandLineArgs.h"
#include "MaxOptimizationWrapper.h"



using namespace std;


class SnoptParameters {
public:
    SmoothEvaluators* eval;
    const set<int>& assertConstraints;
    int minimizeNode;
    Interface* interf;
    double beta;
    double alpha;
    int level;
    
    SnoptParameters(SmoothEvaluators* eval_, const set<int>& assertConstraints_, int minimizeNode_, Interface* interf_, int level_): eval(eval_), assertConstraints(assertConstraints_), minimizeNode(minimizeNode_), interf(interf_), level(level_) {}
};

class SnoptEvaluator {
    static gsl_vector* state;
    static gsl_vector* grad;
public:
    static int counter;
    static double prevVal;
    static int prevCount;
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
        //cout << *Status << " " << *needF << " " << *needG << endl;
        SnoptParameters* p = (SnoptParameters*) cu;
        GradUtil::BETA = p->beta;
        GradUtil::ALPHA = p->alpha;
        //cout << counter++ << endl;
        //cout << "x: ";
        //cout << "I:" << Util::print(state) << ":";
        for (int i =0 ; i < *n; i++) {
            gsl_vector_set(state, i, x[i]);
            // cout << x[i] << " ";
        }
        if (PARAMS->numdebug)  { 
            cout << counter++ << " " << Util::print(state);
            file << Util::print(state) << endl;
        }
        //cout << endl;
        p->eval->run(state);

        double total_dist = 0;
        int fcounter = 0;
        int gcounter = 0;
        if (p->minimizeNode < 0 || p->level >= 0) {
            F[fcounter++] = 0; // objective
            for (int j = 0; j < *n; j++) { // objective gradients
                G[gcounter++] = 0;
            }
        } else {
            double dist = p->eval->getErrorOnConstraint(p->minimizeNode, grad);
            F[fcounter++] = dist;
            total_dist += dist;
            for (int j = 0; j < *n; j++) {
                G[gcounter++] = gsl_vector_get(grad, j);
            }
        }
        if (p->level == -1) {
			
            double dist = p->eval->getErrorForAsserts(p->assertConstraints, grad);
            if (dist < 0.0) {
                total_dist += -dist;
            }
            F[fcounter++] = dist;
            for (int j = 0; j < *n; j++) {
                G[gcounter++] = gsl_vector_get(grad, j);
            }
			
            /*for (auto it = p->assertConstraints.begin(); it != p->  assertConstraints.end(); it++) {
                double dist = p->eval->getErrorOnConstraint(*it, grad);
                if (dist < 0.0) {
                    total_dist += -dist;
                }

                if (dist < 0.1) {
                    string msg = p->eval->getAssertMsg(*it);
                    cout << "(" << msg << "," << -dist << "),"; 
                }
                F[fcounter++] = dist;
                for (int j = 0; j < *n; j++) {
                    G[gcounter++] = gsl_vector_get(grad, j);
                }
            }*/
            /*const set<int>& inputConstraints = p->interf->getInputConstraints();
            for (auto it = inputConstraints.begin(); it != inputConstraints.end(); it++) {
                double dist = p->eval->getErrorOnConstraint(*it, grad);
                if (dist < 0.0) {
                    total_dist += -dist;
                }
                F[fcounter++] = dist;
                for (int j = 0; j < *n; j++) {
                    G[gcounter++] = gsl_vector_get(grad, j);
                }
            }*/
        } 

        for (int i = p->level; i < p->interf->numLevels(); i++) {
            if (i < 0) continue;
            for (auto it = p->interf->clauseLevels[i].begin(); it != p->interf->clauseLevels[i].end(); it++) {
                double dist = p->eval->getErrorOnClause(*it, grad);
                double scale = 1.0;
                if (i > p->level) scale = 100.0;
                if (dist < 0.0) {
                    total_dist += -dist*scale;
                }
                F[fcounter++] = dist*scale;
                gsl_vector_scale(grad, scale);
                for (int j = 0; j < *n; j++) {
                    G[gcounter++] = gsl_vector_get(grad, j);
                }
            }
        }

        //cout << "Fcounter: " << fcounter << " " << *neF << endl;
        for (int i = fcounter; i < *neF; i++) {
            F[i] = 1000.0;
            for (int j = 0; j < *n; j++) {
                G[gcounter++] = gsl_vector_get(grad, j);
            }
        }
        //cout << ":" << total_dist << " beta " << p->alpha << endl;
        if (PARAMS->numdebug)  { 
            cout << " " << total_dist << endl;
        }
        Assert(fcounter <= *neF, "dfqeurq");

        if (abs(total_dist - prevVal) < 1e-3) {
            prevCount++;
            if (prevCount >= 3) {
                //*Status = -2;
            }
        } else {
            prevVal = total_dist;
            prevCount = 0;
        }
        return 0;
    }
    
};


class MaxSnoptParameters {
public:
    SmoothEvaluators* eval;
    Interface* interf;
    const set<int>& assertConstraints;
    int minimizeNode;
    double beta;
    double alpha;
    int level;
    
    MaxSnoptParameters(SmoothEvaluators* eval_, Interface* interf_, const set<int>& assertConstraints_, int minimizeNode_, int level_): eval(eval_), interf(interf_), assertConstraints(assertConstraints_), minimizeNode(minimizeNode_), level(level_) {}
};

class MaxSnoptEvaluator {
    static gsl_vector* state;
    static gsl_vector* grad;
public:
    static double prevVal;
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
        cout << Util::print(state) << endl;
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
        cout << "Error: " << F[0] << endl;
        F[0] = -F[0];
        for (int j = 0; j < *n; j++) {
            G[j] = -G[j];
        }
        fcounter = 1;
        gcounter = *n;
        
        for (int i = p->level+1; i < p->interf->numLevels(); i++) {
            if (i < 0) continue;
            for (auto it = p->interf->clauseLevels[i].begin(); it != p->interf->clauseLevels[i].end(); it++) {
                double dist = p->eval->getErrorOnClause(*it, grad);
                F[fcounter++] = dist;
                for (int j = 0; j < *n; j++) {
                    G[gcounter++] = gsl_vector_get(grad, j);
                }
            }
        }

        if (F[0] >  0.1 + prevVal) {
            *Status = -2;
        } else {
            prevVal = F[0];
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



class SnoptWrapper: public OptimizationWrapper, public MaxOptimizationWrapper {
    OptSolver* snoptSolver;
    integer n; integer neF; integer lenA;
    
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
            Flow[i] = 0.00;
            Fupp[i] = 1e20;
        }
    }

    set<int> emptyConstraints;
    string suffix;
    string dir;

    timerclass timer;
    
public:
    SnoptWrapper(SmoothEvaluators* eval_, int ncontrols_,  doublereal* xlow_, doublereal* xupp_, int numConstraints_, bool useSnopt = true): eval(eval_), n(ncontrols_), xlow(xlow_), xupp(xupp_) {
        SnoptEvaluator::init(n);
        neF = (numConstraints_ + 1)*4;
        MaxSnoptEvaluator::init(n);
        lenA = 10; // we don't use this currently
        
        cout << "nef: " << neF << endl;
        cout << "n: " << n << endl;

#ifndef _NOSNOPT
        if (useSnopt) {
		  snoptSolver = new SnoptSolver(n, neF, lenA);
          suffix = "opt";
        } else {
          snoptSolver = new NotSnoptSolver(n, neF, lenA);
          suffix = "customopt";
        }
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

        if (n == 1) {
            dir = "1Dfull/";
        } else {
            dir = "2Dfull/";
        }
    }
    virtual bool maximize(Interface* inputs, const gsl_vector* initState, const gsl_vector* initDir, const set<int>& assertConstraints, int minimizeNode, float beta, int level, int idx) { 
        MaxSnoptParameters* p = new MaxSnoptParameters(eval, inputs, assertConstraints, minimizeNode, level);
        snoptSolver->init((char *) p, neF, MaxSnoptEvaluator::df, 0, 0.0, xlow, xupp, Flow, Fupp);

        gsl_vector_memcpy(t, initState);

        p->beta = beta;
        p->alpha = -beta;
        if (PARAMS->numdebug) { 
            MaxSnoptEvaluator::file.open("/afs/csail.mit.edu/u/j/jinala/symdiff/scripts/smoothing/" + dir + Util::benchName() + "_" + to_string(GradUtil::counter) + "_" + to_string(idx) + "_snoptmax_"  + to_string(int(-beta)) + "_" + to_string(PARAMS->smoothingMode) + ".txt");

            // print all the clauses
            for (int i = 0; i < inputs->numLevels(); i++) {
                for (auto it = inputs->clauseLevels[i].begin(); it != inputs->clauseLevels[i].end(); it++) {
                    MaxSnoptEvaluator::file << (*it)->creationTime << " ";
                }
            }
            MaxSnoptEvaluator::file << endl;
        }

        MaxSnoptEvaluator::prevVal = 0;
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
    virtual bool optimize(Interface* inputs, gsl_vector* initState, const set<int>& constraints, int minimizeNode, bool suppressPrint, int MAX_TRIES, LocalState* localState, int level = -1) {
        Assert(neF > constraints.size() + inputs->numSet() + inputs->totalClauses(), "Increase neF");
        // start the snopt solving
        SnoptParameters* p = new SnoptParameters(eval, constraints, minimizeNode, inputs, level);
        
        snoptSolver->init((char *) p, neF, SnoptEvaluator::df, 0, 0.0, xlow, xupp, Flow, Fupp);
        
        double betas[3] = {-1, -10, -50};
        double alphas[3] = {1, 10, 50};
        
        if (initState != NULL) {
            gsl_vector_memcpy(t, initState);
        } else {
            randomizeCtrls(t, inputs, constraints, minimizeNode);
        }
        GradUtil::BETA = betas[0];
        GradUtil::ALPHA = alphas[0];
        eval->run(t);
        cout << "I:" << Util::print(t) << "::" << getError(constraints, minimizeNode) << " beta " << alphas[0]  << endl;
        
        double obj;
        int numtries = 0;
        minObjectiveVal = 1e50;
        bool solved=false;
        while (minObjectiveVal > threshold && numtries < MAX_TRIES && !solved) {
            if (!suppressPrint){
                cout << "Attempt: " << (numtries + 1) << endl;
            }
            
            for (int i = 0; i < 3; i++) {
                if (!suppressPrint) {
                    cout << "Beta: " << betas[i] << " Alpha: " << alphas[i] << endl;
                }
                if (PARAMS->numdebug  && level == -1) { 
                    SnoptEvaluator::file.open("/afs/csail.mit.edu/u/j/jinala/symdiff/scripts/smoothing/" + dir + Util::benchName() + "_" + to_string(GradUtil::counter) + "_" + suffix + "_"  + to_string(int(alphas[i])) + "_" + to_string(PARAMS->smoothingMode) + ".txt");

                    // print all the clauses
                    for (int i = 0; i < inputs->numLevels(); i++) {
                        for (auto it = inputs->clauseLevels[i].begin(); it != inputs->clauseLevels[i].end(); it++) {
                            SnoptEvaluator::file << (*it)->creationTime << " ";
                        }
                    }
                    SnoptEvaluator::file << endl;
                    timer.restart();
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
                SnoptEvaluator::prevVal = 0;
                SnoptEvaluator::prevCount = 0;
                if (localState != NULL) {
                    gsl_vector_memcpy(localState->startStates[i], t); 
                }
                solved = snoptSolver->optimize(t, suppressPrint);
                obj = snoptSolver->getObjectiveVal();
                gsl_vector_memcpy(t, snoptSolver->getResults());
                eval->run(t);
                cout << "I:" << Util::print(t) << "::" << getError(constraints, minimizeNode) << " beta " << p->alpha  << endl;
                if (PARAMS->numdebug) {
                    SnoptEvaluator::file << Util::print(t) << endl;
                    SnoptEvaluator::file << timer.stop().get_cur_ms() << endl;
                    SnoptEvaluator::file.close();
                }
                if (localState != NULL) {
                    gsl_vector_memcpy(localState->localSols[i], t); 
                    //eval->run(t);
                    localState->errors[i] = getError(constraints, minimizeNode);
                    // TODO: this does not work with multiple retries and we want best local state so far. 
                }
                cout << "Error: " << getError(constraints, minimizeNode) << endl;

            }
            if (numtries == 0) {
                gsl_vector_memcpy(minState, snoptSolver->getResults());
            }
            if (solved && obj < minObjectiveVal) {
                gsl_vector_memcpy(minState, snoptSolver->getResults());
                minObjectiveVal = obj;
            }
            numtries++;
            if (minObjectiveVal > threshold && numtries < MAX_TRIES && !solved) {
                randomizeCtrls(t, inputs, constraints, minimizeNode);
            }
        }
        return solved || (minObjectiveVal < threshold);
    }
    
    virtual gsl_vector* getMinState() {
        return minState;
    }
    
    virtual double getObjectiveVal() {
        return minObjectiveVal;
    }

    double getError(const set<int>& constraints, int minimizeNode) {
        double error = 0.0;
        double e;
        if (minimizeNode >= 0) {
            e = eval->getErrorOnConstraint(minimizeNode);
            if (e > 0.0) {
                error += e; // TODO : change this
            }
        }
        for (auto it = constraints.begin(); it != constraints.end(); it++) {
            e = eval->getErrorOnConstraint(*it);
            if (e < 0.0) {
                error += -e;
            }
        }
        return error;
    }
    
    virtual void randomizeCtrls(gsl_vector* state, Interface* inputs, const set<int>& constraints, int minimizeNode) {
        double best = GradUtil::MAXVAL;
        bool foundValid = false;
        for (int i = 0; i < RANDOM_SEARCH; i++) {
            randomize(t1);
            cout << "Trying: ";
            for (int j = 0; j < t1->size; j++) {
                cout << gsl_vector_get(t1, j) << ", ";
            }
            GradUtil::BETA = -50;
            GradUtil::ALPHA = 50;
            eval->run(t1);
            double error = getError(constraints, minimizeNode);
            cout << "Error: " << error << " ";
            bool valid = true;
            double e;
            for (int i = 0; i < inputs->numLevels(); i++) {
                for (auto it = inputs->clauseLevels[i].begin(); it != inputs->clauseLevels[i].end(); it++) {
                    e = eval->getErrorOnClause(*it);
                    if (e < 0.0) {
                        valid = false;
                        goto done;
                    }
                }
            }
            done:
            if (valid) {
                cout << "valid" << endl;
            } else {
                cout << "invalid" << endl;
            }
            if (foundValid) {
                if (valid && error < best) {
                    best = error;
                    gsl_vector_memcpy(state, t1);
                }
            } else {
                if (valid) {
                    foundValid = true;
                    best = error;
                    gsl_vector_memcpy(state, t1);
                } else if (error < best) {
                    best = error;
                    gsl_vector_memcpy(state, t1);
                }
            }
        }
    }
    
    void randomize(gsl_vector* state) {
        for (int i = 0; i < state->size; i++) {
            double low = xlow[i];
            double high = xupp[i];
            double r = low + (rand() % (int)((high - low) * 10.0))/10.0;
            gsl_vector_set(state, i, r);
        }
    }
    
};


