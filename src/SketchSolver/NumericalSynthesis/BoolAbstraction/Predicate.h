#pragma once
#include "GradUtil.h"

#ifndef _NOGSL
#include <gsl/gsl_vector.h>
#include <gsl/gsl_blas.h>
#else
#include "CustomSolver.h"
#endif

#include "BooleanDAG.h"



class ActualEvaluators;
class SmoothEvaluators;

class Predicate {
public:
    virtual string print() = 0;
    virtual double evaluate(ActualEvaluators* eval) = 0;
    virtual double evaluate(gsl_vector* grad, ActualEvaluators* eval) = 0;
    virtual double evaluate(SmoothEvaluators* eval) = 0;
    virtual double evaluate(gsl_vector* grad, SmoothEvaluators* eval) = 0;
    virtual double grad_norm(ActualEvaluators* eval) = 0;
    virtual const gsl_vector* grad(ActualEvaluators* eval) = 0;
    virtual bool isPure() = 0;
    virtual void makeImpure() = 0;
    virtual bool isBasic() = 0;
    virtual bool isDiff() = 0;
    virtual ~Predicate() {};
};

class BasicPredicate: public Predicate { // TODO: override equal 
    bool pure;
public:
    int nid;
    BooleanDAG* dag;
    Predicate* oldPred;
    
    BasicPredicate(BooleanDAG* _dag, int _nid): dag(_dag), nid(_nid), oldPred(NULL) { pure = true;} 
    BasicPredicate(BooleanDAG* _dag, int _nid, Predicate* _oldPred): dag(_dag), nid(_nid), oldPred(_oldPred) { pure = true;} 
    virtual void makeImpure() {
        pure = false;
    }
    virtual bool isPure() {
        return pure;
    }
    virtual bool isBasic() { return true; }
    virtual bool isDiff() { return false; }
    virtual string print() {
        stringstream s;
        //s << (*dag)[nid]->lprint();
        s << nid;
        if (oldPred != NULL) {
            s << "(" << oldPred->print() << ")";
        }
        return s.str();
    }
    virtual double evaluate(ActualEvaluators* eval);
    virtual double evaluate(gsl_vector* grad, ActualEvaluators* eval);
    virtual double evaluate(SmoothEvaluators* eval);
    virtual double evaluate(gsl_vector* grad, SmoothEvaluators* eval);
    virtual double grad_norm(ActualEvaluators* eval);
    virtual const gsl_vector* grad(ActualEvaluators* eval);

};
    

class DiffPredicate: public Predicate {
    bool pure;
public:
    Predicate* p1;
    Predicate* p2;
    gsl_vector* tmp_p1; // TODO: Not sure where is the best place to create these tmp vectors for gradient calculation
    gsl_vector* tmp_p2;
    DiffPredicate(Predicate* _p1, Predicate* _p2, int ncontrols): p1(_p1), p2(_p2) {
        tmp_p1 = gsl_vector_alloc(ncontrols);
        tmp_p2 = gsl_vector_alloc(ncontrols);
        pure = true;
    }
    ~DiffPredicate() {
        gsl_vector_free(tmp_p1);
        gsl_vector_free(tmp_p2);
    }
    virtual void makeImpure() {
        pure = false;
    }
    virtual bool isPure() {
        return pure;
    }
    virtual bool isBasic() { return false; }
    virtual bool isDiff() { return true; }
    virtual string print() {
        stringstream s;
        s << p1->print() << " - " << p2->print() << endl;
        return s.str();
    }
    virtual double evaluate(ActualEvaluators* eval);
    virtual double evaluate(gsl_vector* grad, ActualEvaluators* eval);
    virtual double evaluate(SmoothEvaluators* eval);
    virtual double evaluate(gsl_vector* grad, SmoothEvaluators* eval);
    virtual double grad_norm(ActualEvaluators* eval);
    virtual const gsl_vector* grad(ActualEvaluators* eval);

};