#include "Predicate.h"
#include "ActualEvaluators.h"
#include "SmoothEvaluators.h"


double BasicPredicate::evaluate(ActualEvaluators* eval) {
    return eval->computeError(dag, nid);
}

double BasicPredicate::evaluate(gsl_vector* grad, ActualEvaluators* eval) {
    return eval->computeErrorWithGrad(dag, nid, grad);
}

double BasicPredicate::evaluate(SmoothEvaluators* eval) {
    return eval->computeError(dag, nid);
}

double BasicPredicate::evaluate(gsl_vector* grad, SmoothEvaluators* eval) {
    return eval->computeErrorWithGrad(dag, nid, grad);
}

double BasicPredicate::grad_norm(ActualEvaluators* eval) {
    return eval->grad_norm(dag, nid);
}

const gsl_vector* BasicPredicate::grad(ActualEvaluators* eval) {
    return eval->grad(dag, nid);
}


double DiffPredicate::evaluate(ActualEvaluators* eval) {
	double d1 = p1->evaluate(eval);
	double d2 = p2->evaluate(eval);
	return d1 - d2;
}

double DiffPredicate::evaluate(gsl_vector* grad, ActualEvaluators* eval) {
    double d1 = p1->evaluate(tmp_p1, eval);
    double d2 = p2->evaluate(tmp_p2, eval);
    gsl_vector_memcpy(grad, tmp_p1);
    gsl_vector_sub(grad, tmp_p2);
    return d1 - d2;
}

double DiffPredicate::evaluate(SmoothEvaluators* eval) {
    double d1 = p1->evaluate(eval);
    double d2 = p2->evaluate(eval);
    return d1 - d2;
}

double DiffPredicate::evaluate(gsl_vector* grad, SmoothEvaluators* eval) {
    double d1 = p1->evaluate(tmp_p1, eval);
    double d2 = p2->evaluate(tmp_p2, eval);
    gsl_vector_memcpy(grad, tmp_p1);
    gsl_vector_sub(grad, tmp_p2);
    return d1 - d2;
}

double DiffPredicate::grad_norm(ActualEvaluators* eval) {
    double d1 = p1->evaluate(tmp_p1, eval);
    double d2 = p2->evaluate(tmp_p2, eval);
    gsl_vector_sub(tmp_p1, tmp_p2);
    return gsl_blas_dnrm2(tmp_p1);
}

const gsl_vector* DiffPredicate::grad(ActualEvaluators* eval) {
    gsl_vector_memcpy(tmp_p1, p1->grad(eval));
    gsl_vector_memcpy(tmp_p2, p2->grad(eval));
    gsl_vector_sub(tmp_p1, tmp_p2);
    return tmp_p1;
}
