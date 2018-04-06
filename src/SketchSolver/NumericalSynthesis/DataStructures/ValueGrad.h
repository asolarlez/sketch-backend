#pragma once
#include "GradUtil.h"
#include "DistanceGrad.h"

#ifndef _NOGSL
#include <gsl/gsl_vector.h>
#include <gsl/gsl_blas.h>
#else
#include "CustomSolver.h"
#endif

#include <limits>
#include <math.h>
#include <vector>
#include "BasicError.h"

using namespace std;

class ValueGrad {
	double val;
	gsl_vector* grad;
public:
	bool set;
	ValueGrad(double _val, gsl_vector* _grad): val(_val), grad(_grad), set(false) {}
	~ValueGrad(void) {
		delete grad;
	}
	double getVal() const { return val; }
	gsl_vector* getGrad() const { return grad; }
	void update(double _val) {
		val = _val;
	}
	static void vg_plus(ValueGrad* m, ValueGrad* f, ValueGrad* o); // o = m + f
	static void vg_times(ValueGrad* m, ValueGrad* f, ValueGrad* o); // o = m * f
	static void vg_div(ValueGrad* m, ValueGrad* f, ValueGrad* o); // o = m / f
	static void vg_neg(ValueGrad* m, ValueGrad* o); // o = -m
	static void vg_equal(ValueGrad* m, ValueGrad* f, DistanceGrad* o); // o = m == f
	static void vg_lt(ValueGrad* m, ValueGrad* f, DistanceGrad* o); // o = m < f
	static void vg_lt(ValueGrad* m, ValueGrad* f, ValueGrad* o);
	static void vg_square(ValueGrad* m, ValueGrad* o); // o = m * m
	static void vg_arctan(ValueGrad* m, ValueGrad* o); // o = arctan(m)
	static void vg_sin(ValueGrad* m, ValueGrad* o); // o = sin(m)
	static void vg_cos(ValueGrad* m, ValueGrad* o); // o = cos(m)
	static void vg_tan(ValueGrad* m, ValueGrad* o); // o = tan(m)
	static void vg_sqrt(ValueGrad* m, ValueGrad* o); // o = sqrt(m)
	static void vg_exp(ValueGrad* m, ValueGrad* o);
	static void vg_copy(ValueGrad* i1, ValueGrad* i2); // copy i1 into i2
	static void vg_cast_int_float(ValueGrad* m, ValueGrad* o);
	
	static void vg_ite(ValueGrad* m, ValueGrad* f, DistanceGrad* d, ValueGrad* o); // o = ite(d, m, f)
	static void vg_ite(ValueGrad* m, ValueGrad* f, ValueGrad* d, ValueGrad* o); // o = ite(d, m, f)
	static void vg_ite(DistanceGrad* m, DistanceGrad* f, ValueGrad* d, DistanceGrad* o);
    static void vg_ite(ValueGrad* m, ValueGrad* f, double dval, gsl_vector* dgrad, ValueGrad* o);
	
	static void vg_or(ValueGrad* m, ValueGrad* f, ValueGrad* o);
	static void vg_and(ValueGrad* m, ValueGrad* f, ValueGrad* o);
	static void vg_not(ValueGrad* m, ValueGrad* o);
	
	string print() {
		stringstream str;
		str << "Val: " << val;
		return str.str();
	}
	
	string printFull() {
		stringstream str;
		str << "Val: " << val << endl;
		str << "Grads: ";
		for (int i = 0; i < grad->size; i++) {
		 str << gsl_vector_get(grad, i) << ", ";
		}
		str << endl;
        if (val > 1e5 || gsl_blas_dnrm2(grad) > 1e5) {
            str << "LARGE VALUES" << endl;
        }
		return str.str();
	}
	
	void bound() {
		double oldVal = val;
		val = GradUtil::bound(val);
		if (oldVal != val) {
			GradUtil::default_grad(grad);
		}
		for (int i = 0; i < grad->size; i++) {
			gsl_vector_set(grad, i, GradUtil::bound(gsl_vector_get(grad, i)));
		}
	}

};
