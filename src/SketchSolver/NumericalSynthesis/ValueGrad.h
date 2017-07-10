#pragma once
#include "GradUtil.h"
#include "DistanceGrad.h"
#include <gsl/gsl_vector.h>
#include <gsl/gsl_blas.h>
#include <limits>
#include <math.h>
#include <vector>
#include "BasicError.h"

using namespace std;

class ValueGrad {
	float val;
	gsl_vector* grad;
public:
	ValueGrad(float _val, gsl_vector* _grad): val(_val), grad(_grad) {}
	~ValueGrad(void) {
		delete grad;
	}
	float getVal() const { return val; }
	gsl_vector* getGrad() const { return grad; }
	void update(float _val) {
		val = _val;
	}
	static void vg_plus(ValueGrad* m, ValueGrad* f, ValueGrad* o); // o = m + f
	static void vg_times(ValueGrad* m, ValueGrad* f, ValueGrad* o); // o = m * f
	static void vg_div(ValueGrad* m, ValueGrad* f, ValueGrad* o); // o = m / f
	static void vg_neg(ValueGrad* m, ValueGrad* o); // o = -m
	static void vg_equal(ValueGrad* m, ValueGrad* f, DistanceGrad* o); // o = m == f
	static void vg_lt(ValueGrad* m, ValueGrad* f, DistanceGrad* o); // o = m < f
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
