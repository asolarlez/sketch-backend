#pragma once
#include "GradUtil.h"
#include <gsl/gsl_vector.h>
#include <gsl/gsl_blas.h>
#include <limits>
#include <math.h>
#include <vector>
#include "BasicError.h"

using namespace std;

class DistanceGrad {
public:
	float dist;
	gsl_vector* grad;
	bool set; // TODO: this bit is not necessary if the default dist is 0
	DistanceGrad(float d, gsl_vector* g): dist(d), grad(g), set(true) {}
	~DistanceGrad(void) {
		delete grad;
	}
	string print() {
		stringstream str;
		if (set) {
			str << "Dist: " << dist;
		} else {
			str << "Dist: NOT SET";
		}
		return str.str();
	}
	string printFull() {
		stringstream str;
		if (set) {
			str << "Dist: " << dist << endl;
			str << "DGrads: ";
			for (int i = 0; i < grad->size; i++) {
				str << gsl_vector_get(grad, i) << ", ";
			}
		} else {
			str << "Dist: NOT SET";
		}
		return str.str();
	}

	static void dg_and(DistanceGrad* m, DistanceGrad* f, DistanceGrad* d);
	static void dg_or(DistanceGrad* m, DistanceGrad* f, DistanceGrad* d);
	static void dg_not(DistanceGrad* m, DistanceGrad* d);
	static void dg_ite(DistanceGrad* b, DistanceGrad* m, DistanceGrad* f, DistanceGrad* d);
	static void dg_copy(DistanceGrad* m, DistanceGrad* o);
};
