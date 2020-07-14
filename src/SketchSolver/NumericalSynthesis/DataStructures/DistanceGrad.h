#pragma once
#include "GradUtil.h"

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
#include "Util.h"

using namespace std;

class ValueGrad;
class DistanceGrad {
public:
	double dist;
	gsl_vector* grad;
	bool set; // TODO: this bit is not necessary if the default dist is 0
	DistanceGrad(double d, gsl_vector* g): dist(d), grad(g), set(true) {}
	~DistanceGrad(void) {
		gsl_vector_free(grad);
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
            if (dist > 1e5 || gsl_blas_dnrm2(grad) > 1e5) {
                str << "LARGE VALUES" << endl;
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

	static double dg_copy(DistanceGrad* m, gsl_vector* grad);
	static void dg_copy(DistanceGrad* m, DistanceGrad* o);

	static double dg_times(DistanceGrad* m, DistanceGrad* f, gsl_vector* grad);
    
    static void dg_ite(DistanceGrad* m, DistanceGrad* f, double dval, gsl_vector* dgrad, DistanceGrad* o);

    static bool same(DistanceGrad* m, DistanceGrad* f);

    static double dg_combine(DistanceGrad* m, DistanceGrad* f);
    static void dg_combine(DistanceGrad* m, DistanceGrad* f, DistanceGrad* o);

    static double dg_combine(DistanceGrad* m, DistanceGrad* f, double cval, gsl_vector* grad, int bv, gsl_vector* o);
    static double dg_combine(DistanceGrad* m, DistanceGrad* f, double cval, int bv);

    static void dg_combine(vector<DistanceGrad*>& cdists, DistanceGrad* vdist, vector<ValueGrad*>& cvals, int bv, DistanceGrad* o);
};
