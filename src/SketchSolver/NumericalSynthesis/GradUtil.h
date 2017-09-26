#pragma once

#include <gsl/gsl_vector.h>
#include <gsl/gsl_blas.h>
#include <limits>
#include <math.h>
#include <vector>
#include "BasicError.h"

using namespace std;

class GradUtil {
public:
	static constexpr double MAXVAL = 1e30;
	static constexpr double MINVAL = -1e30;
	static constexpr double PI = 3.1415926535897;
	
	static double ALPHA;
	static double BETA;
	
	static gsl_vector* tmp;
	static gsl_vector* tmp1;
	static gsl_vector* tmp2;
	static gsl_vector* tmp3;
	static gsl_vector* tmpT; // used for internal computations in mult, div - don't use it anywhere else
	
	
	static double findMin(double val1, double val2, gsl_vector* grad1, gsl_vector* grad2, gsl_vector* l);
	static double findMin(const vector<double>& vals, const vector<gsl_vector*>& grads, gsl_vector* l);
	static double findMax(double val1, double val2, gsl_vector* grad1, gsl_vector* grad2, gsl_vector* h);
	static double findMax(const vector<double>& vals, const vector<gsl_vector*>& grads, gsl_vector* h);
	static double sigmoid(double d, gsl_vector* grads, gsl_vector* out);
	static void default_grad(gsl_vector* out);

	static double softMinMax(const vector<double>& vals, const vector<gsl_vector*>& grads, gsl_vector* l, double alpha, double t);
	
	static void compute_plus_grad(gsl_vector* mgrads, gsl_vector* fgrads, gsl_vector* out);
	static void compute_mult_grad(double mval, double fval, gsl_vector* mgrads, gsl_vector* fgrads, gsl_vector* out);
	static void compute_div_grad(double mval, double fval, gsl_vector* mgrads, gsl_vector* fgrads, gsl_vector* out);
	static void compute_neg_grad(gsl_vector* mgrads, gsl_vector* out);
	
	static void compute_square_grad(double mval, gsl_vector* mgrads, gsl_vector* out);
	static void compute_arctan_grad(double mval, gsl_vector* mgrads, gsl_vector* out);
	static void compute_sin_grad(double mval, gsl_vector* mgrads, gsl_vector* out);
	static void compute_cos_grad(double mval, gsl_vector* mgrads, gsl_vector* out);
	static void compute_tan_grad(double mval, gsl_vector* mgrads, gsl_vector* out);
	static void compute_sqrt_grad(double mval, gsl_vector* mgrads, gsl_vector* out);
	static void compute_exp_grad(double mval, gsl_vector* mgrads, gsl_vector* out);
	
	static bool inLimit(double v) {
		if (!isfinite(v)) return false;
		if (v > MAXVAL || v < MINVAL) return false;
		return true;
	}

	static double bound(double v) {
		if (!inLimit(v)) {
			//cout << "Bound reached" << endl;
 			return v > 0 ? MAXVAL : MINVAL;
		} else {
			return v;
		}
	}	
};
