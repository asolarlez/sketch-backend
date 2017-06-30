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
	static constexpr float MAXVAL = 1e10;
	static constexpr float MINVAL = -1e10;
	static constexpr float PI = 3.1415926535897;
	
	static float ALPHA;
	static float BETA;
	
	static gsl_vector* tmp;
	static gsl_vector* tmp1;
	static gsl_vector* tmp2;
	static gsl_vector* tmp3;
	static gsl_vector* tmpT; // used for internal computations in mult, div - don't use it anywhere else
	
	
	static float findMin(float val1, float val2, gsl_vector* grad1, gsl_vector* grad2, gsl_vector* l);
	static float findMin(const vector<float>& vals, const vector<gsl_vector*>& grads, gsl_vector* l);
	static float findMax(float val1, float val2, gsl_vector* grad1, gsl_vector* grad2, gsl_vector* h);
	static float findMax(const vector<float>& vals, const vector<gsl_vector*>& grads, gsl_vector* h);
	static float sigmoid(float d, gsl_vector* grads, gsl_vector* out);
	static void default_grad(gsl_vector* out);

	static float softMinMax(const vector<float>& vals, const vector<gsl_vector*>& grads, gsl_vector* l, float alpha, float t);
	
	static void compute_plus_grad(gsl_vector* mgrads, gsl_vector* fgrads, gsl_vector* out);
	static void compute_mult_grad(float mval, float fval, gsl_vector* mgrads, gsl_vector* fgrads, gsl_vector* out);
	static void compute_div_grad(float mval, float fval, gsl_vector* mgrads, gsl_vector* fgrads, gsl_vector* out);
	static void compute_neg_grad(gsl_vector* mgrads, gsl_vector* out);
	
	static void compute_square_grad(float mval, gsl_vector* mgrads, gsl_vector* out);
	static void compute_arctan_grad(float mval, gsl_vector* mgrads, gsl_vector* out);
	static void compute_sin_grad(float mval, gsl_vector* mgrads, gsl_vector* out);
	static void compute_cos_grad(float mval, gsl_vector* mgrads, gsl_vector* out);
	static void compute_tan_grad(float mval, gsl_vector* mgrads, gsl_vector* out);
	static void compute_sqrt_grad(float mval, gsl_vector* mgrads, gsl_vector* out);
	static void compute_exp_grad(float mval, gsl_vector* mgrads, gsl_vector* out);
	
	static bool inLimit(float v) {
		if (!isfinite(v)) return false;
		if (v > MAXVAL || v < MINVAL) return false;
		return true;
	}

	static float bound(float v) {
		if (!inLimit(v)) {
			//cout << "Bound reached" << endl;
 			return v > 0 ? MAXVAL : MINVAL;
		} else {
			return v;
		}
	}	
};
