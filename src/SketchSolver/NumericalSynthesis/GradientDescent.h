#pragma once
#include <gsl/gsl_vector.h>
#include <gsl/gsl_multimin.h>
#include <gsl/gsl_errno.h>
#include <gsl/gsl_blas.h>
#include <vector>

using namespace std;

class NumericalSolver;

class GradientDescent {
	int N; // Total number of components
	gsl_vector *x;  // The current state
	gsl_vector *ds; // The step size
	
	gsl_multimin_fdfminimizer* minidf;  // The GSL minimizer for when derivatives needed
	const gsl_multimin_fdfminimizer_type *Tdf ;
	gsl_multimin_function_fdf myfundf;
	double INIT_STEP_SIZE = 5.0;
	double TOLERANCE = 0.1;
	
public:
	GradientDescent(int N_p): N(N_p) {
		Tdf = gsl_multimin_fdfminimizer_steepest_descent;
		minidf = gsl_multimin_fdfminimizer_alloc(Tdf, N);
		myfundf.n = N;
	}
	
	gsl_vector* getResults() {
		return minidf->x;
	}
	void init(NumericalSolver* ns, const vector<vector<int>>& allInputs, const vector<vector<int>>& allOutputs, gsl_vector* prev);
	double optimize();
};


