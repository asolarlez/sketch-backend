#pragma once
#include <gsl/gsl_vector.h>
#include <gsl/gsl_multimin.h>
#include <gsl/gsl_errno.h>
#include <gsl/gsl_blas.h>
#include <vector>

using namespace std;

typedef double (* GD_F_TYPE)(const gsl_vector*, void*);
typedef void (* GD_DF_TYPE)(const gsl_vector*, void*, gsl_vector*);
typedef void (* GD_FDF_TYPE)(const gsl_vector*, void*, double*, gsl_vector*);

class GradientDescent {
	int N; // Total number of components
	gsl_vector *x;  // The current state
	gsl_vector *ds; // The step size
	
	gsl_multimin_fdfminimizer* minidf;  // The GSL minimizer for when derivatives needed
	const gsl_multimin_fdfminimizer_type *Tdf ;
	gsl_multimin_function_fdf myfundf;

public:
	double INIT_STEP_SIZE = 5.0;
	double TOLERANCE = 0.1;
	double PRECISION = 1e-3;
	double GRAD_PRECISION = 1e-3;
	double ITERATIONS = 4000;
	
	GradientDescent(int N_p): N(N_p) {
		Tdf = gsl_multimin_fdfminimizer_vector_bfgs2;
		minidf = gsl_multimin_fdfminimizer_alloc(Tdf, N);
		myfundf.n = N;
		x = gsl_vector_alloc(N);
	}
	
	~GradientDescent() {
		gsl_multimin_fdfminimizer_free(minidf);
		gsl_vector_free(x);
	}
	
	gsl_vector* getResults() {
		return minidf->x;
	}
	void init(GD_F_TYPE f, GD_DF_TYPE df, GD_FDF_TYPE fdf, void* p);
	double optimize(gsl_vector* initX); // optimizes until value is 0
};


