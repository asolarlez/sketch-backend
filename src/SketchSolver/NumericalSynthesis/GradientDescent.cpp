#include "GradientDescent.h"
#include <iostream>

void GradientDescent::init(GD_F_TYPE f, GD_DF_TYPE df, GD_FDF_TYPE fdf, void* p, gsl_vector* initX) {
	for (int i = 0; i < N; i++) {
		gsl_vector_set(x, i, gsl_vector_get(initX, i));
	}
	myfundf.f = f;
	myfundf.df = df;
	myfundf.fdf = fdf;
	myfundf.params = p;
	gsl_multimin_fdfminimizer_set(minidf, &myfundf, x, INIT_STEP_SIZE, TOLERANCE);
}

double GradientDescent::optimize() {
	size_t iter = 0;
	int status;
	double size;
	double fval;
	const gsl_vector* grad;
	double diff;
	
	bool first = true;
	
	do
	{
		iter++;
		status = gsl_multimin_fdfminimizer_iterate (minidf);
		//cout << gsl_strerror(status) << endl;
		if (status) {
			cout << gsl_strerror(status) << endl;
			break;
		}
		grad = minidf->gradient;
		status = gsl_multimin_test_gradient (grad, GRAD_PRECISION);
		
		if (status == GSL_SUCCESS)
			printf ("Minimum found at:\n");

		diff = fval - minidf->f;
		fval = minidf->f;
		if (first) {
			first = false;
			diff = 10;
		}
	}
	while (status == GSL_CONTINUE && iter < ITERATIONS && fval >= PRECISION && diff >= PRECISION);
	cout << "Iterations: " << iter << endl;
	cout << status << endl;
	cout << "Ending search..." << endl;
	for (int i = 0; i < minidf->x->size; i++) {
		cout << gsl_vector_get (minidf->x, i) << ", ";
	}
	cout << endl;
	cout << "Optimal value found: " << minidf->f << endl;
	double val = minidf->f;
	gsl_multimin_fdfminimizer_restart(minidf);
	return val;
}
