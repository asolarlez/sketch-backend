#include "GradientDescent.h"
#include <iostream>

void GradientDescent::init(GD_F_TYPE f, GD_DF_TYPE df, GD_FDF_TYPE fdf, void* p, gsl_vector* initX) {
	x = gsl_vector_alloc(N);
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
	
	do
	{
		iter++;
		status = gsl_multimin_fdfminimizer_iterate (minidf);
		//cout << gsl_strerror(status) << endl;
		if (status)
			break;
		grad = minidf->gradient;
		status = gsl_multimin_test_gradient (grad, GRAD_PRECISION);
		
		if (status == GSL_SUCCESS)
			printf ("Minimum found at:\n");

		fval = minidf->f;
	}
	while (status == GSL_CONTINUE && iter < ITERATIONS && fval >= PRECISION);
	
	cout << "Ending search..." << endl;
	for (int i = 0; i < minidf->x->size; i++) {
		cout << "Parameter #" << i << ": " << gsl_vector_get (minidf->x, i) << "; " << endl;
	}
	cout << "Optimal value found: " << minidf->f << endl;
	return minidf->f;
}
