#include "GradientDescent.h"
#include "NumericalSolver.h"
#include <iostream>

void GradientDescent::init(GD_F_TYPE f, GD_DF_TYPE df, GD_FDF_TYPE fdf, void* p, gsl_vector* initX) {
	for (int i = 0; i < N; i++) {
		gsl_vector_set(x, i, gsl_vector_get(initX, i));
	}
	myfundf.f = f;
	myfundf.df = df;
	myfundf.fdf = fdf;
	myfundf.params = p;
	((GDParameters*)myfundf.params)->beta = -10;
	((GDParameters*)myfundf.params)->alpha = 10;
	gsl_multimin_fdfminimizer_set(minidf, &myfundf, x, INIT_STEP_SIZE, TOLERANCE);
}

double GradientDescent::optimize() {
	size_t iter = 0;
	int status;
	double size;
	double fval;
	const gsl_vector* grad;
	double diff;
	//double betas[4] = {-1, -10, -50, -100};
	//double alphas[4] = {1, 10, 50, 100};
	double betas[1] = {-10};
	double alphas[1] = {10};
	for (int i = 0; i < 1; i++) {
		cout << "Beta: " << betas[i] << " Alpha: " << alphas[i] << endl;
		((GDParameters*)myfundf.params)->beta = betas[i];
		((GDParameters*)myfundf.params)->alpha = alphas[i];
		if (i != 0) {
			for (int i = 0; i < N; i++) {
				gsl_vector_set(x, i, gsl_vector_get(minidf->x, i));
			}
		}
		gsl_multimin_fdfminimizer_set(minidf, &myfundf, x, INIT_STEP_SIZE, TOLERANCE);
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
		//cout << gsl_strerror(status) << endl;
		cout << "Ending search..." << endl;
		for (int i = 0; i < minidf->x->size; i++) {
			cout << gsl_vector_get (minidf->x, i) << ", ";
		}
		cout << endl;
		cout << "Optimal value found: " << minidf->f << endl;

		//if (minidf->f > 1.0) {
		//	break;
		//}
	}
	double val = minidf->f;
	gsl_multimin_fdfminimizer_restart(minidf);
	return val;
}
