#include "GradientDescent.h"
#include "NumericalSolver.h"

void GradientDescent::init(NumericalSolver* ns, const vector<vector<int>>& allInputs, gsl_vector* prev) {
	x = gsl_vector_alloc(N);
	for (int i = 0; i < N; i++) {
		gsl_vector_set(x, i, gsl_vector_get(prev, i));
	}
	myfundf.f = &ns->eval_f;
	myfundf.df = &ns->eval_df;
	myfundf.fdf = &ns->eval_fdf;
	Parameters* p = new Parameters();
	p->allInputs = allInputs;
	p->ns = ns;
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
		grad = gsl_multimin_fdfminimizer_gradient(minidf);
		size = gsl_blas_dasum(grad);
		status = gsl_multimin_test_gradient (grad, 1e-5);
		
		if (status == GSL_SUCCESS)
			printf ("Minimum found at:\n");
		// printOutput(iter);
		fval = minidf->f;
	}
	while (status == GSL_CONTINUE && iter < 400 && fval > 1e-5);
	
	cout << "Ending search..." << endl;
	for (int i = 0; i < minidf->x->size; i++) {
		cout << "Parameter #" << i << ": " << gsl_vector_get (minidf->x, i) << "; " << endl;
	}
	cout << "Optimal value found: " << minidf->f << endl;
	return minidf->f;
}
