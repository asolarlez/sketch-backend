#include "GradientDescent.h"
#include <iostream>
#include <cassert>
#include <cmath>

using namespace std;

void testQuad() {
	class Quad {
	public:
		static double eval_f(const gsl_vector* x, void* params) {
			double v = gsl_vector_get(x, 0);
			cout << v << endl;
			return v*v;
		}
		
		static void eval_df(const gsl_vector* x, void* params, gsl_vector* d) {
			double v = gsl_vector_get(x, 0);
			gsl_vector_set(d, 0, 2*v);
		}
		
		static void eval_fdf(const gsl_vector* x, void* params, double* f, gsl_vector* df) {
			double v = gsl_vector_get(x, 0);
			*f = v*v;
			cout << v << endl;
			gsl_vector_set(df, 0, 2*v);
		}
	};

	GradientDescent* gd = new GradientDescent(1);
	gsl_vector* init = gsl_vector_alloc(1);
	gsl_vector_set(init, 0, -100.0);
	gd->init(Quad::eval_f, Quad::eval_df, Quad::eval_fdf, NULL);
	double opt = gd->optimize(init);
	assert(abs(opt) <= gd->PRECISION);
}

void testQuad2() {
	class Quad {
		public:
		static double eval_f(const gsl_vector* x, void* params) {
			double v1 = gsl_vector_get(x, 0);
			double v2 = gsl_vector_get(x, 1);
			cout << v1 << " " << v2 << endl;
			return v1*v1 + 10*v2*v2;
		}
		
		static void eval_df(const gsl_vector* x, void* params, gsl_vector* d) {
			double v1 = gsl_vector_get(x, 0);
			double v2 = gsl_vector_get(x, 1);
			gsl_vector_set(d, 0, 2*v1);
			gsl_vector_set(d, 1, 20*v2);
		}
		
		static void eval_fdf(const gsl_vector* x, void* params, double* f, gsl_vector* df) {
			double v1 = gsl_vector_get(x, 0);
			double v2 = gsl_vector_get(x, 1);
			*f = v1*v1 + 10*v2*v2;
			cout << v1 << " " << v2 << endl;
			gsl_vector_set(df, 0, 2*v1);
			gsl_vector_set(df, 1, 20*v2);
		}
	};
	
	GradientDescent* gd = new GradientDescent(2);
	gsl_vector* init = gsl_vector_alloc(2);
	gsl_vector_set(init, 0, -100.0);
	gsl_vector_set(init, 1, -100.0);
	gd->init(Quad::eval_f, Quad::eval_df, Quad::eval_fdf, NULL);
	double opt = gd->optimize(init);
	assert(abs(opt) <= gd->PRECISION);
}

int main() {
	testQuad();
	testQuad2();
	
	cout << "Passed tests" << endl;
	return 0;
}


