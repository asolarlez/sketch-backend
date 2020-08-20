#pragma once

#include <memory.h>
#include <cmath>
#include "BasicError.h"


#ifdef _NOGSL
class alignas(64) gsl_vector {
public:
	int size;
	char B[60];
	double data[];
	gsl_vector(int _size):size(_size) {

	}
};

inline double gsl_vector_get(const gsl_vector* v, int idx) { return v->data[idx]; }
inline void gsl_vector_set(gsl_vector* v, int idx, double val) { v->data[idx] = val;  }
inline gsl_vector* gsl_vector_alloc(int size) { 
#ifdef _mm_malloc
	return new (_mm_malloc(sizeof(gsl_vector) + size * sizeof(double), 64)) gsl_vector(size); 
#else
	return new (aligned_alloc(64, sizeof(gsl_vector) + size * sizeof(double))) gsl_vector(size);
#endif
}
inline void gsl_vector_free(gsl_vector* v) {
	v->~gsl_vector();
#ifdef _mm_malloc
	_mm_free(v);
#else
	free(v);
#endif
}



inline void gsl_blas_ddot(const gsl_vector * X,
	const gsl_vector * Y,
	double * result
) {
	double rv = 0.0;
	for (int i = 0; i < X->size; ++i) {
		double t = X->data[i]*Y->data[i];
		rv += t;
	}
	*result = rv;
}


inline double gsl_blas_dnrm2(const gsl_vector* in) { 
	double rv = 0.0;
	for (int i = 0; i < in->size; ++i) {
		double t = in->data[i];
		rv += t*t;
	}
	return sqrt(rv); 
}

inline void gsl_vector_memcpy(gsl_vector* out, const gsl_vector* in) {
	Assert(in->size == out->size, "WRONG SIZE");
	memcpy(out->data, in->data, sizeof(double)*in->size);
}

inline void gsl_blas_dcopy(gsl_vector* in, gsl_vector* out) {
	Assert(in->size == out->size, "WRONG SIZE");
	memcpy(out->data, in->data, sizeof(double)*in->size);
}

inline void gsl_vector_scale(gsl_vector* in, double scale) {
	for (int i = 0; i < in->size; ++i) {
		in->data[i] *= scale;
	}
}

inline void gsl_blas_dscal(double scale, gsl_vector* in) {
	for (int i = 0; i < in->size; ++i) {
		in->data[i] *= scale;
	}
}

inline void gsl_blas_daxpy(double a, gsl_vector* x, gsl_vector* y) {
	for (int i = 0; i < x->size; ++i) {
		y->data[i] = a*x->data[i] + y->data[i];
	}
}

inline void gsl_vector_sub(gsl_vector* out, gsl_vector* sust) {
	for (int i = 0; i < out->size; ++i) {
		out->data[i] -= sust->data[i];
	}
}

inline void gsl_vector_add(gsl_vector* out, gsl_vector* ad) {
	for (int i = 0; i < out->size; ++i) {
		out->data[i] += ad->data[i];
	}
}

inline void gsl_vector_set_zero(gsl_vector* in) {
	for (int i = 0; i < in->size; ++i) {
		in->data[i] =0.0;
	}
}
#endif 


