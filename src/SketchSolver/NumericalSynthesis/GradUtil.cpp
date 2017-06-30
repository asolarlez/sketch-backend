#include "IntervalGrad.h"

gsl_vector* GradUtil::tmp;
gsl_vector* GradUtil::tmp1;
gsl_vector* GradUtil::tmp2;
gsl_vector* GradUtil::tmp3;
gsl_vector* GradUtil::tmpT;

float GradUtil::BETA;
float GradUtil::ALPHA;

float GradUtil::findMin(float val1, float val2, gsl_vector* grad1, gsl_vector* grad2, gsl_vector* l) {
	vector<float> vals;
	vector<gsl_vector*> grads;
	vals.push_back(val1);
	vals.push_back(val2);
	grads.push_back(grad1);
	grads.push_back(grad2);
	
	return findMin(vals, grads, l);
}

float GradUtil::findMin(const vector<float>& vals, const vector<gsl_vector*>& grads, gsl_vector* l) {
	float minv = MAXVAL;
	for (int i = 0; i < vals.size(); i++) {
		if (vals[i] < minv) {
			minv = vals[i];
		}
	}
	return softMinMax(vals, grads, l, -ALPHA, minv);
	//return minv;
}

float GradUtil::findMax(float val1, float val2, gsl_vector* grad1, gsl_vector* grad2, gsl_vector* h) {
	vector<float> vals;
	vector<gsl_vector*> grads;
	vals.push_back(val1);
	vals.push_back(val2);
	grads.push_back(grad1);
	grads.push_back(grad2);
	
	return findMax(vals, grads, h);
}

float GradUtil::findMax(const vector<float>& vals, const vector<gsl_vector*>& grads, gsl_vector* h) {
	float maxv = MINVAL;
	for (int i = 0; i < vals.size(); i++) {
		if (vals[i] > maxv) {
			maxv = vals[i];
		}
	}
	return softMinMax(vals, grads, h, ALPHA, maxv);
	//return maxv;
}

float GradUtil::sigmoid(float x, gsl_vector* grads, gsl_vector* out) {
	float scale = BETA;// BETA/max(gsl_blas_dnrm2(grads), 1.0); // TODO: is this normalization correct?
	float v = 1.0/(1.0 + exp(scale * x));
	float d = -1.0*scale*v*(1-v);
	gsl_blas_dcopy(grads, out);
	gsl_blas_dscal(d, out);
	return v;
}

/* Computes the gradient of log(e^(alpha*(v1 - t)) + e^(alpha*(v2 - t)) ... )
 If alpha > 0, the result is the soft max of the values
 If alpha < 0, the result is the soft min of the values */
float GradUtil::softMinMax(const vector<float>& vals, const vector<gsl_vector*>& grads, gsl_vector* l, float alpha, float t) {
	float expval = 0.0;
	for (int i = 0; i < vals.size(); i++) {
		expval += exp(alpha * (vals[i] - t));
	}
	gsl_vector_set_zero(l);
	for (int i = 0; i < vals.size(); i++) {
		gsl_blas_daxpy(exp(alpha*(vals[i] - t)), grads[i], l);
	}
	gsl_blas_dscal(1.0/expval, l);
	return (log(expval) + alpha * t)/alpha;
}

void GradUtil::default_grad(gsl_vector* out) {
	gsl_vector_set_zero(out);
}

// computes the grad of mval + fval i.e. mgrads + fgrads
void GradUtil::compute_plus_grad(gsl_vector* mgrads, gsl_vector* fgrads, gsl_vector* out) {
	gsl_blas_dcopy(mgrads, out);
	gsl_blas_daxpy(1.0, fgrads, out);
}

// computes the grad of mval * fval i.e. mval*fgrads + fval*mgrads
void GradUtil::compute_mult_grad(float mval, float fval, gsl_vector* mgrads, gsl_vector* fgrads, gsl_vector* out) {
	gsl_blas_dcopy(mgrads, out);
	gsl_blas_dscal(fval, out);
	gsl_blas_daxpy(mval, fgrads, out);
}

// computes the grad of mval / fval i.e (fval * mgrads - mval * fgrads) / (fval * fval)
void GradUtil::compute_div_grad(float mval, float fval, gsl_vector* mgrads, gsl_vector* fgrads, gsl_vector* out) {
	Assert(mgrads != tmpT && fgrads != tmpT, "Error: tmp will be overwritten");
	gsl_blas_dcopy(mgrads, out);
	gsl_blas_dscal(fval, out);
	gsl_blas_daxpy(-mval, fgrads, out);
	gsl_blas_dscal(1.0/(fval*fval), out);
}

void GradUtil::compute_neg_grad(gsl_vector* mgrads, gsl_vector* out) {
	gsl_blas_dcopy(mgrads, out);
	gsl_blas_dscal(-1.0, out);
}

void GradUtil::compute_square_grad(float mval, gsl_vector* mgrads, gsl_vector* out) {
	float dm = 2.0 * mval;
	gsl_blas_dcopy(mgrads, out);
	gsl_blas_dscal(dm, out);
}

void GradUtil::compute_arctan_grad(float mval, gsl_vector* mgrads, gsl_vector* out) {
	float dm = 1.0/(mval * mval + 1.0);
	gsl_blas_dcopy(mgrads, out);
	gsl_blas_dscal(dm, out);
}

void GradUtil::compute_sin_grad(float mval, gsl_vector* mgrads, gsl_vector* out) {
	float dm = cos(mval);
	gsl_blas_dcopy(mgrads, out);
	gsl_blas_dscal(dm, out);
}

void GradUtil::compute_cos_grad(float mval, gsl_vector* mgrads, gsl_vector* out) {
	float dm = -sin(mval);
	gsl_blas_dcopy(mgrads, out);
	gsl_blas_dscal(dm, out);
}

void GradUtil::compute_tan_grad(float mval, gsl_vector* mgrads, gsl_vector* out) {
	float dm = 1.0/(cos(mval)*cos(mval));
	gsl_blas_dcopy(mgrads, out);
	gsl_blas_dscal(dm, out);
}

void GradUtil::compute_sqrt_grad(float mval, gsl_vector* mgrads, gsl_vector* out) {
	float dm = 0.5/sqrt(mval);
	gsl_blas_dcopy(mgrads, out);
	gsl_blas_dscal(dm, out);
}

void GradUtil::compute_exp_grad(float mval, gsl_vector* mgrads, gsl_vector* out) {
	float dm = exp(mval);
	gsl_blas_dcopy(mgrads, out);
	gsl_blas_dscal(dm, out);
}
