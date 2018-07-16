#include "IntervalGrad.h"

gsl_vector* GradUtil::tmp;
gsl_vector* GradUtil::tmp1;
gsl_vector* GradUtil::tmp2;
gsl_vector* GradUtil::tmp3;
gsl_vector* GradUtil::tmpT;

double GradUtil::BETA;
double GradUtil::ALPHA;

void GradUtil::allocateTempVectors(int size) {
    tmp = gsl_vector_alloc(size);
    tmp1 = gsl_vector_alloc(size);
    tmp2 = gsl_vector_alloc(size);
    tmp3 = gsl_vector_alloc(size);
    tmpT = gsl_vector_alloc(size);
}

void GradUtil::clearTempVectors() {
    gsl_vector_free(tmp);
	gsl_vector_free(tmp1);
	gsl_vector_free(tmp2);
	gsl_vector_free(tmp3);
	gsl_vector_free(tmpT);
}

double GradUtil::findMin(double val1, double val2, gsl_vector* grad1, gsl_vector* grad2, gsl_vector* l) {
	vector<double> vals;
	vector<gsl_vector*> grads;
	vals.push_back(val1);
	vals.push_back(val2);
	grads.push_back(grad1);
	grads.push_back(grad2);
	
	return findMin(vals, grads, l);
}

double GradUtil::findMin(const vector<double>& vals, const vector<gsl_vector*>& grads, gsl_vector* l) {
	double minv = MAXVAL;
	for (int i = 0; i < vals.size(); i++) {
		if (vals[i] < minv) {
			minv = vals[i];
		}
	}
	return softMinMax(vals, grads, l, -ALPHA, minv);
	//return minv;
}

double GradUtil::findMax(double val1, double val2, gsl_vector* grad1, gsl_vector* grad2, gsl_vector* h) {
	vector<double> vals;
	vector<gsl_vector*> grads;
	vals.push_back(val1);
	vals.push_back(val2);
	grads.push_back(grad1);
	grads.push_back(grad2);
	
	return findMax(vals, grads, h);
}

double GradUtil::findMax(const vector<double>& vals, const vector<gsl_vector*>& grads, gsl_vector* h) {
	double maxv = MINVAL;
	for (int i = 0; i < vals.size(); i++) {
		if (vals[i] > maxv) {
			maxv = vals[i];
		}
	}
	return softMinMax(vals, grads, h, ALPHA, maxv);
	//return maxv;
}

double GradUtil::findMax(const vector<double>& vals) {
	double maxv = MINVAL;
	for (int i = 0; i < vals.size(); i++) {
		if (vals[i] > maxv) {
			maxv = vals[i];
		}
	}
	return softMinMax(vals, ALPHA, maxv);
}

double GradUtil::sigmoid(double x, gsl_vector* grads, gsl_vector* out) {
	double scale = BETA;
    double v = 1.0/(1.0 + exp(scale * x));
	double d = -1.0*scale*v*(1-v);
    if (d > 1e3) {
        cout << "LARGE GRADIENT FACTOR in sigmoid " << d << endl;
    }
	gsl_blas_dcopy(grads, out);
	gsl_blas_dscal(d, out);
	return v;
}

double GradUtil::sigmoid(double x) {
	double scale = BETA;
    double v = 1.0/(1.0 + exp(scale * x));
    return v;
}

/* Computes the gradient of log(e^(alpha*(v1 - t)) + e^(alpha*(v2 - t)) ... )
 If alpha > 0, the result is the soft max of the values
 If alpha < 0, the result is the soft min of the values */
double GradUtil::softMinMax(const vector<double>& vals, const vector<gsl_vector*>& grads, gsl_vector* l, double alpha, double t) {
	double expval = 0.0;
	for (int i = 0; i < vals.size(); i++) {
		expval += exp(alpha * (vals[i] - t));
	}
	gsl_vector_set_zero(l);
	for (int i = 0; i < vals.size(); i++) {
        if (exp(alpha * (vals[i] - t)) / expval > 1e3) {
            cout << "LARGE GRADIENT FACTOR in softMinMax" << endl;
        }
		gsl_blas_daxpy(exp(alpha*(vals[i] - t)), grads[i], l);
	}
	gsl_blas_dscal(1.0/expval, l);
	return (log(expval) + alpha * t)/alpha;
}

double GradUtil::softMinMax(const vector<double>& vals, double alpha, double t) {
	double expval = 0.0;
	for (int i = 0; i < vals.size(); i++) {
		expval += exp(alpha * (vals[i] - t));
	}
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
void GradUtil::compute_mult_grad(double mval, double fval, gsl_vector* mgrads, gsl_vector* fgrads, gsl_vector* out) {
	gsl_blas_dcopy(mgrads, out);
	gsl_blas_dscal(fval, out);
	gsl_blas_daxpy(mval, fgrads, out);
}

// computes the grad of mval / fval i.e (fval * mgrads - mval * fgrads) / (fval * fval)
void GradUtil::compute_div_grad(double mval, double fval, gsl_vector* mgrads, gsl_vector* fgrads, gsl_vector* out) {
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

void GradUtil::compute_square_grad(double mval, gsl_vector* mgrads, gsl_vector* out) {
	double dm = 2.0 * mval;
	gsl_blas_dcopy(mgrads, out);
	gsl_blas_dscal(dm, out);
}

void GradUtil::compute_arctan_grad(double mval, gsl_vector* mgrads, gsl_vector* out) {
	double dm = 1.0/(mval * mval + 1.0);
	gsl_blas_dcopy(mgrads, out);
	gsl_blas_dscal(dm, out);
}

void GradUtil::compute_sin_grad(double mval, gsl_vector* mgrads, gsl_vector* out) {
	double dm = cos(mval);
	gsl_blas_dcopy(mgrads, out);
	gsl_blas_dscal(dm, out);
}

void GradUtil::compute_cos_grad(double mval, gsl_vector* mgrads, gsl_vector* out) {
	double dm = -sin(mval);
	gsl_blas_dcopy(mgrads, out);
	gsl_blas_dscal(dm, out);
}

void GradUtil::compute_tan_grad(double mval, gsl_vector* mgrads, gsl_vector* out) {
	double dm = 1.0/(cos(mval)*cos(mval));
	gsl_blas_dcopy(mgrads, out);
	gsl_blas_dscal(dm, out);
}

void GradUtil::compute_sqrt_grad(double mval, gsl_vector* mgrads, gsl_vector* out) {
	double dm = 0.5/sqrt(mval);
	gsl_blas_dcopy(mgrads, out);
	gsl_blas_dscal(dm, out);
}

void GradUtil::compute_exp_grad(double mval, gsl_vector* mgrads, gsl_vector* out) {
	double dm = exp(mval);
	gsl_blas_dcopy(mgrads, out);
	gsl_blas_dscal(dm, out);
}
