#include "DistanceGrad.h"
#include "ValueGrad.h"

void DistanceGrad::dg_and(DistanceGrad* mdist, DistanceGrad* fdist, DistanceGrad* dg) {
	if (mdist->set && fdist->set) {
		vector<double> vals;
		vector<gsl_vector*> grads;
		vals.push_back(mdist->dist);
		vals.push_back(fdist->dist);
		grads.push_back(mdist->grad);
		grads.push_back(fdist->grad);
		dg->dist = GradUtil::findMin(vals, grads, dg->grad);
		dg->set = true;
	} else if (mdist->set) {
		if (mdist->dist < 0) {
			dg->dist = mdist->dist;
			gsl_vector_memcpy(dg->grad, mdist->grad);
			dg->set = true;
		} else {
			dg->set = false;
		}
	} else if (fdist->set) {
		if (fdist->dist < 0) {
			dg->dist = fdist->dist;
			gsl_vector_memcpy(dg->grad, fdist->grad);
			dg->set = true;
		} else {
			dg->set = false;
		}
	} else {
		dg->set = false;
	}
}

void DistanceGrad::dg_or(DistanceGrad* mdist, DistanceGrad* fdist, DistanceGrad* dg) {
	if (mdist->set && fdist->set) {
		vector<double> vals;
		vector<gsl_vector*> grads;
		vals.push_back(mdist->dist);
		vals.push_back(fdist->dist);
		grads.push_back(mdist->grad);
		grads.push_back(fdist->grad);
		dg->dist = GradUtil::findMax(vals, grads, dg->grad);
		dg->set = true;
	} else if (mdist->set) {
		if (mdist->dist > 0) {
			dg->dist = mdist->dist;
			gsl_vector_memcpy(dg->grad, mdist->grad);
			dg->set = true;
		} else {
			dg->set = false;
		}
	} else if (fdist->set) {
		if (fdist->dist > 0) {
			dg->dist = fdist->dist;
			gsl_vector_memcpy(dg->grad, fdist->grad);
			dg->set = true;
		} else {
			dg->set = false;
		}
	} else {
		dg->set = false;
	}
}

void DistanceGrad::dg_not(DistanceGrad* mdist, DistanceGrad* dg) {
	if (mdist->set) {
		dg->dist = - mdist->dist;
		gsl_vector_memcpy(dg->grad, mdist->grad);
		gsl_vector_scale(dg->grad, -1.0);
		dg->set = true;
	} else {
		dg->set = false;
	}
}

void DistanceGrad::dg_ite(DistanceGrad* bdist, DistanceGrad* mdist, DistanceGrad* fdist, DistanceGrad* dg) {
	if (bdist->set) {
		DistanceGrad* tmpd1 = new DistanceGrad(0.0, GradUtil::tmp);
		dg_or(bdist, mdist, tmpd1);
		DistanceGrad* tmpd2 = new DistanceGrad(0.0, GradUtil::tmp1);
		dg_not(bdist, tmpd2);
		DistanceGrad* tmpd3 = new DistanceGrad(0.0, GradUtil::tmp2);
		dg_or(tmpd2, fdist, tmpd3);
		dg_and(tmpd1, tmpd3, dg);
	} else {
		dg_or(mdist, fdist, dg);
	}	
}

double DistanceGrad::dg_copy(DistanceGrad* m, gsl_vector* grad) {
	gsl_blas_dcopy(m->grad, grad);
	return m->dist;
}

void DistanceGrad::dg_copy(DistanceGrad* m, DistanceGrad* o) {
	if (!m->set) {
		o->set = false;
		return;
	}
	o->set = true;
	o->dist = m->dist;
	gsl_blas_dcopy(m->grad, o->grad);
}

// Approximate ite with d*f + (1 - d)*m
void DistanceGrad::dg_ite(DistanceGrad* m, DistanceGrad* f, double dval, gsl_vector* dgrad, DistanceGrad* o) {
    Assert(dgrad != GradUtil::tmp && dgrad != GradUtil::tmp1, "Reusing temp gsl_vectors");
    if (!m->set || !f->set) {
        o->set = false;
        return;
    }
    double v1 = dval * f->dist;
    GradUtil::compute_mult_grad(dval, f->dist, dgrad, f->grad, o->grad);
    
    GradUtil::compute_neg_grad(dgrad, GradUtil::tmp);
    double v2 = (1 - dval) * m->dist;
    GradUtil::compute_mult_grad(1 - dval, m->dist, GradUtil::tmp, m->grad, GradUtil::tmp1);
    gsl_blas_daxpy(1.0, GradUtil::tmp1, o->grad);
    
    o->dist = v1 + v2;
    o->set = true;
}

bool DistanceGrad::same(DistanceGrad* m, DistanceGrad* f) {
	if (m == f) return true;

	if (abs(m->dist - f->dist) > 1e-2) return false;

	for (int i = 0; i < m->grad->size; i++) {
		double g1 = gsl_vector_get(m->grad, i);
		double g2 = gsl_vector_get(f->grad, i);
		if (abs(g1 - g2) > 1e-2) return false;
	}
	return true;
}

double DistanceGrad::dg_combine(DistanceGrad* m, DistanceGrad* f) {
	if (same(m, f)) return m->dist;
	return m->dist  * f->dist;
}

double DistanceGrad::dg_times(DistanceGrad* m, DistanceGrad* f, gsl_vector* grad) {
	double v = m->dist * f->dist;
    GradUtil::compute_mult_grad(m->dist, f->dist, m->grad, f->grad, grad);
    return v;
}


// o = mf
void DistanceGrad::dg_combine(DistanceGrad* m, DistanceGrad* f, DistanceGrad* o) {
	if (!m->set || !f->set) {
        o->set = false;
        Assert(false, "iehwp");
        return;
    }
    if (same(m, f)) {
    	dg_copy(m, o);
    	return;
    }
    double v = m->dist * f->dist;
    GradUtil::compute_mult_grad(m->dist, f->dist, m->grad, f->grad, o->grad);
    o->dist = v;
    o->set = true;
}

// o = m*f* sigmoid(cval) if bv = 1
// o = m*f* sigmoid(-cval) if bv = 0 
double DistanceGrad::dg_combine(DistanceGrad* m, DistanceGrad* f, double cval, gsl_vector* grad, int bv, gsl_vector* o) {

	Assert(o != GradUtil::tmp, "Reusing tmp vectors");
	Assert(o != GradUtil::tmp1, "Reusing tmp vectors");
	Assert(o != GradUtil::tmp2, "Reusing tmp vectors");

	Assert(grad != GradUtil::tmp, "Reusing tmp vectors");
	Assert(grad != GradUtil::tmp1, "Reusing tmp vectors");
	Assert(grad != GradUtil::tmp2, "Reusing tmp vectors");
	if (!m->set || !f->set) {
        Assert(false, "iehwp");
    }
    double v;
    if (same(m, f)) {
    	v = m->dist;
    	gsl_vector_memcpy(GradUtil::tmp, m->grad);
    } else {
    	v = m->dist * f->dist;
    	GradUtil::compute_mult_grad(m->dist, f->dist, m->grad, f->grad, GradUtil::tmp);
    }
    double s;
    if (bv == 1) {
    	s = GradUtil::sigmoid(cval, grad, GradUtil::tmp1);
    } else {
    	GradUtil::compute_neg_grad(grad, GradUtil::tmp2);
    	s = GradUtil::sigmoid(-cval, GradUtil::tmp2, GradUtil::tmp1);
    }
	GradUtil::compute_mult_grad(s, v, GradUtil::tmp1, GradUtil::tmp, o);
	return v*s;
}

void DistanceGrad::dg_combine(vector<DistanceGrad*>& cdists, DistanceGrad* vdist, vector<ValueGrad*>& cvals, int bv, DistanceGrad* o) {
	double sum = 0;
	GradUtil::default_grad(o->grad);
	for (int i = 0; i < cdists.size(); i++) {
		sum += dg_combine(cdists[i], vdist, cvals[i]->getVal(), cvals[i]->getGrad(), bv, GradUtil::tmp3);
		gsl_blas_daxpy(1.0, GradUtil::tmp3, o->grad);
	}
	o->dist = sum;
	o->set = true;
}

double DistanceGrad::dg_combine(DistanceGrad* m, DistanceGrad* f, double cval, int bv) {
	double v;
    if (same(m, f)) {
    	v = m->dist;
    } else {
    	v = m->dist * f->dist;
    }
    double s;
    if (bv == 1) {
    	s = GradUtil::sigmoid(cval);
    } else {
    	s = GradUtil::sigmoid(-cval);
    }
	return v*s;
}
