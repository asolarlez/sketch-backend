#include "DistanceGrad.h"

void DistanceGrad::dg_and(DistanceGrad* mdist, DistanceGrad* fdist, DistanceGrad* dg) {
	if (mdist->set && fdist->set) {
		vector<float> vals;
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
		vector<float> vals;
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

void DistanceGrad::dg_copy(DistanceGrad* m, DistanceGrad* o) {
	if (!m->set) {
		o->set = false;
		return;
	}
	o->set = true;
	o->dist = m->dist;
	gsl_blas_dcopy(m->grad, o->grad);
}


