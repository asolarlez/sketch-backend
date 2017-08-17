#include "IntervalGrad.h"

void IntervalGrad::ig_plus(IntervalGrad* m, IntervalGrad* f, IntervalGrad* o) {
	float mlval = m->getLow();
	float mhval = m->getHigh();
	float flval = f->getLow();
	float fhval = f->getHigh();
	float lval = mlval + flval;
	float hval = mhval + fhval;
	o->update(lval, hval);
	o->singleton = m->singleton && f->singleton;
	
	gsl_vector* mlgrads = m->getLGrad();
	gsl_vector* flgrads = f->getLGrad();
	gsl_vector* mhgrads = m->getHGrad();
	gsl_vector* fhgrads = f->getHGrad();
	GradUtil::compute_plus_grad(mlgrads, flgrads, o->getLGrad());
	GradUtil::compute_plus_grad(mhgrads, fhgrads, o->getHGrad());
	o->bound();
}

void IntervalGrad::ig_times(IntervalGrad* m, IntervalGrad* f, IntervalGrad* o) {
	gsl_vector* mlgrads = m->getLGrad();
	gsl_vector* flgrads = f->getLGrad();
	gsl_vector* mhgrads = m->getHGrad();
	gsl_vector* fhgrads = f->getHGrad();
	
	vector<float> vals;
	vector<gsl_vector*> grads;
	
	float mlval = m->getLow();
	float mhval = m->getHigh();
	float flval = f->getLow();
	float fhval = f->getHigh();
	
	float v1 = mlval * flval;
	float v2 = mlval * fhval;
	float v3 = mhval * flval;
	float v4 = mhval * fhval;
	//cout << v1 << " " << v2 << " " << v3 << " " << v4 << endl;
		
	vals.push_back(v1);
	vals.push_back(v2);
	vals.push_back(v3);
	vals.push_back(v4);
	
	GradUtil::compute_mult_grad(mlval, flval, mlgrads, flgrads, GradUtil::tmp);
	GradUtil::compute_mult_grad(mlval, fhval, mlgrads, fhgrads, GradUtil::tmp1);
	GradUtil::compute_mult_grad(mhval, flval, mhgrads, flgrads, GradUtil::tmp2);
	GradUtil::compute_mult_grad(mhval, fhval, mhgrads, fhgrads, GradUtil::tmp3);
	grads.push_back(GradUtil::tmp);
	grads.push_back(GradUtil::tmp1);
	grads.push_back(GradUtil::tmp2);
	grads.push_back(GradUtil::tmp3);
	
	if (m->singleton && f->singleton) {
		float minv = vals[0];
		float maxv = vals[0];
		gsl_blas_dcopy(grads[0], o->getLGrad());
		gsl_blas_dcopy(grads[0], o->getHGrad());
		o->update(minv, maxv);
		o->singleton = true;
	} else {
		float minv = GradUtil::findMin(vals, grads, o->getLGrad()); // Note: this computes a smooth approximate of min and not the actual value
		float maxv = GradUtil::findMax(vals, grads, o->getHGrad());
		o->update(minv, maxv);
		o->singleton = false;
	}
	o->bound();
}

void IntervalGrad::ig_div(IntervalGrad* m, IntervalGrad* f, IntervalGrad* o) {
	gsl_vector* mlgrads = m->getLGrad();
	gsl_vector* flgrads = f->getLGrad();
	gsl_vector* mhgrads = m->getHGrad();
	gsl_vector* fhgrads = f->getHGrad();
	
	vector<float> vals;
	vector<gsl_vector*> grads;
	
	float mlval = m->getLow();
	float mhval = m->getHigh();
	float flval = f->getLow();
	float fhval = f->getHigh();

	gsl_vector* l = o->getLGrad();
	gsl_vector* h = o->getHGrad();
	
	// Deal with the case when zero can be in the denominator
	if (flval <= 0 && fhval >= 0) { // Zero in the denomintor
		// TODO: check if this correct
		if ((mlval < 0 && mhval > 0) || (flval < 0 && fhval > 0)) {
			o->update(GradUtil::MINVAL, GradUtil::MAXVAL);
			GradUtil::default_grad(l); // TODO: what should be the gradients in this case?
			GradUtil::default_grad(h);
		} else if (flval == 0 && fhval == 0) {
			if (mlval >= 0) {
				o->update(GradUtil::MAXVAL, GradUtil::MAXVAL);
			} else if (mhval <= 0) {
				o->update(GradUtil::MINVAL, GradUtil::MINVAL);
			}
			// the case mlval < 0 and mhval > 0 is already handled.
			GradUtil::default_grad(l);
			GradUtil::default_grad(h);
		} else if (mlval >= 0 && flval == 0) {
			float minv = mlval/fhval;
			o->update(minv, GradUtil::MAXVAL);
			GradUtil::compute_div_grad(mlval, fhval, mlgrads, fhgrads, l);
			GradUtil::default_grad(h);
		} else if (mhval <= 0 && flval == 0) {
			float maxv = mhval/fhval;
			o->update(GradUtil::MINVAL, maxv);
			GradUtil::default_grad(l);
			GradUtil::compute_div_grad(mhval, fhval, mhgrads, fhgrads, h);
		} else if (mlval >= 0 && fhval == 0) {
			float maxv = mlval/flval;
			o->update(GradUtil::MINVAL, maxv);
			GradUtil::default_grad(l);
			GradUtil::compute_div_grad(mlval, flval, flgrads, mlgrads, h);
		} else if (mhval <= 0 && fhval == 0) {
			float minv = mhval/flval;
			o->update(minv, GradUtil::MAXVAL);
			GradUtil::compute_div_grad(mhval, flval, mhgrads, flgrads, l);
			GradUtil::default_grad(h);
		} else {
			Assert(false, "NYI: unaccounted case");
		}
		o->singleton = false;
		o->bound();
		return;
	}
	
	float v1 = mlval / flval;
	float v2 = mlval / fhval;
	float v3 = mhval / flval;
	float v4 = mhval / fhval;
	
	vals.push_back(v1);
	vals.push_back(v2);
	vals.push_back(v3);
	vals.push_back(v4);
	
	GradUtil::compute_div_grad(mlval, flval, mlgrads, flgrads, GradUtil::tmp);
	GradUtil::compute_div_grad(mlval, fhval, mlgrads, fhgrads, GradUtil::tmp1);
	GradUtil::compute_div_grad(mhval, flval, mhgrads, flgrads, GradUtil::tmp2);
	GradUtil::compute_div_grad(mhval, fhval, mhgrads, fhgrads, GradUtil::tmp3);
	grads.push_back(GradUtil::tmp);
	grads.push_back(GradUtil::tmp1);
	grads.push_back(GradUtil::tmp2);
	grads.push_back(GradUtil::tmp3);
	
	
	if (m->singleton && f->singleton) {
		float minv = vals[0];
		float maxv = vals[0];
		gsl_blas_dcopy(grads[0], l);
		gsl_blas_dcopy(grads[0], h);
		o->update(minv, maxv);
		o->singleton = true;
	} else {
		float minv = GradUtil::findMin(vals, grads, l); // Note: this computes a smooth approximate of min and not the actual value
		float maxv = GradUtil::findMax(vals, grads, h);
		o->update(minv, maxv);
		o->singleton = false;
	}
	o->bound();
}

void IntervalGrad::ig_neg(IntervalGrad* m, IntervalGrad* o) {
	o->update(- m->getHigh(), - m->getLow());
	o->singleton = m->singleton;
	gsl_vector* l = o->getLGrad();
	gsl_vector* h = o->getHGrad();
	GradUtil::compute_neg_grad(m->getHGrad(), l);
	GradUtil::compute_neg_grad(m->getLGrad(), h);
	o->bound();
}

void IntervalGrad::ig_union(const vector<IntervalGrad*>& m, IntervalGrad* o) {
	vector<float> lvals;
	vector<float> hvals;
	vector<gsl_vector*> lgrads;
	vector<gsl_vector*> hgrads;
	for (int i = 0; i < m.size(); i++) {
		lvals.push_back(m[i]->getLow());
		hvals.push_back(m[i]->getHigh());
		lgrads.push_back(m[i]->getLGrad());
		hgrads.push_back(m[i]->getHGrad());
	}
	float minv = GradUtil::findMin(lvals, lgrads, o->getLGrad());
	float maxv = GradUtil::findMax(hvals, hgrads, o->getHGrad());
	o->update(minv, maxv);
	o->singleton = false;
	o->bound();
}

void IntervalGrad::ig_conditionalUnion(IntervalGrad* m, IntervalGrad* f, DistanceGrad* d, IntervalGrad* o) {
	float s1 = GradUtil::sigmoid(d->dist, d->grad, GradUtil::tmp);
	float v1 = s1 * f->getLow();
	float v2 = s1 * f->getHigh();
	GradUtil::compute_mult_grad(s1, f->getLow(), GradUtil::tmp, f->getLGrad(), o->getLGrad());
	GradUtil::compute_mult_grad(s1, f->getHigh(), GradUtil::tmp, f->getHGrad(), o->getHGrad());
	
	GradUtil::compute_neg_grad(d->grad, GradUtil::tmp1);
	float s2 = GradUtil::sigmoid(-d->dist, GradUtil::tmp1, GradUtil::tmp);
	float v3 = s2*m->getLow();
	float v4 = s2*m->getHigh();
	GradUtil::compute_mult_grad(s2, m->getLow(), GradUtil::tmp, m->getLGrad(), GradUtil::tmp1);
	gsl_blas_daxpy(1.0, GradUtil::tmp1, o->getLGrad());
	GradUtil::compute_mult_grad(s2, m->getHigh(), GradUtil::tmp, m->getHGrad(), GradUtil::tmp1);
	gsl_blas_daxpy(1.0, GradUtil::tmp1, o->getHGrad());
	
	float minv = v1 + v3;
	float maxv = v2 + v4;
	o->update(minv, maxv);
	if (m->singleton && f->singleton) {
		Assert(minv == maxv, "sfiahoisf");
		o->singleton = true;
	} else {
		o->singleton = false;
	}
	o->bound();
	
	/*float s1 = IntervalGrad::sigmoid(d->dist - DELTA, d->grad, tmp);
	 float v1 = s1 * f->getLow();
	 float v4 = s1 * f->getHigh();
	 compute_mult_grad(s1, f->getLow(), tmp, f->getLGrad(), o->getLGrad());
	 compute_mult_grad(s1, f->getHigh(), tmp, f->getHGrad(), o->getHGrad());
	 
	 gsl_vector_memcpy(tmp1, d->grad);
	 gsl_vector_scale(tmp1, -1.0);
	 float s2 = IntervalGrad::sigmoid(-d->dist + DELTA, tmp1, tmp);
	 float s3 = IntervalGrad::sigmoid(d->dist + DELTA, d->grad, tmp1);
	 compute_mult_grad(s2, s3, tmp, tmp1, tmp2);
	 float minv = findMin(m->getLow(), f->getLow(), m->getLGrad(), f->getLGrad(), tmp);
	 compute_mult_grad(s2*s3, minv, tmp2, tmp, tmp1);
	 float v2 = s2*s3*minv;
	 gsl_vector_add(o->getLGrad(), tmp1);
	 
	 float maxv = findMax(m->getHigh(), f->getHigh(), m->getHGrad(), f->getHGrad(), tmp);
	 compute_mult_grad(s2*s3, maxv, tmp2, tmp, tmp1);
	 float v5 = s2*s3*maxv;
	 gsl_vector_add(o->getHGrad(), tmp1);
	 
	 gsl_vector_memcpy(tmp1, d->grad);
	 gsl_vector_scale(tmp1, -1.0);
	 float s4 = IntervalGrad::sigmoid(-d->dist - DELTA, tmp1, tmp);
	 float v3 = s4*m->getLow();
	 float v6 = s4*m->getHigh();
	 compute_mult_grad(s4, m->getLow(), tmp, m->getLGrad(), tmp1);
	 gsl_vector_add(o->getLGrad(), tmp1);
	 compute_mult_grad(s4, m->getHigh(), tmp, m->getHGrad(), tmp1);
	 gsl_vector_add(o->getHGrad(), tmp1);
	 
	 minv = v1 + v2 + v3;
	 maxv = v4 + v5 + v6;
	 o->update(minv, maxv);
	 if (((d->dist > 5*DELTA && f->singleton) || (d->dist < -5*DELTA && m->singleton))) {
		o->singleton = true;
	 } else {
		o->singleton = false;
	 }
	 o->bound();*/
}

void IntervalGrad::ig_intersect(const vector<IntervalGrad*>& m, IntervalGrad* o) {
	vector<float> lvals;
	vector<float> hvals;
	vector<gsl_vector*> lgrads;
	vector<gsl_vector*> hgrads;
	for (int i = 0; i < m.size(); i++) {
		lvals.push_back(m[i]->getLow());
		hvals.push_back(m[i]->getHigh());
		lgrads.push_back(m[i]->getLGrad());
		hgrads.push_back(m[i]->getHGrad());
	}
	float minv = GradUtil::findMax(lvals, lgrads, o->getLGrad());
	float maxv = GradUtil::findMin(hvals, hgrads, o->getHGrad());
	if (maxv < minv) {
		// empty set
	}
	o->update(minv, maxv);
	o->singleton = false;
	o->bound();
}




void IntervalGrad::ig_equal(IntervalGrad* m, IntervalGrad* f, DistanceGrad* o) {
	Assert(false, "NYI: IntervalGrad equal");
}

void IntervalGrad::ig_lt(IntervalGrad* m, IntervalGrad* f, DistanceGrad* o) {
	if (m->getHigh() <= f->getLow()) {
		// Definitely true
		o->dist = f->getLow()  - m->getHigh();
		gsl_vector_memcpy(o->grad, f->getLGrad());
		gsl_vector_sub(o->grad, m->getHGrad());
		o->set = true;
	} else if (m->getLow() > f->getHigh()) {
		// Definitely false
		o->dist = f->getHigh() - m->getLow();
		gsl_vector_memcpy(o->grad, f->getHGrad());
		gsl_vector_sub(o->grad, m->getLGrad());
		o->set = true;
	} else {
		// can be either
		o->dist = 0;
		GradUtil::default_grad(o->grad);
		o->set = true;
	}
}

void IntervalGrad::ig_square(IntervalGrad* m, IntervalGrad* o) {
	gsl_vector* mlgrads = m->getLGrad();
	gsl_vector* mhgrads = m->getHGrad();
	
	vector<float> vals;
	vector<gsl_vector*> grads;
	
	float mlval = m->getLow();
	float mhval = m->getHigh();
	
	float v1 = mlval * mlval;
	float v2 = mhval * mhval;
	
	vals.push_back(v1);
	vals.push_back(v2);
	
	GradUtil::compute_square_grad(mlval, mlgrads, GradUtil::tmp);
	GradUtil::compute_square_grad(mhval, mhgrads, GradUtil::tmp1);
	grads.push_back(GradUtil::tmp);
	grads.push_back(GradUtil::tmp1);
	
	if (mlval < 0 && mhval > 0) {
		float minv = 0.0;
		GradUtil::default_grad(o->getLGrad()); // TODO: what should be the gradient in this case?
		float maxv = GradUtil::findMax(vals, grads, o->getHGrad());
		o->update(minv, maxv);
		o->singleton = false;
	} else {
		if (m->singleton) {
			o->update(vals[0], vals[0]);
			o->singleton = true;
			gsl_blas_dcopy(grads[0], o->getLGrad());
			gsl_blas_dcopy(grads[0], o->getHGrad());
		} else {
			float minv = GradUtil::findMin(vals, grads, o->getLGrad());
			float maxv = GradUtil::findMax(vals, grads, o->getHGrad());
			o->update(minv, maxv);
			o->singleton = false;
		}
	}
	o->bound();
}

void IntervalGrad::ig_arctan(IntervalGrad* m, IntervalGrad* o) {
	float xl = m->getLow();
	float xh = m->getHigh();
	Assert(xl == xh, "NYI: range computation for arctan");
	o->update(atan(xl), atan(xh));
	o->singleton = m->singleton;
	GradUtil::compute_arctan_grad(xl, m->getLGrad(), o->getLGrad());
	GradUtil::compute_arctan_grad(xh, m->getHGrad(), o->getHGrad());
	o->bound();
}

void IntervalGrad::ig_sin(IntervalGrad* m, IntervalGrad* o) {
	float xl = m->getLow();
	float xh = m->getHigh();
	if (xl == xh) {
		o->update(sin(xl), sin(xh));
		GradUtil::compute_sin_grad(xl, m->getLGrad(), o->getLGrad());
		GradUtil::compute_sin_grad(xh, m->getHGrad(), o->getHGrad());
	} else if (xl >= -GradUtil::PI/2 && xl <= GradUtil::PI/2 && xh >= -GradUtil::PI/2 && xh <= GradUtil::PI/2) {
		o->update(sin(xl), sin(xh));
		GradUtil::compute_sin_grad(xl, m->getLGrad(), o->getLGrad());
		GradUtil::compute_sin_grad(xh, m->getHGrad(), o->getHGrad());
	} else if (xl >= -GradUtil::PI && xl <= -GradUtil::PI/2 && xh >= -GradUtil::PI && xh <= -GradUtil::PI/2) {
		o->update(sin(xh), sin(xl));
		GradUtil::compute_sin_grad(xh, m->getHGrad(), o->getLGrad());
		GradUtil::compute_sin_grad(xl, m->getLGrad(), o->getHGrad());
	} else if (xl >= GradUtil::PI/2 && xl <= GradUtil::PI && xh >= GradUtil::PI/2 && xh <= GradUtil::PI) {
		o->update(sin(xh), sin(xl));
		GradUtil::compute_sin_grad(xh, m->getHGrad(), o->getLGrad());
		GradUtil::compute_sin_grad(xl, m->getLGrad(), o->getHGrad());
	} else if (xl >= -GradUtil::PI && xl <= GradUtil::PI/2 && xh >= -GradUtil::PI && xh <= GradUtil::PI/2) {
		vector<float> vals;
		vector<gsl_vector*> grads;
		vals.push_back(sin(xl));
		vals.push_back(sin(xh));
		GradUtil::compute_sin_grad(xl, m->getLGrad(), GradUtil::tmp);
		GradUtil::compute_sin_grad(xh, m->getHGrad(), GradUtil::tmp1);
		grads.push_back(GradUtil::tmp);
		grads.push_back(GradUtil::tmp1);
		float hval = GradUtil::findMax(vals, grads, o->getHGrad());
		GradUtil::default_grad(o->getLGrad());
		o->update(-1.0, hval);
	} else if (xl >= - GradUtil::PI/2 && xl <= GradUtil::PI && xh >= - GradUtil::PI/2 && xh <= GradUtil::PI) {
		vector<float> vals;
		vector<gsl_vector*> grads;
		vals.push_back(sin(xl));
		vals.push_back(sin(xh));
		GradUtil::compute_sin_grad(xl, m->getLGrad(), GradUtil::tmp);
		GradUtil::compute_sin_grad(xh, m->getHGrad(), GradUtil::tmp1);
		grads.push_back(GradUtil::tmp);
		grads.push_back(GradUtil::tmp1);
		float lval = GradUtil::findMin(vals, grads, o->getLGrad());
		GradUtil::default_grad(o->getHGrad());
		o->update(lval, 1.0);
	} else { // TODO: this is so course grained
		o->update(-1.0, 1.0);
		GradUtil::default_grad(o->getLGrad());
		GradUtil::default_grad(o->getHGrad());
	}
	o->singleton = m->singleton;
	o->bound();
}

void IntervalGrad::ig_cos(IntervalGrad* m, IntervalGrad* o) {
	float xl = m->getLow();
	float xh = m->getHigh();
	if (xl == xh) {
		o->update(cos(xl), cos(xh));
		GradUtil::compute_cos_grad(xl, m->getLGrad(), o->getLGrad());
		GradUtil::compute_cos_grad(xh, m->getHGrad(), o->getHGrad());
	} else if (xl >= 0 && xl <= GradUtil::PI && xh >= 0 && xh <= GradUtil::PI) {
		o->update(cos(xh), cos(xl));
		GradUtil::compute_cos_grad(xh, m->getHGrad(), o->getLGrad());
		GradUtil::compute_cos_grad(xl, m->getLGrad(), o->getHGrad());
	} else if (xl <= 0 && xl >= -GradUtil::PI && xh <= 0 && xh >= -GradUtil::PI) {
		o->update(cos(xl), cos(xh));
		GradUtil::compute_cos_grad(xl, m->getLGrad(), o->getLGrad());
		GradUtil::compute_cos_grad(xh, m->getHGrad(), o->getHGrad());
	} else if (xl >= - GradUtil::PI && xl <= GradUtil::PI && xh >= - GradUtil::PI && xh <= GradUtil::PI) {
		vector<float> vals;
		vector<gsl_vector*> grads;
		vals.push_back(cos(xl));
		vals.push_back(cos(xh));
		GradUtil::compute_cos_grad(xl, m->getLGrad(), GradUtil::tmp);
		GradUtil::compute_cos_grad(xh, m->getHGrad(), GradUtil::tmp1);
		grads.push_back(GradUtil::tmp);
		grads.push_back(GradUtil::tmp1);
		float lval = GradUtil::findMin(vals, grads, o->getLGrad());
		GradUtil::default_grad(o->getHGrad());
		o->update(lval, 1.0);
	} else {
		o->update(-1.0, 1.0);
		GradUtil::default_grad(o->getLGrad());
		GradUtil::default_grad(o->getHGrad());
	}
	o->singleton = m->singleton;
	o->bound();
}

void IntervalGrad::ig_tan(IntervalGrad* m, IntervalGrad* o) {
	float xl = m->getLow();
	float xh = m->getHigh();
	Assert(xl == xh, "NYI: range computation for tan");
	o->update(tan(xl), tan(xh));
	GradUtil::compute_tan_grad(xl, m->getLGrad(), o->getLGrad());
	GradUtil::compute_tan_grad(xh, m->getHGrad(), o->getHGrad());
	o->singleton = m->singleton;
	o->bound();
}

void IntervalGrad::ig_exp(IntervalGrad* m, IntervalGrad* o) {
	float xl = m->getLow();
	float xh = m->getHigh();
	o->update(exp(xl), exp(xh));
	GradUtil::compute_exp_grad(xl, m->getLGrad(), o->getLGrad());
	GradUtil::compute_exp_grad(xh, m->getHGrad(), o->getHGrad());
	o->singleton = m->singleton;
	o->bound();
}

void IntervalGrad::ig_sqrt(IntervalGrad* m, IntervalGrad* o) {
	float xl = m->getLow();
	float xh = m->getHigh();
	if (xh < 0.0) { // Ideally, this should raise error
		o->update(0.0, 0.0);
		o->singleton = m->singleton;
		GradUtil::default_grad(o->getLGrad());
		GradUtil::default_grad(o->getHGrad());
		return;
	}
	Assert(xh >= 0.0, "Invalid range");
	if (xl < 0.0) {
		o->update(0.0, sqrt(xh));
		GradUtil::default_grad(o->getLGrad());
		GradUtil::compute_sqrt_grad(xh, m->getHGrad(), o->getHGrad());
	} else {
		o->update(sqrt(xl), sqrt(xh));
		GradUtil::compute_sqrt_grad(xl, m->getLGrad(), o->getLGrad());
		GradUtil::compute_sqrt_grad(xh, m->getHGrad(), o->getHGrad());
	}
	o->singleton = m->singleton;
	o->bound();
}

void IntervalGrad::ig_cast_int_float(IntervalGrad* m, IntervalGrad* o) {
	ig_copy(m, o);
}

// Copy i1 into i2
void IntervalGrad::ig_copy(IntervalGrad* i1, IntervalGrad* i2) {
	i2->update(i1->getLow(), i1->getHigh());
	gsl_blas_dcopy(i1->getLGrad(), i2->getLGrad());
	gsl_blas_dcopy(i1->getHGrad(), i2->getHGrad());
	i2->singleton = i1->singleton;
}
