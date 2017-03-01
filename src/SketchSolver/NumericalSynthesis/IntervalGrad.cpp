#include "IntervalGrad.h"

gsl_vector* IntervalGrad::tmp;

void IntervalGrad::ig_plus(IntervalGrad* m, IntervalGrad* f, IntervalGrad* o) {
	float mlval = m->getLow();
	float mhval = m->getHigh();
	float flval = f->getLow();
	float fhval = f->getHigh();
	float lval = mlval + flval;
	float hval = mhval + fhval;
	if (!isfinite(lval)) {
		lval = lval > 0 ? MAXVAL : MINVAL;
	}
	if (!isfinite(hval)) {
		hval = hval > 0 ? MAXVAL : MINVAL;
	}
	o->update(lval, hval);
	
	gsl_vector* mlgrads = m->getLGrad();
	gsl_vector* flgrads = f->getLGrad();
	gsl_vector* mhgrads = m->getHGrad();
	gsl_vector* fhgrads = f->getHGrad();
	gsl_vector* l = o->getLGrad();
	gsl_vector_memcpy(l, mlgrads);
	gsl_vector_add(l, flgrads);
	gsl_vector* h = o->getHGrad();
	gsl_vector_memcpy(h, mhgrads);
	gsl_vector_add(h, fhgrads);
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
		
	if (!isfinite(v1)) {
		v1 = v1 > 0 ? MAXVAL : MINVAL;
	}
	if (!isfinite(v2)) {
		v2 = v2 > 0 ? MAXVAL : MINVAL;
	}
	if (!isfinite(v3)) {
		v3 = v3 > 0 ? MAXVAL : MINVAL;
	}
	if (!isfinite(v4)) {
		v4 = v4 > 0 ? MAXVAL : MINVAL;
	}
	//cout << v1 << " " << v2 << " " << v3 << " " << v4 << endl;
		
	vals.push_back(v1);
	vals.push_back(v2);
	vals.push_back(v3);
	vals.push_back(v4);
	
	grads.push_back(compute_mult_grad(mlval, flval, mlgrads, flgrads));
	grads.push_back(compute_mult_grad(mlval, fhval, mlgrads, fhgrads));
	grads.push_back(compute_mult_grad(mhval, flval, mhgrads, flgrads));
	grads.push_back(compute_mult_grad(mhval, fhval, mhgrads, fhgrads));
	
	float minv = findMin(vals, grads, o->getLGrad());
	float maxv = findMax(vals, grads, o->getHGrad());
	o->update(minv, maxv);
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
			o->update(MINVAL, MAXVAL);
			default_grad(l); // TODO: what should be the gradients in this case?
			default_grad(h);
		} else if (flval == 0 && fhval == 0) {
			if (mlval >= 0) {
				o->update(MAXVAL, MAXVAL);
			} else if (mhval <= 0) {
				o->update(MINVAL, MINVAL);
			}
			// the case mlval < 0 and mhval > 0 is already handled.
			default_grad(l);
			default_grad(h);
		} else if (mlval >= 0 && flval == 0) {
			float minv = mlval/fhval;
			o->update(minv, MAXVAL);
			compute_div_grad(mlval, fhval, mlgrads, fhgrads, l);
			default_grad(h);
		} else if (mhval <= 0 && flval == 0) {
			float maxv = mhval/fhval;
			o->update(MINVAL, maxv);
			default_grad(l);
			compute_div_grad(mhval, fhval, mhgrads, fhgrads, h);
		} else if (mlval >= 0 && fhval == 0) {
			float maxv = mlval/flval;
			o->update(MINVAL, maxv);
			default_grad(l);
			compute_div_grad(mlval, flval, flgrads, mlgrads, h);
		} else if (mhval <= 0 && fhval == 0) {
			float minv = mhval/flval;
			o->update(minv, MAXVAL);
			compute_div_grad(mhval, flval, mhgrads, flgrads, l);
			default_grad(h);
		} else {
			Assert(false, "NYI: unaccounted case");
		}
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
	
	grads.push_back(compute_div_grad(mlval, flval, mlgrads, flgrads));
	grads.push_back(compute_div_grad(mlval, fhval, mlgrads, fhgrads));
	grads.push_back(compute_div_grad(mhval, flval, mhgrads, flgrads));
	grads.push_back(compute_div_grad(mhval, fhval, mhgrads, fhgrads));
	
	float minv = findMin(vals, grads, l);
	float maxv = findMax(vals, grads, h);
	o->update(minv, maxv);
}

void IntervalGrad::ig_neg(IntervalGrad* m, IntervalGrad* o) {
	o->update(- m->getHigh(), - m->getLow());
	gsl_vector* l = o->getLGrad();
	gsl_vector* h = o->getHGrad();
	gsl_vector_memcpy(l, m->getHGrad());
	gsl_vector_scale(l, -1.0);
	gsl_vector_memcpy(h, m->getLGrad());
	gsl_vector_scale(h, -1.0);
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
	float minv = findMin(lvals, lgrads, o->getLGrad());
	float maxv = findMax(hvals, hgrads, o->getHGrad());
	o->update(minv, maxv);
}

void IntervalGrad::ig_equal(IntervalGrad* m, IntervalGrad* f, IntervalGrad* o) {
	float m1 = m->getLow();
	float m2 = m->getHigh();
	float f1 = f->getLow();
	float f2 = f->getHigh();

	if (m2 < f1) {
		o->update(0, 0); // TODO: What should be the gradient in this case?
	} else if (m1 > f2) {
		o->update(0, 0);
	} else if (m1 == m2 && m2 == f1 && f1 == f2) {
		o->update(1, 1);
	} else {
		// can be anything
		o->update(0, 1);
	}
}

void IntervalGrad::ig_lt(IntervalGrad* m, IntervalGrad* f, IntervalGrad* o) {
	float m1 = m->getLow();
	float m2 = m->getHigh();
	float f1 = f->getLow();
	float f2 = f->getHigh();

	if (m1 >= f2) { // TODO: What should be the gradient in this case?
		o->update(0, 0);
	} else if (m2 < f1) {
		o->update(1, 1);
	} else {
		// can be anything
		o->update(0, 1);
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
	
	if (!isfinite(v1)) {
		v1 = v1 > 0 ? MAXVAL : MINVAL;
	}
	if (!isfinite(v2)) {
		v2 = v2 > 0 ? MAXVAL : MINVAL;
	}
	
	vals.push_back(v1);
	vals.push_back(v2);
	
	grads.push_back(compute_square_grad(mlval, mlgrads));
	grads.push_back(compute_square_grad(mhval, mhgrads));
	
	if (mlval < 0 && mhval > 0) {
		float minv = 0.0;
		default_grad(o->getLGrad()); // TODO: what should be the gradient in this case?
		float maxv = findMax(vals, grads, o->getHGrad());
		o->update(minv, maxv);
	} else {
		float minv = findMin(vals, grads, o->getLGrad());
		float maxv = findMax(vals, grads, o->getHGrad());
		o->update(minv, maxv);
	}
}

void IntervalGrad::ig_arctan(IntervalGrad* m, IntervalGrad* o) {
	float xl = m->getLow();
	float xh = m->getHigh();
	Assert(xl == xh, "NYI: range computation for arctan");
	o->update(atan(xl), atan(xh));
	compute_arctan_grad(xl, m->getLGrad(), o->getLGrad());
	compute_arctan_grad(xh, m->getHGrad(), o->getHGrad());
}

void IntervalGrad::ig_sin(IntervalGrad* m, IntervalGrad* o) {
	float xl = m->getLow();
	float xh = m->getHigh();
	if (xl == xh) {
		o->update(sin(xl), sin(xh));
		compute_sin_grad(xl, m->getLGrad(), o->getLGrad());
		compute_sin_grad(xh, m->getHGrad(), o->getHGrad());
	} else if (xl >= -PI/2 && xl <= PI/2 && xh >= -PI/2 && xh <= PI/2) {
		o->update(sin(xl), sin(xh));
		compute_sin_grad(xl, m->getLGrad(), o->getLGrad());
		compute_sin_grad(xh, m->getHGrad(), o->getHGrad());
	}else { // TODO: this is so course grained
		o->update(-1.0, 1.0);
		default_grad(o->getLGrad());
		default_grad(o->getHGrad());
	}
}

void IntervalGrad::ig_cos(IntervalGrad* m, IntervalGrad* o) {
	float xl = m->getLow();
	float xh = m->getHigh();
	if (xl == xh) {
		o->update(cos(xl), cos(xh));
		compute_cos_grad(xl, m->getLGrad(), o->getLGrad());
		compute_cos_grad(xh, m->getHGrad(), o->getHGrad());
	} else if (xl >= 0 && xl <= PI && xh >= 0 && xh <= PI) {
		o->update(cos(xh), cos(xl));
		compute_cos_grad(xh, m->getHGrad(), o->getLGrad());
		compute_cos_grad(xl, m->getLGrad(), o->getHGrad());
	} else if (xl <= 0 && xl >= -PI && xh <= 0 && xh >= -PI) {
		o->update(cos(xl), cos(xh));
		compute_cos_grad(xl, m->getLGrad(), o->getLGrad());
		compute_cos_grad(xh, m->getHGrad(), o->getHGrad());
	} else {
		o->update(-1.0, 1.0);
		default_grad(o->getLGrad());
		default_grad(o->getHGrad());
	}
}

void IntervalGrad::ig_tan(IntervalGrad* m, IntervalGrad* o) {
	float xl = m->getLow();
	float xh = m->getHigh();
	Assert(xl == xh, "NYI: range computation for tan");
	o->update(tan(xl), tan(xh));
	compute_tan_grad(xl, m->getLGrad(), o->getLGrad());
	compute_tan_grad(xh, m->getHGrad(), o->getHGrad());
}

void IntervalGrad::ig_sqrt(IntervalGrad* m, IntervalGrad* o) {
	float xl = m->getLow();
	float xh = m->getHigh();
	if (xh < 0.0) { // Ideally, this should raise error
		o->update(0.0, 0.0);
		default_grad(o->getLGrad());
		default_grad(o->getHGrad());
		return;
	}
	Assert(xh >= 0.0, "Invalid range");
	if (xl < 0.0) {
		o->update(0.0, sqrt(xh));
		default_grad(o->getLGrad());
		compute_sqrt_grad(xh, m->getHGrad(), o->getHGrad());
	} else {
		o->update(sqrt(xl), sqrt(xh));
		compute_sqrt_grad(xl, m->getLGrad(), o->getLGrad());
		compute_sqrt_grad(xh, m->getHGrad(), o->getHGrad());
	}
}

void IntervalGrad::ig_cast_int_float(IntervalGrad* m, IntervalGrad* o) {
	ig_copy(m, o);
}

// Copy i1 into i2
void IntervalGrad::ig_copy(IntervalGrad* i1, IntervalGrad* i2) {
	i2->update(i1->getLow(), i1->getHigh());
	gsl_vector_memcpy(i2->getLGrad(), i1->getLGrad());
	gsl_vector_memcpy(i2->getHGrad(), i1->getHGrad());
}

float IntervalGrad::findMin(const vector<float>& vals, const vector<gsl_vector*>& grads, gsl_vector* l) {
	float minv = MAXVAL;
	for (int i = 0; i < vals.size(); i++) {
		if (vals[i] < minv) {
			minv = vals[i];
		}
	}
	softMinMax(vals, grads, l, -1.0, minv);
	return minv;
}

float IntervalGrad::findMax(const vector<float>& vals, const vector<gsl_vector*>& grads, gsl_vector* h) {
	float maxv = MINVAL;
	for (int i = 0; i < vals.size(); i++) {
		if (vals[i] > maxv) {
			maxv = vals[i];
		}
	}
	softMinMax(vals, grads, h, 1.0, maxv);
	return maxv;
}

/* Computes the gradient of log(e^(alpha*(v1 - t)) + e^(alpha*(v2 - t)) ... )
 If alpha > 0, the result is the soft max of the values
 If alpha < 0, the result is the soft min of the values */
void IntervalGrad::softMinMax(const vector<float>& vals, const vector<gsl_vector*>& grads, gsl_vector* l, float alpha, float t) {
	float expval = 0.0;
	for (int i = 0; i < vals.size(); i++) {
		expval += exp(alpha * (vals[i] - t));
	}
	
	for (int i = 0; i < vals.size(); i++) {
		gsl_vector_memcpy(tmp, grads[i]);
		gsl_vector_scale(tmp, exp(alpha*(vals[i] - t)));
		if (i == 0)
			gsl_vector_memcpy(l, tmp);
		else
			gsl_vector_add(l, tmp);
	}
	gsl_vector_scale(l, 1.0/expval);
}

void IntervalGrad::default_grad(gsl_vector* out) {
	for (int i = 0; i < out->size; i++) {
		gsl_vector_set(out, i, 0.0);
	}
}

// computes the grad of mval * fval i.e. mval*fgrads + fval*mgrads
void IntervalGrad::compute_mult_grad(float mval, float fval, gsl_vector* mgrads, gsl_vector* fgrads, gsl_vector* out) {
	gsl_vector_memcpy(out, mgrads);
	gsl_vector_scale(out, fval);
	gsl_vector_memcpy(tmp, fgrads);
	gsl_vector_scale(tmp, mval);
	gsl_vector_add(out, tmp);
}

gsl_vector* IntervalGrad::compute_mult_grad(float mval, float fval, gsl_vector* mgrads, gsl_vector* fgrads) {
	gsl_vector* out = gsl_vector_alloc(mgrads->size);
	compute_mult_grad(mval, fval, mgrads, fgrads, out);
	return out;
}

// computes the grad of mval / fval i.e (fval * mgrads - mval * fgrads) / (fval * fval)
void IntervalGrad::compute_div_grad(float mval, float fval, gsl_vector* mgrads, gsl_vector* fgrads, gsl_vector* out) {
	gsl_vector_memcpy(out, mgrads);
	gsl_vector_scale(out, fval);
	gsl_vector_memcpy(tmp, fgrads);
	gsl_vector_scale(tmp, mval);
	gsl_vector_sub(out, tmp);
	gsl_vector_scale(out, 1.0/(fval*fval));
}

gsl_vector* IntervalGrad::compute_div_grad(float mval, float fval, gsl_vector* mgrads, gsl_vector* fgrads) {
	gsl_vector* out = gsl_vector_alloc(mgrads->size);
	compute_div_grad(mval, fval, mgrads, fgrads, out);
	return out;
}

void IntervalGrad::compute_square_grad(float mval, gsl_vector* mgrads, gsl_vector* out) {
	float dm = 2.0 * mval;
	gsl_vector_memcpy(out, mgrads);
	gsl_vector_scale(out, dm);
}

gsl_vector* IntervalGrad::compute_square_grad(float mval, gsl_vector* mgrads) {
	gsl_vector* out = gsl_vector_alloc(mgrads->size);
	compute_square_grad(mval, mgrads, out);
	return out;
}

void IntervalGrad::compute_arctan_grad(float mval, gsl_vector* mgrads, gsl_vector* out) {
	float dm = 1.0/(mval * mval + 1.0);
	gsl_vector_memcpy(out, mgrads);
	gsl_vector_scale(out, dm);
}

void IntervalGrad::compute_sin_grad(float mval, gsl_vector* mgrads, gsl_vector* out) {
	float dm = cos(mval);
	gsl_vector_memcpy(out, mgrads);
	gsl_vector_scale(out, dm);
}

void IntervalGrad::compute_cos_grad(float mval, gsl_vector* mgrads, gsl_vector* out) {
	float dm = -sin(mval);
	gsl_vector_memcpy(out, mgrads);
	gsl_vector_scale(out, dm);
}

void IntervalGrad::compute_tan_grad(float mval, gsl_vector* mgrads, gsl_vector* out) {
	float dm = 1.0/(cos(mval)*cos(mval));
	gsl_vector_memcpy(out, mgrads);
	gsl_vector_scale(out, dm);
}

void IntervalGrad::compute_sqrt_grad(float mval, gsl_vector* mgrads, gsl_vector* out) {
	float dm = 0.5/sqrt(mval);
	gsl_vector_memcpy(out, mgrads);
	gsl_vector_scale(out, dm);
}
