#include "IntervalGrad.h"

gsl_vector* IntervalGrad::tmp;
gsl_vector* IntervalGrad::tmp1;
gsl_vector* IntervalGrad::tmp2;
gsl_vector* IntervalGrad::tmp3;

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
	gsl_vector* l = o->getLGrad();
	gsl_vector_memcpy(l, mlgrads);
	gsl_vector_add(l, flgrads);
	gsl_vector* h = o->getHGrad();
	gsl_vector_memcpy(h, mhgrads);
	gsl_vector_add(h, fhgrads);
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
	
	grads.push_back(compute_mult_grad(mlval, flval, mlgrads, flgrads));
	grads.push_back(compute_mult_grad(mlval, fhval, mlgrads, fhgrads));
	grads.push_back(compute_mult_grad(mhval, flval, mhgrads, flgrads));
	grads.push_back(compute_mult_grad(mhval, fhval, mhgrads, fhgrads));
	
	if (m->singleton && f->singleton) {
		float minv = vals[0];
		float maxv = vals[0];
		gsl_vector_memcpy(o->getLGrad(), grads[0]);
		gsl_vector_memcpy(o->getHGrad(), grads[0]);
		o->update(minv, maxv);
		o->singleton = true;
	} else {
		float minv = findMin(vals, grads, o->getLGrad()); // Note: this computes a smooth approximate of min and not the actual value
		float maxv = findMax(vals, grads, o->getHGrad());
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
	
	grads.push_back(compute_div_grad(mlval, flval, mlgrads, flgrads));
	grads.push_back(compute_div_grad(mlval, fhval, mlgrads, fhgrads));
	grads.push_back(compute_div_grad(mhval, flval, mhgrads, flgrads));
	grads.push_back(compute_div_grad(mhval, fhval, mhgrads, fhgrads));
	
	
	if (m->singleton && f->singleton) {
		float minv = vals[0];
		float maxv = vals[0];
		gsl_vector_memcpy(l, grads[0]);
		gsl_vector_memcpy(h, grads[0]);
		o->update(minv, maxv);
		o->singleton = true;
	} else {
		float minv = findMin(vals, grads, l); // Note: this computes a smooth approximate of min and not the actual value
		float maxv = findMax(vals, grads, h);
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
	gsl_vector_memcpy(l, m->getHGrad());
	gsl_vector_scale(l, -1.0);
	gsl_vector_memcpy(h, m->getLGrad());
	gsl_vector_scale(h, -1.0);
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
	float minv = findMin(lvals, lgrads, o->getLGrad());
	float maxv = findMax(hvals, hgrads, o->getHGrad());
	o->update(minv, maxv);
	o->singleton = false;
	o->bound();
}

void IntervalGrad::ig_conditionalUnion(IntervalGrad* m, IntervalGrad* f, DistanceGrad* d, IntervalGrad* o) {
	float s1 = IntervalGrad::sigmoid(d->dist - DELTA, d->grad, tmp);
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
	o->bound();
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
	float minv = findMax(lvals, lgrads, o->getLGrad());
	float maxv = findMin(hvals, hgrads, o->getHGrad());
	if (maxv < minv) {
		// empty set
	}
	o->update(minv, maxv);
	o->singleton = false;
	o->bound();
}

IntervalGrad* IntervalGrad::ig_leftExtend(IntervalGrad* m) {
	gsl_vector* lg = gsl_vector_alloc(m->lgrad->size);
	default_grad(lg);
	gsl_vector* hg = gsl_vector_alloc(m->hgrad->size);
	gsl_vector_memcpy(hg, m->hgrad);
	return new IntervalGrad(MINVAL, m->getHigh(), lg, hg);
}

IntervalGrad* IntervalGrad::ig_rightExtend(IntervalGrad* m) {
	gsl_vector* lg = gsl_vector_alloc(m->lgrad->size);
	gsl_vector_memcpy(lg, m->lgrad);
	gsl_vector* hg = gsl_vector_alloc(m->hgrad->size);
	default_grad(hg);
	return new IntervalGrad(m->getLow(), MAXVAL, lg, hg);

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
	o->singleton = m->singleton && f->singleton;
	o->bound();
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
	o->singleton = m->singleton && f->singleton;
	o->bound();
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
	
	grads.push_back(compute_square_grad(mlval, mlgrads));
	grads.push_back(compute_square_grad(mhval, mhgrads));
	
	if (mlval < 0 && mhval > 0) {
		float minv = 0.0;
		default_grad(o->getLGrad()); // TODO: what should be the gradient in this case?
		float maxv = findMax(vals, grads, o->getHGrad());
		o->update(minv, maxv);
		o->singleton = false;
	} else {
		if (m->singleton) {
			o->update(vals[0], vals[0]);
			o->singleton = true;
			gsl_vector_memcpy(o->getLGrad(), grads[0]);
			gsl_vector_memcpy(o->getHGrad(), grads[0]);
		} else {
			float minv = findMin(vals, grads, o->getLGrad());
			float maxv = findMax(vals, grads, o->getHGrad());
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
	compute_arctan_grad(xl, m->getLGrad(), o->getLGrad());
	compute_arctan_grad(xh, m->getHGrad(), o->getHGrad());
	o->bound();
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
	} else if (xl >= -PI && xl <= -PI/2 && xh >= -PI && xh <= -PI/2) {
		o->update(sin(xh), sin(xl));
		compute_sin_grad(xh, m->getHGrad(), o->getLGrad());
		compute_sin_grad(xl, m->getLGrad(), o->getHGrad());
	} else if (xl >= PI/2 && xl <= PI && xh >= PI/2 && xh <= PI) {
		o->update(sin(xh), sin(xl));
		compute_sin_grad(xh, m->getHGrad(), o->getLGrad());
		compute_sin_grad(xl, m->getLGrad(), o->getHGrad());
	} else if (xl >= -PI && xl <= PI/2 && xh >= -PI && xh <= PI/2) {
		vector<float> vals;
		vector<gsl_vector*> grads;
		vals.push_back(sin(xl));
		vals.push_back(sin(xh));
		grads.push_back(compute_sin_grad(xl, m->getLGrad()));
		grads.push_back(compute_sin_grad(xh, m->getHGrad()));
		float hval = findMax(vals, grads, o->getHGrad());
		default_grad(o->getLGrad());
		o->update(-1.0, hval);
	} else if (xl >= - PI/2 && xl <= PI && xh >= - PI/2 && xh <= PI) {
		vector<float> vals;
		vector<gsl_vector*> grads;
		vals.push_back(sin(xl));
		vals.push_back(sin(xh));
		grads.push_back(compute_sin_grad(xl, m->getLGrad()));
		grads.push_back(compute_sin_grad(xh, m->getHGrad()));
		float lval = findMin(vals, grads, o->getLGrad());
		default_grad(o->getHGrad());
		o->update(lval, 1.0);
	} else { // TODO: this is so course grained
		o->update(-1.0, 1.0);
		default_grad(o->getLGrad());
		default_grad(o->getHGrad());
	}
	o->singleton = m->singleton;
	o->bound();
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
	} else if (xl >= - PI && xl <= PI && xh >= - PI && xh <= PI) {
		vector<float> vals;
		vector<gsl_vector*> grads;
		vals.push_back(cos(xl));
		vals.push_back(cos(xh));
		grads.push_back(compute_cos_grad(xl, m->getLGrad()));
		grads.push_back(compute_cos_grad(xh, m->getHGrad()));
		float lval = findMin(vals, grads, o->getLGrad());
		default_grad(o->getHGrad());
		o->update(lval, 1.0);
	} else {
		o->update(-1.0, 1.0);
		default_grad(o->getLGrad());
		default_grad(o->getHGrad());
	}
	o->singleton = m->singleton;
	o->bound();
}

void IntervalGrad::ig_tan(IntervalGrad* m, IntervalGrad* o) {
	float xl = m->getLow();
	float xh = m->getHigh();
	Assert(xl == xh, "NYI: range computation for tan");
	o->update(tan(xl), tan(xh));
	compute_tan_grad(xl, m->getLGrad(), o->getLGrad());
	compute_tan_grad(xh, m->getHGrad(), o->getHGrad());
	o->singleton = m->singleton;
	o->bound();
}

void IntervalGrad::ig_sqrt(IntervalGrad* m, IntervalGrad* o) {
	float xl = m->getLow();
	float xh = m->getHigh();
	if (xh < 0.0) { // Ideally, this should raise error
		o->update(0.0, 0.0);
		o->singleton = m->singleton;
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
	o->singleton = m->singleton;
	o->bound();
}

void IntervalGrad::ig_cast_int_float(IntervalGrad* m, IntervalGrad* o) {
	ig_copy(m, o);
}

// Copy i1 into i2
void IntervalGrad::ig_copy(IntervalGrad* i1, IntervalGrad* i2) {
	i2->update(i1->getLow(), i1->getHigh());
	gsl_vector_memcpy(i2->getLGrad(), i1->getLGrad());
	gsl_vector_memcpy(i2->getHGrad(), i1->getHGrad());
	i2->singleton = i1->singleton;
}

float IntervalGrad::findMin(float val1, float val2, gsl_vector* grad1, gsl_vector* grad2, gsl_vector* l) {
	vector<float> vals;
	vector<gsl_vector*> grads;
	vals.push_back(val1);
	vals.push_back(val2);
	grads.push_back(grad1);
	grads.push_back(grad2);
	
	return findMin(vals, grads, l);
}

float IntervalGrad::findMin(const vector<float>& vals, const vector<gsl_vector*>& grads, gsl_vector* l) {
	float minv = MAXVAL;
	for (int i = 0; i < vals.size(); i++) {
		if (vals[i] < minv) {
			minv = vals[i];
		}
	}
	return softMinMax(vals, grads, l, -ALPHA, minv);
}

float IntervalGrad::findMax(float val1, float val2, gsl_vector* grad1, gsl_vector* grad2, gsl_vector* h) {
	vector<float> vals;
	vector<gsl_vector*> grads;
	vals.push_back(val1);
	vals.push_back(val2);
	grads.push_back(grad1);
	grads.push_back(grad2);
	
	return findMax(vals, grads, h);
}

float IntervalGrad::findMax(const vector<float>& vals, const vector<gsl_vector*>& grads, gsl_vector* h) {
	float maxv = MINVAL;
	for (int i = 0; i < vals.size(); i++) {
		if (vals[i] > maxv) {
			maxv = vals[i];
		}
	}
	return softMinMax(vals, grads, h, ALPHA, maxv);
}

float IntervalGrad::sigmoid(float x, gsl_vector* grads, gsl_vector* out) {
	float v = 1.0/(1.0 + exp(BETA * x));
	float d = BETA*v*(1-v);
	gsl_vector_memcpy(out, grads);
	gsl_vector_scale(out, d);
	return v;
}

/* Computes the gradient of log(e^(alpha*(v1 - t)) + e^(alpha*(v2 - t)) ... )
 If alpha > 0, the result is the soft max of the values
 If alpha < 0, the result is the soft min of the values */
float IntervalGrad::softMinMax(const vector<float>& vals, const vector<gsl_vector*>& grads, gsl_vector* l, float alpha, float t) {
	float expval = 0.0;
	for (int i = 0; i < vals.size(); i++) {
		expval += exp(alpha * (vals[i] - t));
	}
	
	for (int i = 0; i < vals.size(); i++) {
		Assert(grads[i] != tmp3, "Error: tmp3 will be overwritten");
		gsl_vector_memcpy(tmp3, grads[i]);
		gsl_vector_scale(tmp3, exp(alpha*(vals[i] - t)));
		if (i == 0)
			gsl_vector_memcpy(l, tmp3);
		else
			gsl_vector_add(l, tmp3);
	}
	gsl_vector_scale(l, 1.0/expval);
	return (log(expval) + alpha * t)/alpha;
}

void IntervalGrad::default_grad(gsl_vector* out) {
	for (int i = 0; i < out->size; i++) {
		gsl_vector_set(out, i, 0.0);
	}
}

// computes the grad of mval * fval i.e. mval*fgrads + fval*mgrads
void IntervalGrad::compute_mult_grad(float mval, float fval, gsl_vector* mgrads, gsl_vector* fgrads, gsl_vector* out) {
	Assert(mgrads != tmp3 && fgrads != tmp3, "Error: tmp3 will be overwritten");
	gsl_vector_memcpy(out, mgrads);
	gsl_vector_scale(out, fval);
	gsl_vector_memcpy(tmp3, fgrads);
	gsl_vector_scale(tmp3, mval);
	gsl_vector_add(out, tmp3);
}

gsl_vector* IntervalGrad::compute_mult_grad(float mval, float fval, gsl_vector* mgrads, gsl_vector* fgrads) {
	gsl_vector* out = gsl_vector_alloc(mgrads->size);
	compute_mult_grad(mval, fval, mgrads, fgrads, out);
	return out;
}

// computes the grad of mval / fval i.e (fval * mgrads - mval * fgrads) / (fval * fval)
void IntervalGrad::compute_div_grad(float mval, float fval, gsl_vector* mgrads, gsl_vector* fgrads, gsl_vector* out) {
	Assert(mgrads != tmp3 && fgrads != tmp3, "Error: tmp will be overwritten");
	gsl_vector_memcpy(out, mgrads);
	gsl_vector_scale(out, fval);
	gsl_vector_memcpy(tmp3, fgrads);
	gsl_vector_scale(tmp3, mval);
	gsl_vector_sub(out, tmp3);
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

gsl_vector* IntervalGrad::compute_sin_grad(float mval, gsl_vector* mgrads) {
	gsl_vector* out = gsl_vector_alloc(mgrads->size);
	compute_sin_grad(mval, mgrads, out);
	return out;
}

void IntervalGrad::compute_cos_grad(float mval, gsl_vector* mgrads, gsl_vector* out) {
	float dm = -sin(mval);
	gsl_vector_memcpy(out, mgrads);
	gsl_vector_scale(out, dm);
}

gsl_vector* IntervalGrad::compute_cos_grad(float mval, gsl_vector* mgrads) {
	gsl_vector* out = gsl_vector_alloc(mgrads->size);
	compute_cos_grad(mval, mgrads, out);
	return out;
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
