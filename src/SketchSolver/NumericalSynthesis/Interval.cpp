#include "Interval.h"

void Interval::i_plus(Interval* m, Interval* f, Interval* o) {
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
}

void Interval::i_minus(Interval* m, Interval* f, Interval* o) {
	float mlval = m->getLow();
	float mhval = m->getHigh();
	float flval = f->getLow();
	float fhval = f->getHigh();
	
	float lval = mlval - fhval;
	float hval = mhval - flval;
	if (!isfinite(lval)) {
		lval = lval > 0 ? MAXVAL : MINVAL;
	}
	if (!isfinite(hval)) {
		hval = hval > 0 ? MAXVAL : MINVAL;
	}
	o->update(lval, hval);
}

void Interval::i_times(Interval* m, Interval* f, Interval* o) {
	float mlval = m->getLow();
	float mhval = m->getHigh();
	float flval = f->getLow();
	float fhval = f->getHigh();
	
	vector<float> vals;

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
	
	float minv = findMin(vals);
	float maxv = findMax(vals);
	o->update(minv, maxv);
}

void Interval::i_div(Interval* m, Interval* f, Interval* o) {
	float mlval = m->getLow();
	float mhval = m->getHigh();
	float flval = f->getLow();
	float fhval = f->getHigh();
	
	// Deal with the case when zero can be in the denominator
	if (flval <= 0 && fhval >= 0) { // Zero in the denomintor
		if ((mlval < 0 && mhval > 0) || (flval < 0 && fhval > 0)) {
			o->update(MINVAL, MAXVAL);
		} else if (flval == 0 && fhval == 0) {
			if (mlval >= 0) {
				o->update(MAXVAL, MAXVAL);
			} else if (mhval <= 0) {
				o->update(MINVAL, MINVAL);
			}
			// the case mlval < 0 and mhval > 0 is already handled.
		} else if (mlval >= 0 && flval == 0) {
			float minv = mlval/fhval;
			o->update(minv, MAXVAL);
		} else if (mhval <= 0 && flval == 0) {
			float maxv = mhval/fhval;
			o->update(MINVAL, maxv);
		} else if (mlval >= 0 && fhval == 0) {
			float maxv = mlval/flval;
			o->update(MINVAL, maxv);
		} else if (mhval <= 0 && fhval == 0) {
			float minv = mhval/flval;
			o->update(minv, MAXVAL);
		} else {
			Assert(false, "NYI: unaccounted case");
		}
		return;
	}
	
	vector<float> vals;

	float v1 = mlval / flval;
	float v2 = mlval / fhval;
	float v3 = mhval / flval;
	float v4 = mhval / fhval;
	
	vals.push_back(v1);
	vals.push_back(v2);
	vals.push_back(v3);
	vals.push_back(v4);
	
	float minv = findMin(vals);
	float maxv = findMax(vals);
	o->update(minv, maxv);
}

void Interval::i_neg(Interval* m, Interval* o) {
	o->update(- m->getHigh(), - m->getLow());
}

void Interval::i_union(const vector<Interval*>& m, Interval* o) {
	vector<float> lvals;
	vector<float> hvals;
	for (int i = 0; i < m.size(); i++) {
		lvals.push_back(m[i]->getLow());
		hvals.push_back(m[i]->getHigh());
	}
	float minv = findMin(lvals);
	float maxv = findMax(hvals);
	o->update(minv, maxv);
}

void Interval::i_intersect(const vector<Interval*>& m, Interval* o) {
	vector<float> lvals;
	vector<float> hvals;
	for (int i = 0; i < m.size(); i++) {
		lvals.push_back(m[i]->getLow());
		hvals.push_back(m[i]->getHigh());
	}
	float minv = findMax(lvals);
	float maxv = findMin(hvals);
	if (minv > maxv) {
		o->makeEmpty();
	} else {
		o->update(minv, maxv);
	}
}

void Interval::i_equal(Interval* m, Interval* f, Interval* o) {
	float m1 = m->getLow();
	float m2 = m->getHigh();
	float f1 = f->getLow();
	float f2 = f->getHigh();
	
	if (m2 < f1) {
		o->update(0, 0);
	} else if (m1 > f2) {
		o->update(0, 0);
	} else if (m1 == m2 && m2 == f1 && f1 == f2) {
		o->update(1, 1);
	} else {
		// can be anything
		o->update(0, 1);
	}
}

void Interval::i_lt(Interval* m, Interval* f, Interval* o) {
	float m1 = m->getLow();
	float m2 = m->getHigh();
	float f1 = f->getLow();
	float f2 = f->getHigh();
	
	if (m1 >= f2) {
		o->update(0, 0);
	} else if (m2 < f1) {
		o->update(1, 1);
	} else {
		// can be anything
		o->update(0, 1);
	}
}

void Interval::i_square(Interval* m, Interval* o) {
	vector<float> vals;
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
	
	if (mlval < 0 && mhval > 0) {
		float minv = 0.0;
		float maxv = findMax(vals);
		o->update(minv, maxv);
	} else {
		float minv = findMin(vals);
		float maxv = findMax(vals);
		o->update(minv, maxv);
	}
}

void Interval::i_arctan(Interval* m, Interval* o) {
	float xl = m->getLow();
	float xh = m->getHigh();
	Assert(xl == xh, "NYI: range computation for arctan");
	o->update(atan(xl), atan(xh));
}

void Interval::i_sin(Interval* m, Interval* o) {
	float xl = m->getLow();
	float xh = m->getHigh();
	if (xl == xh) {
		o->update(sin(xl), sin(xh));
	} else if (xl >= -PI/2 && xl <= PI/2 && xh >= -PI/2 && xh <= PI/2) {
		o->update(sin(xl), sin(xh));
	}else { // TODO: this is so course grained
		o->update(-1.0, 1.0);
	}
}

void Interval::i_cos(Interval* m, Interval* o) {
	float xl = m->getLow();
	float xh = m->getHigh();
	if (xl == xh) {
		o->update(cos(xl), cos(xh));
	} else if (xl >= 0 && xl <= PI && xh >= 0 && xh <= PI) {
		o->update(cos(xh), cos(xl));
	} else if (xl <= 0 && xl >= -PI && xh <= 0 && xh >= -PI) {
		o->update(cos(xl), cos(xh));
	} else {
		o->update(-1.0, 1.0);
	}
}

void Interval::i_tan(Interval* m, Interval* o) {
	float xl = m->getLow();
	float xh = m->getHigh();
	Assert(xl == xh, "NYI: range computation for tan");
	o->update(tan(xl), tan(xh));
}

void Interval::i_sqrt(Interval* m, Interval* o) {
	float xl = m->getLow();
	float xh = m->getHigh();
	Assert(xh >= 0.0, "Invalid range");
	if (xl < 0.0) {
		o->update(0.0, sqrt(xh));
	} else {
		o->update(sqrt(xl), sqrt(xh));
	}
}

void Interval::i_cast_int_float(Interval* m, Interval* o) {
	i_copy(m, o);
}

// Copy i1 into i2
void Interval::i_copy(Interval* i1, Interval* i2) {
	i2->update(i1->getLow(), i1->getHigh());
}

float Interval::findMin(const vector<float>& vals) {
	float minv = MAXVAL;
	for (int i = 0; i < vals.size(); i++) {
		if (vals[i] < minv) {
			minv = vals[i];
		}
	}
	return minv;
}

float Interval::findMax(const vector<float>& vals) {
	float maxv = MINVAL;
	for (int i = 0; i < vals.size(); i++) {
		if (vals[i] > maxv) {
			maxv = vals[i];
		}
	}
	return maxv;
}
