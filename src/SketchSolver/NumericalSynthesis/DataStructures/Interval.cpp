#include "Interval.h"

Interval* EMPTY_INTERVAL = new Interval(Interval::MINVAL, Interval::MINVAL);
Interval* FULL_INTERVAL = new Interval(Interval::MINVAL, Interval::MAXVAL);

Interval* Interval::i_plus(Interval* m, Interval* f) {
	if (m == FULL_INTERVAL || f == FULL_INTERVAL) return FULL_INTERVAL;
	if (m == EMPTY_INTERVAL || f == EMPTY_INTERVAL) return EMPTY_INTERVAL;
	double mlval = m->getLow();
	double mhval = m->getHigh();
	double flval = f->getLow();
	double fhval = f->getHigh();
	
	double lval = mlval + flval;
	double hval = mhval + fhval;
	if (!isfinite(lval)) {
		lval = lval > 0 ? MAXVAL : MINVAL;
	}
	if (!isfinite(hval)) {
		hval = hval > 0 ? MAXVAL : MINVAL;
	}
	return new Interval(lval, hval);
}

Interval* Interval::i_minus(Interval* m, Interval* f) {
	if (m == FULL_INTERVAL || f == FULL_INTERVAL) return FULL_INTERVAL;
	if (m == EMPTY_INTERVAL || f == EMPTY_INTERVAL) return EMPTY_INTERVAL;
	double mlval = m->getLow();
	double mhval = m->getHigh();
	double flval = f->getLow();
	double fhval = f->getHigh();
	
	double lval = mlval - fhval;
	double hval = mhval - flval;
	if (!isfinite(lval)) {
		lval = lval > 0 ? MAXVAL : MINVAL;
	}
	if (!isfinite(hval)) {
		hval = hval > 0 ? MAXVAL : MINVAL;
	}
	return new Interval(lval, hval);
}

Interval* Interval::i_times(Interval* m, Interval* f) {
	if (m == EMPTY_INTERVAL || f == EMPTY_INTERVAL) return EMPTY_INTERVAL;
	if (m == FULL_INTERVAL && f == FULL_INTERVAL) return FULL_INTERVAL;
	if (m == FULL_INTERVAL && f->getLow() == 0 && f->getHigh() == 0) return new Interval(0, 0);
	if (f == FULL_INTERVAL && m->getLow() == 0 && m->getHigh() == 0) return new Interval(0, 0);
	if (m == FULL_INTERVAL || f == FULL_INTERVAL) return FULL_INTERVAL;

	double mlval = m->getLow();
	double mhval = m->getHigh();
	double flval = f->getLow();
	double fhval = f->getHigh();
	
	vector<double> vals;

	double v1 = mlval * flval;
	double v2 = mlval * fhval;
	double v3 = mhval * flval;
	double v4 = mhval * fhval;
	
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
	
	double minv = findMin(vals);
	double maxv = findMax(vals);
	return new Interval(minv, maxv);
}

Interval* Interval::i_div(Interval* m, Interval* f) {
	if (m == FULL_INTERVAL || f == FULL_INTERVAL) return FULL_INTERVAL;
	if (m == EMPTY_INTERVAL || f == EMPTY_INTERVAL) return EMPTY_INTERVAL;

	double mlval = m->getLow();
	double mhval = m->getHigh();
	double flval = f->getLow();
	double fhval = f->getHigh();
	
	// Deal with the case when zero can be in the denominator
	if (flval <= 0 && fhval >= 0) { // Zero in the denomintor
		if ((mlval < 0 && mhval > 0) || (flval < 0 && fhval > 0)) {
			return new Interval(MINVAL, MAXVAL);
		} else if (flval == 0 && fhval == 0) {
			if (mlval >= 0) {
				return new Interval(MAXVAL, MAXVAL);
			} else if (mhval <= 0) {
				return new Interval(MINVAL, MINVAL);
			}
			// the case mlval < 0 and mhval > 0 is already handled.
		} else if (mlval >= 0 && flval == 0) {
			double minv = mlval/fhval;
			return new Interval(minv, MAXVAL);
		} else if (mhval <= 0 && flval == 0) {
			double maxv = mhval/fhval;
			return new Interval(MINVAL, maxv);
		} else if (mlval >= 0 && fhval == 0) {
			double maxv = mlval/flval;
			return new Interval(MINVAL, maxv);
		} else if (mhval <= 0 && fhval == 0) {
			double minv = mhval/flval;
			return new Interval(minv, MAXVAL);
		} else {
			Assert(false, "NYI: unaccounted case");
		}
	}
	
	vector<double> vals;

	double v1 = mlval / flval;
	double v2 = mlval / fhval;
	double v3 = mhval / flval;
	double v4 = mhval / fhval;
	
	vals.push_back(v1);
	vals.push_back(v2);
	vals.push_back(v3);
	vals.push_back(v4);
	
	double minv = findMin(vals);
	double maxv = findMax(vals);
	return new Interval(minv, maxv);
}

Interval* Interval::i_neg(Interval* m) {
	if (m == FULL_INTERVAL) return FULL_INTERVAL;
	if (m == EMPTY_INTERVAL) return EMPTY_INTERVAL;

	return new Interval(- m->getHigh(), - m->getLow());
}

Interval* Interval::i_union(const vector<Interval*>& m) {
	vector<double> lvals;
	vector<double> hvals;
	for (int i = 0; i < m.size(); i++) {
		if (m[i] == FULL_INTERVAL) return FULL_INTERVAL;
		if (m[i] == EMPTY_INTERVAL) continue;
		lvals.push_back(m[i]->getLow());
		hvals.push_back(m[i]->getHigh());
	}
	if (lvals.size() == 0) return EMPTY_INTERVAL;
	double minv = findMin(lvals);
	double maxv = findMax(hvals);
	return new Interval(minv, maxv);
}

Interval* Interval::i_intersect(const vector<Interval*>& m) {
	vector<double> lvals;
	vector<double> hvals;
	for (int i = 0; i < m.size(); i++) {
		if (m[i] == EMPTY_INTERVAL) {
			return EMPTY_INTERVAL;
		}
		if (m[i] == FULL_INTERVAL) {
			continue;
		}
		lvals.push_back(m[i]->getLow());
		hvals.push_back(m[i]->getHigh());
	}
	if (lvals.size() == 0) {
		return FULL_INTERVAL;
	}
	double minv = findMax(lvals);
	double maxv = findMin(hvals);
	if (minv > maxv + PRECISION) {
		return EMPTY_INTERVAL;
	} else {
		return new Interval(minv, maxv);
	}
}

Interval* Interval::i_intersect(Interval* m, Interval* f) {
	vector<Interval*> intervals;
	intervals.push_back(m);
	intervals.push_back(f);
	return i_intersect(intervals);
}

Interval* Interval::i_equal(Interval* m, Interval* f) {
	if (m == FULL_INTERVAL || f == FULL_INTERVAL) return new Interval(0, 1);
	if (m == EMPTY_INTERVAL || f == EMPTY_INTERVAL) return EMPTY_INTERVAL;

	double m1 = m->getLow();
	double m2 = m->getHigh();
	double f1 = f->getLow();
	double f2 = f->getHigh();
	
	if (m2 < f1) {
		return new Interval(0, 0);
	} else if (m1 > f2) {
		return new Interval(0, 0);
	} else if (m1 == m2 && m2 == f1 && f1 == f2) {
		return new Interval(1, 1);
	} else {
		// can be anything
		return new Interval(0, 1);
	}
}

Interval* Interval::i_lt(Interval* m, Interval* f) {
	if (m == FULL_INTERVAL || f == FULL_INTERVAL) return new Interval(0, 1);
	if (m == EMPTY_INTERVAL || f == EMPTY_INTERVAL) return EMPTY_INTERVAL;
	
	double m1 = m->getLow();
	double m2 = m->getHigh();
	double f1 = f->getLow();
	double f2 = f->getHigh();
	
	if (m1 >= f2) {
		return new Interval(0, 0);
	} else if (m2 < f1) {
		return new Interval(1, 1);
	} else {
		// can be anything
		return new Interval(0, 1);
	}
}

Interval* Interval::i_and(Interval* m, Interval* f) {
	if (m == EMPTY_INTERVAL || f == EMPTY_INTERVAL) return EMPTY_INTERVAL;
	if (m != FULL_INTERVAL && m->getLow() == m->getHigh() && m->getLow() == 0) {
		return new Interval(0, 0);
	}
	if (f != FULL_INTERVAL && f->getLow() == f->getHigh() && f->getLow() == 0) {
		return new Interval(0, 0);
	}
	if (m != FULL_INTERVAL && m->getLow() == m->getHigh() && f->getLow() == f->getHigh() && m->getLow() == 1 && f->getLow() == 1) {
		return new Interval(1, 1);
	}
	return new Interval(0, 1);
}

Interval* Interval::i_or(Interval* m, Interval* f) {
	if (m == EMPTY_INTERVAL || f == EMPTY_INTERVAL) return EMPTY_INTERVAL;
	if (m != FULL_INTERVAL && m->getLow() == m->getHigh() && m->getLow() == 1) {
		return new Interval(1, 1);
	}
	if (f != FULL_INTERVAL && f->getLow() == f->getHigh() && f->getLow() == 1) {
		return new Interval(1, 1);
	}
	if (m != FULL_INTERVAL && m->getLow() == m->getHigh() && f->getLow() == f->getHigh() && m->getLow() == 0 && f->getLow() == 0) {
		return new Interval(0, 0);
	}
	return new Interval(0, 1);
}

Interval* Interval::i_not(Interval* m) {
	if (m == EMPTY_INTERVAL) return EMPTY_INTERVAL;
	if (m == FULL_INTERVAL) return new Interval(0, 1);
	if (m->getLow() == m->getHigh()) {
		if (m->getLow() == 0) {
			return new Interval(1, 1);
		} else {
			Assert(m->getLow() == 1, "Something is wrong NOT");
			return new Interval(0, 0);
		}
	}
	return new Interval(0, 1);
}

Interval* Interval::i_square(Interval* m) {
	if (m == FULL_INTERVAL) return new Interval(0, MAXVAL);
	if (m == EMPTY_INTERVAL) return EMPTY_INTERVAL;
	
	vector<double> vals;
	double mlval = m->getLow();
	double mhval = m->getHigh();
	
	double v1 = mlval * mlval;
	double v2 = mhval * mhval;
	
	if (!isfinite(v1)) {
		v1 = v1 > 0 ? MAXVAL : MINVAL;
	}
	if (!isfinite(v2)) {
		v2 = v2 > 0 ? MAXVAL : MINVAL;
	}
	
	vals.push_back(v1);
	vals.push_back(v2);
	
	if (mlval < 0 && mhval > 0) {
		double minv = 0.0;
		double maxv = findMax(vals);
		return new Interval(minv, maxv);
	} else {
		double minv = findMin(vals);
		double maxv = findMax(vals);
		return new Interval(minv, maxv);
	}
}

Interval* Interval::i_arctan(Interval* m) {
	if (m == FULL_INTERVAL) return FULL_INTERVAL;
	if (m == EMPTY_INTERVAL) return EMPTY_INTERVAL;
	
	double xl = m->getLow();
	double xh = m->getHigh();
	Assert(xl == xh, "NYI: range computation for arctan");
	return new Interval(atan(xl), atan(xh));
}

Interval* Interval::i_sin(Interval* m) {
	if (m == FULL_INTERVAL) return new Interval(-1.0, 1.0);
	if (m == EMPTY_INTERVAL) return EMPTY_INTERVAL;
	
	double xl = m->getLow();
	double xh = m->getHigh();
	if (xl == xh) {
		return new Interval(sin(xl), sin(xh));
	} else if (xl >= -PI/2 && xl <= PI/2 && xh >= -PI/2 && xh <= PI/2) {
		return new Interval(sin(xl), sin(xh));
	} else if (xl >= - PI && xl <= -PI/2 && xh >= -PI && xh <= -PI/2 ) {
		return new Interval(sin(xh), sin(xl));
	} else if (xl >= PI/2 && xl <= PI && xh >= PI/2 && xh <= PI) {
		return new Interval(sin(xh), sin(xl));
	} else if (xl >= -PI && xl <= PI/2 && xh >= -PI && xh <= PI/2) {
		vector<double> vals;
		vals.push_back(sin(xl));
		vals.push_back(sin(xh));
		return new Interval(-1.0, findMax(vals));
	} else if (xl >= - PI/2 && xl <= PI && xh >= -PI/2 && xh <= PI) {
		vector<double> vals;
		vals.push_back(sin(xl));
		vals.push_back(sin(xh));
		return new Interval(findMin(vals), 1.0);
	}else { // TODO: this is so course grained
		return new Interval(-1.0, 1.0);
	}
}

Interval* Interval::i_cos(Interval* m) {
	if (m == FULL_INTERVAL) return new Interval(-1.0, 1.0);
	if (m == EMPTY_INTERVAL) return EMPTY_INTERVAL;
	
	double xl = m->getLow();
	double xh = m->getHigh();
	if (xl == xh) {
		return new Interval(cos(xl), cos(xh));
	} else if (xl >= 0 && xl <= PI && xh >= 0 && xh <= PI) {
		return new Interval(cos(xh), cos(xl));
	} else if (xl <= 0 && xl >= -PI && xh <= 0 && xh >= -PI) {
		return new Interval(cos(xl), cos(xh));
	} else if (xl >= - PI && xl <= PI && xh >= -PI && xh <= PI) {
		vector<double> vals;
		vals.push_back(cos(xl));
		vals.push_back(cos(xh));
		return new Interval(findMin(vals), 1.0);
	} else {
		return new Interval(-1.0, 1.0);
	}
}

Interval* Interval::i_tan(Interval* m) {
	if (m == FULL_INTERVAL) return FULL_INTERVAL;
	if (m == EMPTY_INTERVAL) return EMPTY_INTERVAL;
	
	double xl = m->getLow();
	double xh = m->getHigh();
	Assert(xl == xh, "NYI: range computation for tan");
	return new Interval(tan(xl), tan(xh));
}

Interval* Interval::i_sqrt(Interval* m) {
	if (m == FULL_INTERVAL) return new Interval(0, MAXVAL);
	if (m == EMPTY_INTERVAL) return EMPTY_INTERVAL;
	
	double xl = m->getLow();
	double xh = m->getHigh();
	//Assert(xh >= 0.0, "Invalid range");
	if (xh < 0.0) {
		return new Interval(0.0, 0.0);
	}
	if (xl < 0.0) {
		return new Interval(0.0, sqrt(xh));
	} else {
		return new Interval(sqrt(xl), sqrt(xh));
	}
}

Interval* Interval::i_cast_int_float(Interval* m) {
	return i_copy(m);
}

Interval* Interval::i_cast_float_int(Interval* m) {
	return i_copy(m);
}

Interval* Interval::i_arcsin(Interval* m) {
	if (m == EMPTY_INTERVAL) return EMPTY_INTERVAL;
	return FULL_INTERVAL;
}

Interval* Interval::i_arccos(Interval* m) {
	if (m == EMPTY_INTERVAL) return EMPTY_INTERVAL;
	return FULL_INTERVAL;
}

Interval* Interval::i_invsqrt(Interval* m) {
	if (m == FULL_INTERVAL) return new Interval(0, MAXVAL);
	if (m == EMPTY_INTERVAL) return EMPTY_INTERVAL;
	double xl = m->getLow();
	double xh = m->getHigh();
	return new Interval(xl*xl, xh*xh);
}

Interval* Interval::i_invsquare(Interval* m) {
	if (m == FULL_INTERVAL) return FULL_INTERVAL;
	if (m == EMPTY_INTERVAL) return EMPTY_INTERVAL;
	double xh = m->getHigh();
	return new Interval(-sqrt(xh), sqrt(xh));
}

// Copy i1 into i2
Interval* Interval::i_copy(Interval* i1) {
	if (i1 == FULL_INTERVAL) return FULL_INTERVAL;
	if (i1 == EMPTY_INTERVAL) return EMPTY_INTERVAL;
	return new Interval(i1->getLow(), i1->getHigh());
}

double Interval::findMin(const vector<double>& vals) {
	double minv = MAXVAL;
	for (int i = 0; i < vals.size(); i++) {
		if (vals[i] < minv) {
			minv = vals[i];
		}
	}
	return minv;
}

double Interval::findMax(const vector<double>& vals) {
	double maxv = MINVAL;
	for (int i = 0; i < vals.size(); i++) {
		if (vals[i] > maxv) {
			maxv = vals[i];
		}
	}
	return maxv;
}

string Interval::print() {
	if (this == FULL_INTERVAL) return "FULL INTERVAL";
	if (this == EMPTY_INTERVAL) return "EMPTY_INTERVAL";
	stringstream str;
	str << "[" << low << "," << high << "]";
	return str.str();
}
