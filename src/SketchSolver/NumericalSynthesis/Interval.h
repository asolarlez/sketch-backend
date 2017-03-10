#pragma once 
#include "BasicError.h"
#include <string>
#include <sstream>
#include <limits>
#include <math.h>
#include <vector>
#include "BasicError.h"


using namespace std;
class Interval {
	double low;
	double high;

public:
	static constexpr double MAXVAL = numeric_limits<double>::max();
	static constexpr double MINVAL = - numeric_limits<double>::max();
	static constexpr double PI = 3.1415926535897;
	static constexpr double PRECISION = 1e-8;
	
	Interval(double _low, double _high): low(_low), high(_high) {
		if (fabs(low - high) < PRECISION) {
			high = low;
		}
	}
	double getLow() const { return low; }
	double getHigh() const { return high; }
	void update(double _low, double _high) {
		low = _low;
		high = _high;
	}
	static Interval* i_plus(Interval* m, Interval* f); // m + f
	static Interval* i_minus(Interval* m, Interval* f); // m - f
	static Interval* i_times(Interval* m, Interval* f); // m * f
	static Interval* i_div(Interval* m, Interval* f); // m / f
	static Interval* i_neg(Interval* m); // -m
	static Interval* i_union(const vector<Interval*>& m); // m_1 V m_2 V ...
	static Interval* i_intersect(const vector<Interval*>& m); // m_1 ^ m_2 ^ ...
	static Interval* i_intersect(Interval* m, Interval* f); // m ^ f
	static Interval* i_equal(Interval* m, Interval* f); // m == f
	static Interval* i_lt(Interval* m, Interval* f); // m < f
	static Interval* i_square(Interval* m); // m * m
	static Interval* i_arctan(Interval* m); // arctan(m)
	static Interval* i_sin(Interval* m); // sin(m)
	static Interval* i_cos(Interval* m); // cos(m)
	static Interval* i_tan(Interval* m); // tan(m)
	static Interval* i_sqrt(Interval* m); // sqrt(m)
	static Interval* i_copy(Interval* i1); // copy i1 
	static Interval* i_cast_int_float(Interval* m);
	static Interval* i_cast_float_int(Interval* m);
	static Interval* i_arcsin(Interval* m);
	static Interval* i_arccos(Interval* m);
	static Interval* i_invsqrt(Interval* m);
	static Interval* i_invsquare(Interval* m);
	static Interval* i_and(Interval* m, Interval* f);
	static Interval* i_or(Interval* m, Interval* f);
	static Interval* i_not(Interval* m);
	
	static bool sameInterval(Interval* a, Interval* b) {
		if (a == b) return true;
		if (a == NULL || b == NULL) return false;
		if (fabs(a->getLow() - b->getLow()) < PRECISION && fabs(a->getHigh() - b->getHigh()) < PRECISION) {
			return true;
		} else {
			return false;
		}
	}
	string print(); 	
private:
	static double findMin(const vector<double>& vals);
	static double findMax(const vector<double>& vals);
};

extern Interval* EMPTY_INTERVAL;
extern Interval* FULL_INTERVAL;
