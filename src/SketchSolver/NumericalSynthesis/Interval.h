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
	float low;
	float high;

public:
	static constexpr float MAXVAL = numeric_limits<float>::max();
	static constexpr float MINVAL = - numeric_limits<float>::max();
	static constexpr float PI = 3.1415926535897;
	
	Interval(float _low, float _high): low(_low), high(_high) {}
	float getLow() const { return low; }
	float getHigh() const { return high; }
	void update(float _low, float _high) {
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
	
	static bool sameInterval(Interval* a, Interval* b) {
		if (a == b) return true;
		if (a == NULL || b == NULL) return false;
		if (a->getLow() == b->getLow() && a->getHigh() == b->getHigh()) {
			return true;
		} else {
			return false;
		}
	}
	string print(); 	
private:
	static float findMin(const vector<float>& vals);
	static float findMax(const vector<float>& vals);
};

extern Interval* EMPTY_INTERVAL;
extern Interval* FULL_INTERVAL;
