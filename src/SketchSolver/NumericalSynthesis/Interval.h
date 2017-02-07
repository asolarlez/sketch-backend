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
	bool empty;
	
	static constexpr float MAXVAL = numeric_limits<float>::max();
	static constexpr float MINVAL = - numeric_limits<float>::max();
	static constexpr float PI = 3.1415926535897;

public:
	Interval(float _low, float _high): low(_low), high(_high), empty(false) {}
	float getLow() const { return low; }
	float getHigh() const { return high; }
	bool isEmpty() const { return empty; }
	void makeEmpty() { empty = true; }
	void update(float _low, float _high) {
		low = _low;
		high = _high;
	}
	static void i_plus(Interval* m, Interval* f, Interval* o); // o = m + f
	static void i_minus(Interval* m, Interval* f, Interval* o); // o = m - f
	static void i_times(Interval* m, Interval* f, Interval* o); // o = m * f
	static void i_div(Interval* m, Interval* f, Interval* o); // o = m / f
	static void i_neg(Interval* m, Interval* o); // o = -m
	static void i_union(const vector<Interval*>& m, Interval* o); // o = m_1 V m_2 V ...
	static void i_intersect(const vector<Interval*>& m, Interval* o); // o = m_1 ^ m_2 ^ ...
	static void i_equal(Interval* m, Interval* f, Interval* o); // o = m == f
	static void i_lt(Interval* m, Interval* f, Interval* o); // o = m < f
	static void i_square(Interval* m, Interval* o); // o = m * m
	static void i_arctan(Interval* m, Interval* o); // o = arctan(m)
	static void i_sin(Interval* m, Interval* o); // o = sin(m)
	static void i_cos(Interval* m, Interval* o); // o = cos(m)
	static void i_tan(Interval* m, Interval* o); // o = tan(m)
	static void i_sqrt(Interval* m, Interval* o); // o = sqrt(m)
	static void i_copy(Interval* i1, Interval* i2); // copy i1 into i2
	static void i_cast_int_float(Interval* m, Interval* o);
	
	string print() {
		stringstream str;
		str << "Range: " << low << " " << high;
		return str.str();
	}
	
private:
	static float findMin(const vector<float>& vals);
	static float findMax(const vector<float>& vals);
};
