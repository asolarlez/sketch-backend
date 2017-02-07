#pragma once 
#include "BasicError.h"
#include <string>
#include <sstream>

using namespace std;
class Interval {
	float low;
	float high;
	
public:
	Interval(float _low, float _high): low(_low), high(_high) {}
	float getLow() const { return low; }
	float getHigh() const { return high; }
	void update(float _low, float _high) {
		low = _low;
		high = _high;
	}
	Interval interval_add(Interval a, Interval b);
	Interval interval_sub(Interval a, Interval b);
	Interval interval_mul(Interval a, Interval b);
	Interval interval_div(Interval a, Interval b);
	Interval interval_neg(Interval a);
	Interval interval_mux(Interval a, Interval b);
	bool interval_equal(Interval a, Interval b);
	bool interval_lt(Interval a, Interval b);
	
	string print() {
		stringstream str;
		str << "Range: " << low << " " << high;
		return str.str();
	}
};
