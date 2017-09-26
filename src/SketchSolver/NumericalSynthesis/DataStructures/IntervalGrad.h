#pragma once
#include "GradUtil.h"
#include "DistanceGrad.h"
#include <gsl/gsl_vector.h>
#include <gsl/gsl_blas.h>
#include <limits>
#include <math.h>
#include <vector>
#include "BasicError.h"

using namespace std;

class IntervalGrad {
	double low;
	double high;
	gsl_vector* lgrad;
	gsl_vector* hgrad;
	
	//static constexpr double DELTA = 0.7;

public:
	bool singleton;
	IntervalGrad(double _low, double _high, gsl_vector* _lgrad, gsl_vector* _hgrad): low(_low), high(_high), lgrad(_lgrad), hgrad(_hgrad) {
		singleton = false;
	}
	~IntervalGrad(void) {
		delete lgrad;
		delete hgrad;
	}
	double getLow() const { return low; }
	double getHigh() const { return high; }
	gsl_vector* getLGrad() const { return lgrad; }
	gsl_vector* getHGrad() const { return hgrad; }
	void update(double _low, double _high) {
		low = _low;
		high = _high;
	}
	static void ig_plus(IntervalGrad* m, IntervalGrad* f, IntervalGrad* o); // o = m + f
	static void ig_times(IntervalGrad* m, IntervalGrad* f, IntervalGrad* o); // o = m * f
	static void ig_div(IntervalGrad* m, IntervalGrad* f, IntervalGrad* o); // o = m / f
	static void ig_neg(IntervalGrad* m, IntervalGrad* o); // o = -m
	static void ig_union(const vector<IntervalGrad*>& m, IntervalGrad* o); // o = m_1 V m_2 V ...
	static void ig_equal(IntervalGrad* m, IntervalGrad* f, DistanceGrad* o); // o = m == f
	static void ig_lt(IntervalGrad* m, IntervalGrad* f, DistanceGrad* o); // o = m < f
	static void ig_square(IntervalGrad* m, IntervalGrad* o); // o = m * m
	static void ig_arctan(IntervalGrad* m, IntervalGrad* o); // o = arctan(m)
	static void ig_sin(IntervalGrad* m, IntervalGrad* o); // o = sin(m)
	static void ig_cos(IntervalGrad* m, IntervalGrad* o); // o = cos(m)
	static void ig_tan(IntervalGrad* m, IntervalGrad* o); // o = tan(m)
	static void ig_sqrt(IntervalGrad* m, IntervalGrad* o); // o = sqrt(m)
	static void ig_exp(IntervalGrad* m, IntervalGrad* o);
	static void ig_copy(IntervalGrad* i1, IntervalGrad* i2); // copy i1 into i2
	static void ig_cast_int_float(IntervalGrad* m, IntervalGrad* o);
	
	static void ig_conditionalUnion(IntervalGrad* m, IntervalGrad* f, DistanceGrad* d, IntervalGrad* o);
	static void ig_intersect(const vector<IntervalGrad*>& m, IntervalGrad* o);
	
	string print() {
		stringstream str;
		str << "Range: " << low << " " << high;
		return str.str();
	}
	
	string printFull() {
		stringstream str;
		str << "Range: " << low << " " << high << endl;
		str << "LGrads: ";
		for (int i = 0; i < lgrad->size; i++) {
		 str << gsl_vector_get(lgrad, i) << ", ";
		}
		str << endl;
		str << "HGrads: " ;
		for (int i = 0; i < hgrad->size; i++) {
		 str << gsl_vector_get(hgrad, i) << ", ";
		}
		return str.str();
	}
	
private:
	void bound() {
		double oldLow = low;
		low = GradUtil::bound(low);
		if (oldLow != low) {
			GradUtil::default_grad(lgrad);
		}
		double oldHigh = high;
		high = GradUtil::bound(high);
		if (oldHigh != high) {
			GradUtil::default_grad(hgrad);
		}
		
		for (int i = 0; i < lgrad->size; i++) {
			gsl_vector_set(lgrad, i, GradUtil::bound(gsl_vector_get(lgrad, i)));
			gsl_vector_set(hgrad, i, GradUtil::bound(gsl_vector_get(hgrad, i)));
		}
	}
};
