#pragma once

#include <gsl/gsl_vector.h>
#include <limits>
#include <math.h>
#include <vector>
#include "BasicError.h"

using namespace std;
class DistanceGrad {
public:
	float dist;
	gsl_vector* grad;
	bool set; // TODO: this bit is not necessary if the default dist is 0
	DistanceGrad(float d, gsl_vector* g): dist(d), grad(g), set(true) {}
	~DistanceGrad(void) {
		delete grad;
	}
	string print() {
		stringstream str;
		if (set) {
			str << "Dist: " << dist << endl;
			str << "DGrads: ";
			for (int i = 0; i < grad->size; i++) {
				str << gsl_vector_get(grad, i) << ", ";
			}
		} else {
			str << "Dist: NOT SET";
		}
		return str.str();
	}
};

class IntervalGrad {
	float low;
	float high;
	gsl_vector* lgrad;
	gsl_vector* hgrad;
	
	static constexpr float MAXVAL = 1e10;
	static constexpr float MINVAL = -1e10;
	static constexpr float PI = 3.1415926535897;
	static constexpr float MINSIZE = 0.01;
	
	static constexpr float DELTA = 0.7;
	static constexpr float ALPHA = 10;
	static constexpr float BETA = -10;
	
public:
	static gsl_vector* tmp;
	static gsl_vector* tmp1;
	static gsl_vector* tmp2;
	static gsl_vector* tmp3;
	static gsl_vector* tmpT; // used for internal computations in mult, div - don't use it anywhere else
	bool singleton;
	IntervalGrad(float _low, float _high, gsl_vector* _lgrad, gsl_vector* _hgrad): low(_low), high(_high), lgrad(_lgrad), hgrad(_hgrad) {
		singleton = false;
	}
	~IntervalGrad(void) {
		delete lgrad;
		delete hgrad;
	}
	float getLow() const { return low; }
	float getHigh() const { return high; }
	gsl_vector* getLGrad() const { return lgrad; }
	gsl_vector* getHGrad() const { return hgrad; }
	void update(float _low, float _high) {
		low = _low;
		high = _high;
	}
	static void ig_plus(IntervalGrad* m, IntervalGrad* f, IntervalGrad* o); // o = m + f
	static void ig_times(IntervalGrad* m, IntervalGrad* f, IntervalGrad* o); // o = m * f
	static void ig_div(IntervalGrad* m, IntervalGrad* f, IntervalGrad* o); // o = m / f
	static void ig_neg(IntervalGrad* m, IntervalGrad* o); // o = -m
	static void ig_union(const vector<IntervalGrad*>& m, IntervalGrad* o); // o = m_1 V m_2 V ...
	static void ig_equal(IntervalGrad* m, IntervalGrad* f, IntervalGrad* o); // o = m == f
	static void ig_lt(IntervalGrad* m, IntervalGrad* f, IntervalGrad* o); // o = m < f
	static void ig_square(IntervalGrad* m, IntervalGrad* o); // o = m * m
	static void ig_arctan(IntervalGrad* m, IntervalGrad* o); // o = arctan(m)
	static void ig_sin(IntervalGrad* m, IntervalGrad* o); // o = sin(m)
	static void ig_cos(IntervalGrad* m, IntervalGrad* o); // o = cos(m)
	static void ig_tan(IntervalGrad* m, IntervalGrad* o); // o = tan(m)
	static void ig_sqrt(IntervalGrad* m, IntervalGrad* o); // o = sqrt(m)
	static void ig_copy(IntervalGrad* i1, IntervalGrad* i2); // copy i1 into i2
	static void ig_cast_int_float(IntervalGrad* m, IntervalGrad* o);
	
	static void ig_conditionalUnion(IntervalGrad* m, IntervalGrad* f, DistanceGrad* d, IntervalGrad* o);
	static void ig_intersect(const vector<IntervalGrad*>& m, IntervalGrad* o);
	
	double getSize() {
		if (high - low < MINSIZE) return MINSIZE;
		return high - low;
	}
	
	string print() {
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
	
	static float findMin(float val1, float val2, gsl_vector* grad1, gsl_vector* grad2, gsl_vector* l);
	static float findMin(const vector<float>& vals, const vector<gsl_vector*>& grads, gsl_vector* l);
	static float findMax(float val1, float val2, gsl_vector* grad1, gsl_vector* grad2, gsl_vector* h);
	static float findMax(const vector<float>& vals, const vector<gsl_vector*>& grads, gsl_vector* h);
	static float sigmoid(float d, gsl_vector* grads, gsl_vector* out);
	static void default_grad(gsl_vector* out);

private:
	static float softMinMax(const vector<float>& vals, const vector<gsl_vector*>& grads, gsl_vector* l, float alpha, float t);
	
	static void compute_mult_grad(float mval, float fval, gsl_vector* mgrads, gsl_vector* hgrads, gsl_vector* out);
	static void compute_div_grad(float mval, float fval, gsl_vector* mgrads, gsl_vector* hgrads, gsl_vector* out);
	
	static void compute_square_grad(float mval, gsl_vector* mgrads, gsl_vector* out);
	static void compute_arctan_grad(float mval, gsl_vector* mgrads, gsl_vector* out);
	static void compute_sin_grad(float mval, gsl_vector* mgrads, gsl_vector* out);
	static void compute_cos_grad(float mval, gsl_vector* mgrads, gsl_vector* out);
	static void compute_tan_grad(float mval, gsl_vector* mgrads, gsl_vector* out);
	static void compute_sqrt_grad(float mval, gsl_vector* mgrads, gsl_vector* out);
	
	static bool inLimit(float v) {
		if (!isfinite(v)) return false;
		if (v > MAXVAL || v < MINVAL) return false;
		return true;
	}
	
	float bound(float v) {
		if (!inLimit(v)) {
			return v > 0 ? MAXVAL : MINVAL;
		} else {
			return v;
		}
	}
	
	void bound() {
		low = bound(low);
		high = bound(high);
		
		for (int i = 0; i < lgrad->size; i++) {
			gsl_vector_set(lgrad, i, bound(gsl_vector_get(lgrad, i)));
			gsl_vector_set(hgrad, i, bound(gsl_vector_get(hgrad, i)));
		}
	}
};
