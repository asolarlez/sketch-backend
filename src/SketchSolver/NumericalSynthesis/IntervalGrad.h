#pragma once

#include <gsl/gsl_vector.h>
#include <limits>
#include <math.h>
#include <vector>
#include "BasicError.h"

using namespace std;
class IntervalGrad {
	float low;
	float high;
	gsl_vector* lgrad;
	gsl_vector* hgrad;
	
	static constexpr float MAXVAL = numeric_limits<float>::max();
	static constexpr float MINVAL = - numeric_limits<float>::max();
	static constexpr float PI = 3.1415926535897;
	
public:
	static gsl_vector* tmp;
	IntervalGrad(float _low, float _high, gsl_vector* _lgrad, gsl_vector* _hgrad): low(_low), high(_high), lgrad(_lgrad), hgrad(_hgrad) {	}
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
	
private:
	static float findMin(const vector<float>& vals, const vector<gsl_vector*>& grads, gsl_vector* l);
	static float findMax(const vector<float>& vals, const vector<gsl_vector*>& grads, gsl_vector* h);
	static void softMinMax(const vector<float>& vals, const vector<gsl_vector*>& grads, gsl_vector* l, float alpha, float t);
	
	static void default_grad(gsl_vector* out);
	static void compute_mult_grad(float mval, float fval, gsl_vector* mgrads, gsl_vector* hgrads, gsl_vector* out);
	static gsl_vector* compute_mult_grad(float mval, float fval, gsl_vector* mgrads, gsl_vector* hgrads);
	static void compute_div_grad(float mval, float fval, gsl_vector* mgrads, gsl_vector* hgrads, gsl_vector* out);
	static gsl_vector* compute_div_grad(float mval, float fval, gsl_vector* mgrads, gsl_vector* hgrads);
	
	static void compute_square_grad(float mval, gsl_vector* mgrads, gsl_vector* out);
	static gsl_vector* compute_square_grad(float mval, gsl_vector* mgrads);
	static void compute_arctan_grad(float mval, gsl_vector* mgrads, gsl_vector* out);
	static void compute_sin_grad(float mval, gsl_vector* mgrads, gsl_vector* out);
	static void compute_cos_grad(float mval, gsl_vector* mgrads, gsl_vector* out);
	static void compute_tan_grad(float mval, gsl_vector* mgrads, gsl_vector* out);
	static void compute_sqrt_grad(float mval, gsl_vector* mgrads, gsl_vector* out);
	
};
