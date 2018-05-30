#pragma once

#include "BasicError.h"
#include <vector>
#include <map>
#include <unordered_map>
#include <cmath>
#include <functional>
#include "CommandLineArgs.h"
#include "FastSet.h"

using namespace std;

typedef float(*floatfun)(float);

class FloatManager;

template<typename Op>
class FloatOp {
	FloatManager& fm;
public:
	FloatOp(FloatManager& _fm) :fm(_fm) {}
	int operator()(int x, int y);
};

class FloatFun {
protected:
	FloatManager& fm;
	floatfun f;
public:
	FloatFun(FloatManager& _fm, floatfun _f) :fm(_fm), f(_f) {}
	inline int operator()(int x);
	inline float apply(float in) {
		return f(in);
	}
};





extern CommandLineArgs* PARAMS;

inline
float tlog(float in) {
	if (in < PARAMS->epsilon) {
		return log(PARAMS->epsilon / 2.0);
	}
	else {
		return log(in);
	}

}


class FloatManager {
	map<float, int> floatIdx;
	vector<float> floats;
	map<string, floatfun> floatfuns;

public:
	const float epsilon;
	FloatManager(float _epsilon) :epsilon(_epsilon) {
		floatIdx[0.0] = 0;
		floats.push_back(0.0);
		floatfuns["arctan_math"] = atan;
		floatfuns["sin_math"] = sin;
		floatfuns["cos_math"] = cos;
		floatfuns["tan_math"] = tan;
		floatfuns["sqrt_math"] = sqrt;
		floatfuns["log_math"] = tlog;
	}

	bool hasFun(const string& name) {
		return floatfuns.count(name) > 0;
	}
	FloatFun getFun(const string& name) {
		return FloatFun(*this, floatfuns[name]);
	}

	float operator()(int id) {
		return getFloat(id);
	}

	float getFloat(int id) {
		if (id < 0) {
			return -floats[-id];
		}
		else {
			return	floats[id];
		}

	}

	int getIdx(float x) {
		//floatIdx only stores positive values. Negative values will yield negative indices. 
		//This means that negating the index will automatically negate the value that the index corresponds to.
		//That's why zero must always be stored in index zero.
		bool isNeg = false;
		if (x < -0.0) {
			x = -x;
			isNeg = true;
		}
		auto lbd = floatIdx.lower_bound(x - epsilon + (epsilon / 100));

		if (lbd != floatIdx.end()) {
			float dist = lbd->first - x;
			if (-epsilon < dist && dist < epsilon) {
				if (isNeg) {
					return -lbd->second;
				}
				else {
					return  lbd->second;
				}
			}
			lbd++;
			if (lbd != floatIdx.end()) {
				dist = lbd->first - x;
				if (-epsilon < dist && dist < epsilon) {
					if (isNeg) {
						return -lbd->second;
					}
					else {
						return  lbd->second;
					}
				}
			}
		}


		int pos = floatIdx.size();
		floatIdx[x] = pos;
		floats.push_back(x);
		Assert(floatIdx.size() == floats.size(), "What???");
		if (isNeg) {
			return -pos;
		}
		else {
			return pos;
		}
	}
};



/*

class FloatManager {	
	vector<uint64_t> floats;
	//vector<double> actualfloats;
	map<string, floatfun> floatfuns;
	int count;

public:
	const float epsilon;
	FloatManager(float _epsilon) :epsilon(_epsilon) {				
		floats.push_back(0.0);
		floatfuns["arctan_math"] = atan;
		floatfuns["sin_math"] = sin;
		floatfuns["cos_math"] = cos;
		floatfuns["tan_math"] = tan;
		floatfuns["sqrt_math"] = sqrt;	
		floatfuns["log_math"] = tlog;	
		count = 0;
	}

	int size() {
		return count;
	}

	bool hasFun(const string& name) {
		return floatfuns.count(name) > 0;
	}
	FloatFun getFun(const string& name) {		
		return FloatFun(*this, floatfuns[name]);
	}

	double operator()(int id) {
		return getFloat(id);
	}

	double getFloat(int id) {		
		if (id < 0) {
			double v =  ((double)floats[-id])*epsilon;
			return -v;
		}
		else {
			double v =  ((double)floats[id])*epsilon;
			return	v;
		}

	}

	int getIdx(double x) {
		const int NPRIMES = 9;
		const int FLSIZES[NPRIMES] = {2029, 4049, 8093, 16063, 32999, 64091, 125659, 265117, 500083};
		const int NSTEPS = 36;
		int STEPS[NSTEPS] = { 0, 1, 2, 3, 4, 5, 6, 6, 6, 6, 6, 6, 6, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8};
			                //0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28 29 30 31 32 33 34 35
		const uint64_t FLEMPTY = UINT64_MAX;

		//floatIdx only stores positive values. Negative values will yield negative indices. 
		//This means that negating the index will automatically negate the value that the index corresponds to.
		//That's why zero must always be stored in index zero.
		bool isNeg = false;
		if (x < -0.0) {
			x = -x;
			isNeg = true;
		}

		auto maybeneg = [=](int val) {
			return isNeg ? -val : val;
		};
		
		double iidx = (x-(epsilon/2)) / epsilon;		
		uint64_t idx = max(0.0, floor(iidx));		
		
		Assert(idx != FLEMPTY, "Doesn't work! nuyftgjk,;atg");
		int lidx;
		int prev = -1;		
		for (int i = 0; i < NSTEPS; ++i) {
			int cur = STEPS[i];
			if (cur != prev) {				
				lidx = idx %FLSIZES[cur];
			}
			else {
				lidx = (idx %FLSIZES[cur-1]) + FLSIZES[cur - 1] + i;
			}
			prev = cur;
			
			if (lidx < floats.size()) {
				uint64_t tmp = floats[lidx];
				if (tmp == FLEMPTY) {
					floats[lidx] = idx;
					// actualfloats[lidx] = x;
					++count;
					return maybeneg(lidx);
				}
				if (tmp == idx) {
					return maybeneg(lidx);
				}
				else {
					continue;
				}
			}
			else {
				floats.resize(lidx + 2, FLEMPTY);
				// actualfloats.resize(lidx + 2);
				floats[lidx] = idx;
				// actualfloats[lidx] = x;
				++count;
				return maybeneg(lidx);
			}
		}		
		Assert(false, "NO MORE ROOM!! size="<<count);
		return -1;
		
	}
};
*/

class FloatComp {
protected:
  FloatManager& fm;
public:
  FloatComp(FloatManager& _fm) :fm(_fm) {}
  bool operator() (const int x, const int y) const {
    return fm.getFloat(x) < fm.getFloat(y);
  }
};

template<typename Op>
inline
int FloatOp<Op>::operator()(int x, int y) {
	Op op;
	float xf = fm.getFloat(x);
	float yf = fm.getFloat(y);
	return fm.getIdx(op(xf, yf));
}

template<typename T>
class FloatOp<std::divides<T> > {
	FloatManager& fm;
public:
	FloatOp(FloatManager& _fm) :fm(_fm) {}
	int operator()(int x, int y) {
		if (y == 0) { return 0;  }
		float xf = fm.getFloat(x);
		float yf = fm.getFloat(y);
		return fm.getIdx((xf / yf));
	}
};


inline int FloatFun::operator()(int x) {
	float xf = fm.getFloat(x);
	return fm.getIdx(f(xf));
}

