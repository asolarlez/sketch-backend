#include "MaxSolver.h"


void MaxSolver::init(char* _workspace, integer neF_, MAX_SOLVER_DF_TYPE _df, doublereal *xlow_, doublereal *xupp_, doublereal *Flow_, doublereal *Fupp_) {
	Assert(neF_ <= neF, "Error: small nef");
	neF = neF_;
	for (integer i = 0; i < n; i++) {
		xlow[i] = xlow_[i];
		xupp[i] = xupp_[i];
	}
	
	for (integer i = 0; i < neF; i++) {
		Flow[i] = Flow_[i];
		Fupp[i] = Fupp_[i];
	}
	integer neG = 0;
	for (integer i = 0; i < neF; i++) {
		for (integer j = 0; j < n; j++) {
			iGfun[neG] = i;
			jGvar[neG] = j;
			neG++;
		}
	}

	df = _df;
	workspace = _workspace;
}

double norm(doublereal* v, int start, int len) {
	double res = 0.0;
	for (int i = start; i < start + len; i++) {
		res += v[i]*v[i];
	}
	return res;
}

void copy(doublereal* v, int start, int len, doublereal* newv, int start1) {
	int j = start1;
	for (int i = start; i < start + len; i++, j++) {
		newv[j] = v[i]; 
	}
}

void update(doublereal* x, doublereal* g, doublereal d, int len) {
	for (int i = 0; i < len; i++) {
		x[i] += g[i]*d;
	}
} 

string print(doublereal* v, int start, int len) {
	stringstream s;
	for (int i = start; i < start + len; i++) {
		s << v[i] << ",";
	}
	return s.str();
}

bool MaxSolver::inLimits() {
	for (int i = 0; i < n; i++) {
		if (x[i] < xlow[i] - 0.01 || x[i] > xupp[i] + 0.01) {
			return false;
		}
	}
	return true;
}


bool MaxSolver::optimize(const gsl_vector* initState, const gsl_vector* initDir,  bool suppressPrint) {
	for (int i = 0; i < n; i++) {
		x[i] = gsl_vector_get(initState, i);
	}
	
	integer nasserts;
	df(&n, x, &neF, F, &lenG, G, workspace, &nasserts);
	obj = F[0];
	
	double ng = norm(G, 0, n);
	double factor = stepSize;
	//cout << "O:" << F[0] << " " << ng << endl;
	if (ng > 0.1) {
		factor = stepSize/ng;
		copy(G, 0, n, grad, 0);
	} else {
		for (int i = 0; i < n; i++) {
			grad[i] = gsl_vector_get(initDir, i);
		}
		ng = norm(grad, 0, n);
		factor = stepSize/ng;
		//cout << "INITIAL GRAD IS TOO SMALL" << endl;
	}
	
	copy(x, 0, n, oldx, 0);
	update(x, grad, factor, n);

	for (int i = 0; i < maxSteps; i++) {
		if (!inLimits()) {
			break;
		}
		df(&n, x, &neF, F, &lenG, G, workspace, &nasserts);
		if (F[0] < obj - 0.1) {
			break;
		} else {
			obj = F[0];
			ng = norm(G, 0, n);
			//cout << "O:" << F[0] << " " << ng << endl;
			double factor = stepSize;
			if (ng > 0.1) {
				factor = stepSize/ng;
				copy(G, 0, n, grad, 0);
			} else {
				// use old grad
				ng = norm(grad, 0, n);
				factor = stepSize/ng;
			}
			copy(x, 0, n, oldx, 0);
			update(x, grad, factor, n);
		}
	}
	
	for (int i = 0; i < n; i++) {
		gsl_vector_set(result, i, oldx[i]);
	}
	objectiveVal = obj;
    
	
	return true;
}


