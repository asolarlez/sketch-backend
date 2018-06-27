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


bool MaxSolver::optimize(gsl_vector* initState, gsl_vector* initGrad, bool suppressPrint) {
	for (int i = 0; i < n; i++) {
		x[i] = gsl_vector_get(initState, i);
	}
	
	integer nasserts;
	df(&n, x, &neF, F, &lenG, G, workspace, &nasserts);
	obj = F[0];
	cout << obj << " " << G[0] << " " << gsl_vector_get(initGrad, 0) << endl;
	if (norm(G, 0, n) < 0.01) {
		for(int i = 0; i < n; i++) {
			grad[i] = gsl_vector_get(initGrad, i);
		}
	} else {
		copy(G, 0, n, grad, 0);
	}
	double ng = norm(grad, 0, n);
	update(x, grad, stepSize/ng, n);



	for (int i = 0; i < maxSteps; i++) {
		if (!inLimits()) {
			break;
		}
		df(&n, x, &neF, F, &lenG, G, workspace, &nasserts);
		if (F[0] < obj - 0.01) {
			break;
		} else {
			obj = F[0];
			cout << obj << " " << G[0] << endl;
			copy(G, 0, n, grad, 0);
			ng = norm(grad, 0, n);
			update(x, grad, stepSize/ng, n);
		}
	}
	
	for (int i = 0; i < n; i++) {
		gsl_vector_set(result, i, x[i]);
	}
	objectiveVal = F[0];
    
	
	return true;
}


