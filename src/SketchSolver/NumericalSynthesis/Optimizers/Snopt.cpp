#include "Snopt.h"

void SnoptSolver::init(char* workspace, integer neF_, SNOPT_DF_TYPE df, integer ObjRow, doublereal ObjAdd, doublereal *xlow_, doublereal *xupp_, doublereal *Flow_, doublereal *Fupp_) {
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
	integer neA = 0;
	integer neG = 0;
	for (integer i = 0; i < neF; i++) {
		for (integer j = 0; j < n; j++) {
			iGfun[neG] = i;
			jGvar[neG] = j;
			neG++;
		}
	}
	
	snoptProb.setProblemSize(n, neF);
	snoptProb.setObjective(ObjRow, ObjAdd);
	snoptProb.setA(lenA, iAfun, jAvar, A);
	snoptProb.setG(lenG, iGfun, jGvar);
	snoptProb.setX(x, xlow, xupp, xmul, xstate);
	snoptProb.setF(F, Flow, Fupp, Fmul, Fstate);
	
	snoptProb.setNeA(neA);
	snoptProb.setNeG(neG);
	snoptProb.setUserFun(df);
	snoptProb.setWorkspace(workspace);
}

bool SnoptSolver::optimize(gsl_vector* initState) {
	for (int i = 0; i < n; i++) {
		x[i] = gsl_vector_get(initState, i);
		xstate[i] = 0;
	}
	
	for (int i = 0; i < neF; i++) {
		Fmul[i] = 0;
		Fstate[i] = 0;
	}
	
	snoptProb.setIntParameter("Derivative option", 1);
	snoptProb.setIntParameter("Major Iteration limit", 250);
	snoptProb.setIntParameter("Summary file", 1);

	integer status = snoptProb.solve(Cold);
	
	for (int i = 0; i < n; i++) {
		gsl_vector_set(result, i, x[i]);
	}
	objectiveVal = F[0];
	cout << "Status: " << status << endl;
	if (status >= 1 && status <= 9) {
		cout << "Solution found" << endl;
	} else if (status >= 11 && status <= 19) {
		cout << "Infeasible constraints" << endl;
	} else if (status == 91) {
		cout << "Invalid input" << endl;
	} else {
		cout << "Unknown error " << status << endl;
	}
	
	cout << "x = ";
	for (int i = 0; i < n; i++){
		cout << x[i] << ", ";
	}
	cout << endl;
	cout << "F = ";
	for (int i = 0; i < neF; i++){
		cout << F[i] << ", ";
	}
	cout << endl;
	
	return status >= 1 && status <= 9;
}

