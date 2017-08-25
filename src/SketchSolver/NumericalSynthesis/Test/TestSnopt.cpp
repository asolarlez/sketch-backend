#include <iostream>

#include "snopt.hh"
#include "snoptProblem.hh"

using namespace std;

int toyusrf(integer *Status, integer *n, doublereal x[],
						 integer *needF, integer *neF, doublereal F[],
						 integer *needG, integer *neG, doublereal G[],
						 char *cu, integer *lencu,
						 integer iu[], integer *leniu,
						 doublereal ru[], integer*lenru) {
	F[0] = x[1];
	F[1] = x[0]*x[0] + 4*x[1]*x[1];
	F[2] = (x[0] - 2)*(x[0] - 2) + x[1]*x[1];
	return 0;
}

int toyusrfg(integer    *Status, integer *n,    doublereal x[],
							integer    *needF,  integer *neF,  doublereal F[],
							integer    *needG,  integer *neG,  doublereal G[],
							char      *cu,  integer *lencu,
							integer    iu[],    integer *leniu,
							doublereal ru[],    integer *lenru) {
	if (*needF > 0) {
		F[0] = x[1];
		F[1] = x[0]*x[0] + 4*x[1]*x[1];
		F[2] = (x[0] - 2)*(x[0] - 2) + x[1]*x[1];
	}
	
	if (*needG > 0) {
		G[0] = 0;
		G[1] = 1;
		G[2] = 2*x[0];
		G[3] = 8*x[1];
		G[4] = 2*(x[0] - 2);
		G[5] = 2*x[1];
	}
	return 0;
}


int main(int argc, char **argv) {
	snoptProblem ToyProb;
	
	integer n  = 2;
	integer neF = 3;
	integer lenA = 10;
	
	integer *iAfun = new integer[lenA];
	integer *jAvar = new integer[lenA];
	doublereal *A = new doublereal[lenA];
	
	integer lenG = 10;
	integer* iGfun = new integer[lenG];
	integer *jGvar = new integer[lenG];
	
	doublereal *x = new doublereal[n];
	doublereal *xlow = new doublereal[n];
	doublereal *xupp = new doublereal[n];
	doublereal *xmul = new doublereal[n]; // Not sure what this is?
	integer *xstate = new integer[n]; // Not sure what this is?
	
	doublereal *F = new doublereal[neF];
	doublereal *Flow = new doublereal[neF];
	doublereal *Fupp = new doublereal[neF];
	doublereal *Fmul = new doublereal[neF];
	integer *Fstate = new integer[neF];
	
	integer nxnames = 1;
	integer nFnames = 1;
	char *xnames = new char[nxnames*8];
	char *Fnames = new char[nFnames*8];
	
	integer ObjRow = 0;
	doublereal ObjAdd = 0;
	
	xlow[0] = 0.0; xlow[1] = -1e20;
	xupp[0] = 1e20; xupp[1] = 1e20;
	xstate[0] = 0; xstate[1] = 0;
	
	Flow[0] = -1e20; Flow[1] = -1e20; Flow[2] = -1e20;
	Fupp[0] = 1e20; Fupp[1] = 4.0; Fupp[2] = 5.0;
	
	x[0] = 1.0;
	x[1] = 1.0;
	
	ToyProb.setProblemSize(n, neF);
	ToyProb.setObjective(ObjRow, ObjAdd);
	ToyProb.setA(lenA, iAfun, jAvar, A);
	ToyProb.setG(lenG, iGfun, jGvar);
	ToyProb.setX(x, xlow, xupp, xmul, xstate);
	ToyProb.setF(F, Flow, Fupp, Fmul, Fstate);
	ToyProb.setXNames(xnames, nxnames);
	ToyProb.setFNames(Fnames, nFnames);
	//ToyProb.setProbName("Toy0");
	ToyProb.setUserFun(toyusrf);
	
	ToyProb.computeJac();
	ToyProb.setIntParameter("Derivative option", 0);
	
	integer Cold = 0, Basis = 1, Warm = 2;
	ToyProb.solve(Cold);
	
	integer status;
	ToyProb.getIntParameter("", status);
	cout << status << endl;
	
	
	for (int i = 0; i < n; i++){
		cout << "x = " << x[i] << " xstate = " << xstate[i] << endl;
	}
	for (int i = 0; i < neF; i++){
		cout << "F = " << F[i] << " Fstate = " << Fstate[i] << endl;
	}
	
	delete []iAfun;  delete []jAvar;  delete []A;
	delete []iGfun;  delete []jGvar;
	
	delete []x;      delete []xlow;   delete []xupp;
	delete []xmul;   delete []xstate;
	
	delete []F;      delete []Flow;   delete []Fupp;
	delete []Fmul;   delete []Fstate;
}
