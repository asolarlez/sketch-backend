#include "Snopt.h"
#include <iostream>
#include <cassert>
#include <cmath>

using namespace std;

void test() {
	class Eval {
	public:
		static int df(integer    *Status, integer *n,    doublereal x[],
									integer    *needF,  integer *neF,  doublereal F[],
									integer    *needG,  integer *neG,  doublereal G[],
									char      *cu,			 integer *lencu,
									integer    iu[],    integer *leniu,
									doublereal ru[],    integer *lenru)  {
			F[0] = 0;
			F[1] = 1000;
			F[2] = 1000;
			G[0] = 0;
			G[1] = 0;
			G[2] = 0;
			return 0;
		}
	};

	SnoptSolver* snopt = new SnoptSolver(1, 3, 10);
	doublereal* xlow = new doublereal[1];
	doublereal* xupp = new doublereal[1];
	doublereal* Flow = new doublereal[3];
	doublereal* Fupp = new doublereal[3];
	xlow[0] = -1e20;
	xupp[0] = 1e20;
	Flow[0] = -1e20; Flow[1] = 0; Flow[2] = 0;
	Fupp[0] = 1e20; Fupp[1] = 1e20; Fupp[2] = 1e20;
	
	snopt->init((char *)snopt, 3, Eval::df, 0, 0.0, xlow, xupp, Flow, Fupp);
	gsl_vector* init = gsl_vector_alloc(1);
	gsl_vector_set(init, 0, -100.0);
	snopt->optimize(init);
}


int main() {
	test();
	
	cout << "Passed tests" << endl;
	return 0;
}


