#ifndef FINDCHECKSOLVER_H_
#define FINDCHECKSOLVER_H_

#include "BooleanToCNF.h"

class SolverException{
	public:
	int code;
	string msg;
	SolverException(int code_p, const string& msg_p){ msg = msg_p; code = code_p; };
};



#ifdef OUT
#undef OUT
#endif



#endif /*FINDCHECKSOLVER_H_*/
