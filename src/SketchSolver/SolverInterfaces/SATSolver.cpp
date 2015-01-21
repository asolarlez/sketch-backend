#include "SATSolver.h"
//#include "ABCSATSolver.h"
#include "MiniSATSolver.h"
//#include "ZchaffSATSolver.h"


void SATSolver::lightSolve(){
}


void SATSolver::addCountingHelperClause(int c[], int sz){

}

SATSolver* SATSolver::solverCreate(SolverType t, SolverMode m, string name){
		SATSolver* solver = NULL;
//		if( t ==  ABC ){
//      		solver = new ABCSATSolver(name, ABCSATSolver::FULL, m);
//      		cout<<name<<" = ABC"<<endl;      		
//      	}else if ( t ==  ABCLIGHT ){
//      		solver = new ABCSATSolver(name, ABCSATSolver::BASICSAT, m);
//      		cout<<name<<" = ABC LIGHT"<<endl;
//      	}else if( t == ZCHAFF){
//      		solver = new ZchaffSATSolver(name, m);
//     		cout<<name<<" = ZCHAFF"<<endl;
//      	}else if( t ==  MINI){
      		solver = new MiniSATSolver(name, m);			     		
//      	}
		return solver;
	}
	
