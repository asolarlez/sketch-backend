#ifndef MINISATSOLVER_H
#define MINISATSOLVER_H

#include "SATSolver.h"
#include <iostream>
using namespace std;

#include "MiniSat_submission/Solver.h"

using namespace MSsolverNS;

inline void MiniSolverStart(){ cout<<" STARTING SAT "<<endl; }
inline void MiniSolverEnd(){cout<<" ENDING SAT"<<endl;  }



class MiniSATSolver : public SATSolver{
protected:
	Solver* s;
	void addClause(int tmp[], int sz, MSsolverNS::vec<Lit>& lits);
public:
	 MiniSATSolver(const string& name_p):SATSolver(name_p){
	 	s = new Solver();
		FileOutput( string nm = name; nm += ".circuit"; );
		FileOutput( output.open(nm.c_str()) );	
		s->newVar();
	 }
	 
	 virtual void annotate(const string& msg);
	 virtual void annotateInput(const string& name, int i, int sz);
	 virtual void addChoiceClause(int x, int a, int b, int c, int gid=0);
	 virtual void addXorClause(int x, int a, int b, int gid=0);
	 virtual void addOrClause(int x, int a, int b, int gid=0);
	 virtual void addBigOrClause(int* a, int size, int gid=0);
	 virtual void addAndClause(int x, int a, int b, int gid=0);
	 virtual void addEqualsClause(int x, int a, int gid=0);
	 virtual void addEquateClause(int x, int a, int gid=0);
	 virtual void setVarClause(int x, int gid=0);
     virtual void assertVarClause(int x, int gid=0);
	 
	 virtual int getVarVal(int id);
	 
	 virtual int newVar();
	 
	 virtual int newInVar();
	 virtual void disableVarBranch(int i);
	 
	 virtual bool ignoreOld();
	 
	 virtual void deleteClauseGroup(int i);
	 virtual int solve();
	
	 virtual void reset();
	 virtual void cleanupDatabase();
	
	 virtual void clean();	
	 virtual void printDiagnostics(char c);
	 
};

#endif
