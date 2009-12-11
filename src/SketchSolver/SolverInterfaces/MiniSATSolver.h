#ifndef MINISATSOLVER_H
#define MINISATSOLVER_H

#include "SATSolver.h"
#include <iostream>
using namespace std;

#include "MSolver.h"

using namespace MSsolverNS;

inline void MiniSolverStart(){ cout<<" STARTING SAT "<<endl; }
inline void MiniSolverEnd(){cout<<" ENDING SAT"<<endl;  }



class MiniSATSolver : public SATSolver{
protected:
	vector<int> finalOr;
	Solver* s;
	void addClause(int tmp[], int sz, MSsolverNS::vec<Lit>& lits);
	int clauseCount;
public:
	 MiniSATSolver(const string& name_p,  SolverMode smode):SATSolver(name_p, smode){
	 	s = new Solver();
		FileOutput( string nm = name; nm += ".circuit"; );
		FileOutput( output.open(nm.c_str()) );	
		s->newVar();
		clauseCount=0;
	 }
	 virtual void addHelperClause(int c[], int sz){
		MSsolverNS::vec<Lit> lits;
		addClause(c, sz, lits);
	 }

	 virtual int isValKnown(int i){
		 if(s->value(i) != MSsolverNS::l_Undef){
			 return (s->value(i)==MSsolverNS::l_True) ? 1 : -1;
		 }
		 return 0; 
	 }
	 virtual ~MiniSATSolver();
	 virtual void annotate(const string& msg);
	 virtual void annotateInput(const string& name, int i, int sz);
	 virtual void addChoiceClause(int x, int a, int b, int c);
	 virtual void addXorClause(int x, int a, int b);
	 virtual void addOrClause(int x, int a, int b);
	 virtual void addBigOrClause(int* a, int size);
	 virtual void addAndClause(int x, int a, int b);
	 virtual void addEqualsClause(int x, int a);
	 virtual void addEquateClause(int x, int a);
	 virtual void setVarClause(int x);
     virtual void assertVarClause(int x);
	 virtual void hardAssertVarClause(int x);
	 
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
