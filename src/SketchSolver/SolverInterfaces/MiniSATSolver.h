#ifndef MINISATSOLVER_H
#define MINISATSOLVER_H

#include "SATSolver.h"
#include <vector>
#include <iostream>
using namespace std;

#include "MSolver.h"
#include "SolverTypes.h"
#include "Vec.h"


using namespace MSsolverNS;

inline void MiniSolverStart(){ cout<<" STARTING SAT "<<endl; }
inline void MiniSolverEnd(){cout<<" ENDING SAT"<<endl;  }



class MiniSATSolver : public SATSolver{
protected:
	int solveCount;
	bool outputProblems;
	vector<int> finalOr;
	vec<Lit> assumptions;
	Solver* s;
	void addClause(int tmp[], int sz, MSsolverNS::vec<Lit>& lits);
	int clauseCount;
	bool lsolve;
public:
	ostream* debugout;
	 MiniSATSolver(const string& name_p,  SolverMode smode):SATSolver(name_p, smode){
	 	s = new Solver();
		if(isNegated()){ s->polarity_mode = Solver::polarity_false; }	
		FileOutput( string nm = name; nm += ".circuit"; );
		FileOutput( output.open(nm.c_str()) );	
		s->newVar();
		clauseCount=0;
		debugout = NULL;
		lsolve = false;
		solveCount = 0;
		outputProblems = false;

	 }
	 virtual void outputSAT(){
		 cout<<"Outputing problems"<<endl;
		 outputProblems = true;
	 }
	 virtual void addHelperClause(int c[], int sz);

	 virtual int isValKnown(int i){
		 int var = abs(i);
		 int rv = 1;
		 if(i<0){
			 rv = -1;
		 }
		 if(s->value(var) != MSsolverNS::l_Undef){
			 return (s->value(var)==MSsolverNS::l_True) ? rv : -rv;
		 }
		 return 0; 
	 }
	 virtual bool isOK();
	 virtual bool assertIfPossible(int a);
	 virtual ~MiniSATSolver();
	 virtual void annotate(const string& msg);
	 virtual void annotateInput(const string& name, int i, int sz);
	 virtual void addChoiceClause(int x, int a, int b, int c);
	 virtual void addXorClause(int x, int a, int b);
	 // virtual void addOrClause(int x, int a, int b);
	 virtual void addBigOrClause(int* a, int size);
	 virtual void addAndClause(int x, int a, int b);
	 virtual void addEqualsClause(int x, int a);
	 virtual void addEquateClause(int x, int a);
	 virtual void setVarClause(int x);
     virtual void assertVarClause(int x);
	 virtual void assumeVarClause(int x);

	 virtual void addHelper2Clause(int l1, int l2);
	 virtual void addHelperClause(vec<Lit>& vl);

	 virtual bool tryAssignment(int a){
		 return s->tryAssignment(lfromInt(a));
	 }

	 virtual void retractableAssertClause(int x);
	 void addCountingHelperClause(int c[], int sz);

	 virtual void addExPairConstraint(int* pairs, int npairs, int out);

	 virtual void hardAssertVarClause(int x);
	 
	 virtual void markInput(int id);

	 virtual int getVarVal(int id);
	 
	 virtual int newVar();
	 
	 virtual int newInVar();
	 virtual void disableVarBranch(int i);
	 
	 virtual bool ignoreOld();
	 
	 virtual int solve();
	
	 virtual void reset();
	 virtual void retractAssumptions();
	
	 virtual void finish(){
		if(solveNegation){
			MSsolverNS::vec<Lit> lits;
			addClause(finalOr.size() > 0 ? (&finalOr[0]) : NULL  , finalOr.size(), lits);
		}
	 }
	 virtual void clean();	
	 virtual void printDiagnostics(char c);
	 virtual void lightSolve();
	 virtual void writeDIMACS(ofstream& dimacs_file){
		 s->writeDIMACS(dimacs_file);
	 }

	 void dump(){
		 s->dump();
	 }
	 
};



#endif
