#ifndef SATSOLVER_H
#define SATSOLVER_H

#include <iostream>

using namespace std;

#include "BasicError.h"

#define dout(out)  /* (cout << "[" << __FUNCTION__ << ":" << __LINE__ << "] " << out << endl); */
#define CheckRepeats( AR, N) /* for(int _i=0; _i<N; ++_i){ for(int _j=_i+1; _j<N; ++_j){ Assert( (AR[_i])/2 != (AR[_j])/2, "REPEAT ENTRY IN CLAUSE "<<_i<<"  "<<_j<<"  "<<AR[_i] ); } } */
#define FileOutput( out ) /* out */


class SATSolver {	

protected:
    string name;
    FileOutput( ofstream output );
    
    /**
     * If this field is true, then instead of solving the problem that
     * makes all asserts and equates true, we solve the problem that makes
     * some assert or equate false.
     */
    const bool solveNegation;
public:
    enum SATSolverResult{
	UNDETERMINED,
	UNSATISFIABLE,
	SATISFIABLE,
	TIME_OUT,
	MEM_OUT,
	ABORTED
    };

    SATSolver(const string& name_p, bool solveNegation_p):name(name_p), solveNegation(solveNegation_p){		
	FileOutput( string nm = name; nm += ".circuit"; );
	FileOutput( output.open(nm.c_str()) );		
    }

    virtual void annotate(const string& msg)=0;
    virtual void annotateInput(const string& name, int i, int sz)=0;
    virtual void addChoiceClause(int x, int a, int b, int c)=0;
    virtual void addXorClause(int x, int a, int b)=0;
    virtual void addOrClause(int x, int a, int b)=0;
    virtual void addBigOrClause(int* a, int size)=0;
    virtual void addAndClause(int x, int a, int b)=0;
    virtual void addEqualsClause(int x, int a)=0;
    virtual void addEquateClause(int x, int a)=0;
    virtual void setVarClause(int x)=0;
    virtual void assertVarClause(int x)=0;
    
    /**
     * The difference between assertVarClause and hardAssertVarClause is as follows.
     * 
     * assertVarClause expresses a property that the user expects to be true. 
     * This means that when doing verification, the solver looks for a counterexample
     * that either violates the EquateClauses, or violates the assert clauses. 
     * 
     * By contrast, a hard assert asserts something that we KNOW is always true no matter
     * what. We don't trust users to provide these, but they are used internally by 
     * the compiler to generate some redundant clauses that could speed up the search. 
     * 
     * 
    */
    virtual void hardAssertVarClause(int x)=0;

    virtual int getVarVal(int id)=0;
    virtual int newVar()=0;

    virtual int newInVar()=0;
    virtual void disableVarBranch(int i)=0;

    virtual bool ignoreOld()=0;

    virtual void deleteClauseGroup(int i)=0;
    virtual int solve()=0;

    virtual void reset()=0;
    virtual void cleanupDatabase()=0;

    virtual void clean()=0;	
    virtual void printDiagnostics(char c)=0;	 
};


#endif
