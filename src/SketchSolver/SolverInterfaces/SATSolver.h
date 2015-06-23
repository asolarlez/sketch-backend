#ifndef SATSOLVER_H
#define SATSOLVER_H

#include <iostream>
#include "Vec.h"
#include "SolverTypes.h"

using namespace std;
using namespace MSsolverNS;

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
	bool isNegated(){
		return solveNegation;
	}
	typedef enum {ABC, ABCLIGHT, ZCHAFF, MINI} SolverType;
	typedef enum {FINDER, CHECKER} SolverMode;
	
	static SATSolver* solverCreate(SolverType t, SolverMode m, string name);

	virtual bool isOK()=0;
	virtual bool assertIfPossible(int a)=0;

    enum SATSolverResult{
	UNDETERMINED,
	UNSATISFIABLE,
	SATISFIABLE,
	TIME_OUT,
	MEM_OUT,
	ABORTED
    };

    SATSolver(const string& name_p, SolverMode smode):name(name_p), solveNegation(smode==CHECKER){		
	FileOutput( string nm = name; nm += ".circuit"; );
	FileOutput( output.open(nm.c_str()) );		
    }
	virtual ~SATSolver(){};
    virtual void annotate(const string& msg)=0;
    virtual void annotateInput(const string& name, int i, int sz)=0;
    virtual void addChoiceClause(int x, int a, int b, int c)=0;
    virtual void addXorClause(int x, int a, int b)=0;
    
	// virtual void addOrClause(int x, int a, int b)=0;

    virtual void addBigOrClause(int* a, int size)=0;
    virtual void addAndClause(int x, int a, int b)=0;
    virtual void addEqualsClause(int x, int a)=0;
    virtual void addEquateClause(int x, int a)=0;
    virtual void setVarClause(int x)=0;
    virtual void assertVarClause(int x)=0;
	virtual void hardAssertVarClause(int x)=0;
	virtual void assumeVarClause(int x)=0;
	virtual void retractableAssertClause(int x)=0;
	virtual void outputSAT()=0;
	virtual void addHelperClause(int c[], int size)=0;
	virtual void addHelperClause(vec<Lit>& vl)=0;
	virtual int isValKnown(int i){return 0; }
	virtual void addCountingHelperClause(int c[], int sz);
	virtual void addExPairConstraint(int* pairs, int npairs, int out)=0;
	virtual void addHelper2Clause(int l1, int l2){}
    virtual int getVarVal(int id)=0;
    virtual int newVar()=0;

    virtual int newInVar()=0;
    virtual void disableVarBranch(int i)=0;

	virtual void markInput(int id)=0;

    virtual bool ignoreOld()=0;

    
    virtual int solve()=0;

    virtual void reset()=0;
    virtual void retractAssumptions()=0;
	virtual bool tryAssignment(int a)=0;
    virtual void clean()=0;	
    virtual void printDiagnostics(char c)=0;	 
	virtual void lightSolve();
	virtual void writeDIMACS(ofstream& dimacs_file)=0;

};


#endif
