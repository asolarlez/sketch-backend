#ifndef ZCHAFFSATSOLVER_H
#define ZCHAFFSATSOLVER_H

#include "SATSolver.h"
#include <iostream>
#include "SAT.h"

using namespace std;


inline void ZSolverStart(){ cout<<" STARTING SAT "<<endl; }
inline void ZSolverEnd(){cout<<" ENDING SAT"<<endl;  }



class ZchaffSATSolver : public SATSolver{
protected:
	vector<int> finalOr;
	SAT_Manager mng;
public:
	 ZchaffSATSolver(const string& name_p, SolverMode smode):SATSolver(name_p, smode){
		mng = SAT_InitManager();
		SAT_SetNumVariables(mng, 0);		
	 }
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



void SAT_AddClauseSigned(SAT_Manager          mng,
                   int *                clause_lits,
                   int                  num_lits);


#endif
