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
	SAT_Manager mng;
public:
	 ZchaffSATSolver(const string& name_p):SATSolver(name_p){
		mng = SAT_InitManager();
		SAT_SetNumVariables(mng, 0);
		FileOutput( string nm = name; nm += ".circuit"; );
		FileOutput( output.open(nm.c_str()) );		
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



void SAT_AddClauseSigned(SAT_Manager          mng,
                   int *                clause_lits,
                   int                  num_lits,
                   int                  gid = 0);


#endif
