

#include <iostream>
#include <fstream>
#include <cstdlib>
#include <cstdio>
#include <map>
#include <string>
#include <set>
#include <vector>
#include <dirent.h>
#include <queue>
#include "ZchaffSATSolver.h"
#include "zchaff_solver.h"
#include "zchaff_clsgen.h"






 void ZchaffSATSolver::annotate(const string& msg){
	Dout( cout<<msg );
	FileOutput(output<<msg<<endl);
}

 void ZchaffSATSolver::annotateInput(const string& name, int i, int sz){
	Dout( cout<<"x "<<name<<" ");
	FileOutput(output<<"x "<<name<<" ");
	for(int t=0; t<sz; ++t){
		Dout( cout<<(i+t)<<" ");
		FileOutput(output<<(i+t)<<" ");
	}
	Dout(cout<<endl);
	FileOutput(output<<endl);
}

//This function encodes x == a ? b:c;
 void ZchaffSATSolver::addChoiceClause(int x, int a, int b, int c, int gid){
	Dout( cout<<" "<<x<<"= "<<a<<" ? "<<b<<":"<<c<<";"<<endl );
	FileOutput( output<<x<<" CHOICE "<<a<<" "<<b<<" "<<c<<endl );
	{ int tmp[] = { -(x), -(a), (b) }; SAT_AddClauseSigned(mng, tmp, 3, gid); 	CheckRepeats(tmp, 3);}
	{ int tmp[] = { -(x), (c), (a) }; SAT_AddClauseSigned(mng, tmp, 3, gid); 	CheckRepeats(tmp, 3);}
	{ int tmp[] = { -(x), (c), (b) }; SAT_AddClauseSigned(mng, tmp, 3, gid);	CheckRepeats(tmp, 3);}
	{ int tmp[] = { (x), -(c), -(b) }; SAT_AddClauseSigned(mng, tmp, 3, gid);	CheckRepeats(tmp, 3);}
	{ int tmp[] = { (x), (a), -(c) }; SAT_AddClauseSigned(mng, tmp, 3, gid);}
	{ int tmp[] = { (x), -(a), -(b) }; SAT_AddClauseSigned(mng, tmp, 3, gid);}
}


//This function encodes x == a xor b;
 void ZchaffSATSolver::addXorClause(int x, int a, int b, int gid){
	Dout( cout<<" "<<x<<"= "<<a<<" xor "<<b<<"; "<<endl );
	FileOutput( output<<x<<" XOR "<<a<<" "<<b<<endl );
	{ int tmp[] = { -(x), -(a), -(b) }; SAT_AddClauseSigned(mng, tmp, 3, gid); 	CheckRepeats(tmp, 3);}
	{ int tmp[] = { -(x), (a), (b) }; SAT_AddClauseSigned(mng, tmp, 3, gid);}
	{ int tmp[] = { (x), -(a), (b) }; SAT_AddClauseSigned(mng, tmp, 3, gid);}
	{ int tmp[] = { (x), (a), -(b) }; SAT_AddClauseSigned(mng, tmp, 3, gid);}
}

//This function encodes x == a or b;
 void ZchaffSATSolver::addOrClause(int x, int a, int b, int gid){
	Dout( cout<<" "<<x<<"= "<<a<<" or "<<b<<"; "<<endl );
	FileOutput( output<<x<<" OR "<<a<<" "<<b<<endl );
	{ int tmp[] = { (x), -(a)}; SAT_AddClauseSigned(mng, tmp, 2, gid);}
	{ int tmp[] = { (x), -(b)}; SAT_AddClauseSigned(mng, tmp, 2, gid);}
	{ int tmp[] = { -(x), (a), (b)}; SAT_AddClauseSigned(mng, tmp, 3, gid);	CheckRepeats(tmp, 3);}	
}


//This function encodes a[0] == a[1] or a[2] or ... a[size];
 void ZchaffSATSolver::addBigOrClause(int* a, int size, int gid){
	Dout( cout<<" "<<a[0]<<"= " );
	FileOutput( output<<a[0]<<" BOR "<<size<<" " );
	for(int i=0; i<size; ++i){
		Dout(cout<<a[i+1]<<" or ");
		{ int tmp[] = { (a[0]), -(a[i+1])}; SAT_AddClauseSigned(mng, tmp, 2, gid);}
		FileOutput( output<<a[i+1]<<" " );
	}
	FileOutput( output<<endl );
	Dout(cout<<"; "<<endl);
	a[0] = -a[0];
	{SAT_AddClauseSigned(mng, a, size+1, gid); 	CheckRepeats(a, size+1);}
}


//This function encodes x == a and b;
 void ZchaffSATSolver::addAndClause(int x, int a, int b, int gid){
	Dout( cout<<" "<<x<<"= "<<a<<" and "<<b<<"; "<<endl );
	FileOutput( output<<x<<" AND "<<a<<" "<<b<<endl );
	{ int tmp[] = { -(x), (a)}; SAT_AddClauseSigned(mng, tmp, 2, gid);}
	{ int tmp[] = { -(x), (b)}; SAT_AddClauseSigned(mng, tmp, 2, gid);}
	{ int tmp[] = { (x), -(a), -(b)}; SAT_AddClauseSigned(mng, tmp, 3, gid); 	CheckRepeats(tmp, 3);}
}

//This function encodes x = a;
 void ZchaffSATSolver::addEqualsClause(int x, int a, int gid){
	Dout( cout<<" "<<x<<"= "<<a<<"; "<<flush<<endl );
	FileOutput( output<<x<<" EQ "<<a<<endl );
	{ int tmp[] = { -(x), (a)}; SAT_AddClauseSigned(mng, tmp, 2, gid);}
	{ int tmp[] = { (x), -(a)}; SAT_AddClauseSigned(mng, tmp, 2, gid); 	CheckRepeats(tmp, 2);}
}


//This function encodes x == a;
 void ZchaffSATSolver::addEquateClause(int x, int a, int gid){
	Dout( cout<<" "<<x<<"= "<<a<<"; "<<flush<<endl );
	FileOutput( output<<"x OUTXOR "<<x<<" "<<-a<<endl );
	{ int tmp[] = { -(x), (a)}; SAT_AddClauseSigned(mng, tmp, 2, gid);}
	{ int tmp[] = { (x), -(a)}; SAT_AddClauseSigned(mng, tmp, 2, gid); 	CheckRepeats(tmp, 2);}
}


 void ZchaffSATSolver::setVarClause(int x, int gid){
	Dout( cout<<" set "<<x<<";"<<endl );
	FileOutput( output<<"x SET "<<x<<" ;"<<endl );
	{ int tmp[] = { (x)}; SAT_AddClauseSigned(mng, tmp, 1, gid);}
}


 void ZchaffSATSolver::assertVarClause(int x, int gid){
	Dout( cout<<" assert "<<x<<";"<<endl );
	FileOutput( output<<"x OUTASSERT "<<x<<" ;"<<endl );
	{ int tmp[] = { (x)}; SAT_AddClauseSigned(mng, tmp, 1, gid);}
}

 void ZchaffSATSolver::printDiagnostics(char c){
    cout << c << "# Random Seed Used\t\t\t\t" << SAT_Random_Seed(mng) << endl;
    cout << c << "# Max Decision Level\t\t\t\t" << SAT_MaxDLevel(mng) << endl;
    cout << c << "# Num. of Decisions\t\t\t\t" << SAT_NumDecisions(mng)<< endl;
    cout << c << "# ( Stack + Vsids + Shrinking Decisions )\t\t" <<SAT_NumDecisionsStackConf(mng);
    cout << c << "#  + " <<SAT_NumDecisionsVsids(mng)<<" + "<<SAT_NumDecisionsShrinking(mng)<<endl;
    cout << c << "# Original Num Variables\t\t\t\t" << SAT_NumVariables(mng) << endl;
    cout << c << "# Original Num Clauses\t\t\t\t" << SAT_InitNumClauses(mng) << endl;
    cout << c << "# Original Num Literals\t\t\t\t" << SAT_InitNumLiterals(mng) << endl;
    cout << c << "# Added Conflict Clauses\t\t\t\t" << SAT_NumAddedClauses(mng)- SAT_InitNumClauses(mng)<< endl;
    cout << c << "# Num of Shrinkings\t\t\t\t" << SAT_NumShrinkings(mng)<< endl;
    cout << c << "# Deleted Conflict Clauses\t\t\t" << SAT_NumDeletedClauses(mng)-SAT_NumDelOrigCls(mng) <<endl;
    cout << c << "# Deleted Clauses\t\t\t\t\t" << SAT_NumDeletedClauses(mng) <<endl;
    cout << c << "# Added Conflict Literals\t\t\t\t" << SAT_NumAddedLiterals(mng) - SAT_InitNumLiterals(mng) << endl;
    cout << c << "# Deleted (Total) Literals\t\t\t" << SAT_NumDeletedLiterals(mng) <<endl;
    cout << c << "# Number of Implication\t\t\t\t" << SAT_NumImplications(mng)<< endl;
    //other statistics comes here
    cout << c << "# Total Memory\t\t\t\t\t" << SAT_EstimateMemUsage(mng) << endl;	
    cout << c << "# Total Run Time\t\t\t\t\t" << SAT_GetCPUTime(mng) << endl;	
}






 void SAT_AddClauseSigned(SAT_Manager           mng,
                          int *                 clause_lits,
                          int                   num_lits,
                          int                   gid) {
 
 	
  CSolver * solver = (CSolver*) mng;
  int vars = solver->num_variables();
   for(int i=0; i<num_lits; ++i){   
   	int sign=0;	
   	if( clause_lits[i] < 0){ clause_lits[i] = -clause_lits[i]; sign = 1;}
   	if( clause_lits[i]>vars){
  		cout<<" INCORRECT STUFF "<<vars<<"  "<< clause_lits[i]<<"  "<<i<<endl;	
  	}
   	clause_lits[i] = (clause_lits[i] << 1) + sign;
  }

  solver->add_orig_clause(clause_lits, num_lits, gid);
}


int ZchaffSATSolver::getVarVal(int id){
 return SAT_GetVarAsgnment(mng, id);
}
 
 int ZchaffSATSolver::newVar(){
	return SAT_AddVariable(mng);
 }
	 
	  int ZchaffSATSolver::newInVar(){
		return SAT_AddVariable(mng);
	 }
	 	 
	  void ZchaffSATSolver::disableVarBranch(int i){
	 	SAT_DisableVarBranch(mng, i);	
	 }
	 
	  bool ZchaffSATSolver::ignoreOld(){
	 	return false;	
	 }
	 
	 void ZchaffSATSolver::deleteClauseGroup(int i){
		SAT_DeleteClauseGroup(mng, i);
	}
	
	 int ZchaffSATSolver::solve(){
		int result = SAT_Solve(mng);
		return result;
	}
	
	 void ZchaffSATSolver::reset(){
		FileOutput(output<<"#  ======================================="<<endl);
		SAT_Reset(mng);
	}
	
	 void ZchaffSATSolver::cleanupDatabase(){
		SAT_CleanUpDatabase(mng);
	}
	
	 void ZchaffSATSolver::clean(){
		SAT_ReleaseManager(mng);
		mng =  SAT_InitManager();
	}
	