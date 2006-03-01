#ifndef SATSOLVER_H
#define SATSOLVER_H


#include <iostream>
#include "SAT.h"

using namespace std;


#define Assert( in, msg) if(!(in)){cout<<msg<<endl; exit(1); }
#define Dout( out )   /* out */
#define CheckRepeats( AR, N) /* for(int _i=0; _i<N; ++_i){ for(int _j=_i+1; _j<N; ++_j){ Assert( (AR[_i])/2 != (AR[_j])/2, "REPEAT ENTRY IN CLAUSE "<<_i<<"  "<<_j<<"  "<<AR[_i] ); } } */
#define FileOutput( out ) /* out */


class SATSolver{
protected:
	SAT_Manager mng;
	string name;
	FileOutput( ofstream output );
public:
	 SATSolver(const string& name_p):name(name_p){
		mng = SAT_InitManager();
		SAT_SetNumVariables(mng, 0);
		FileOutput( string nm = name; nm += ".circuit"; );
		FileOutput( output.open(nm.c_str()) );		
	 }
	 void annotate(const string& msg);
	 void annotateInput(const string& name, int i, int sz);
	 void addChoiceClause(int x, int a, int b, int c, int gid=0);
	 void addXorClause(int x, int a, int b, int gid=0);
	 void addOrClause(int x, int a, int b, int gid=0);
	 void addBigOrClause(int* a, int size, int gid=0);
	 void addAndClause(int x, int a, int b, int gid=0);
	 void addEqualsClause(int x, int a, int gid=0);
	 void addEquateClause(int x, int a, int gid=0);
	 void setVarClause(int x, int gid=0);
     void assertVarClause(int x, int gid=0);
	 
	 inline int getVarVal(int id){
		 return SAT_GetVarAsgnment(mng, id);
	 }
	 
	 inline int newVar(){
		return SAT_AddVariable(mng);
	 }
	 
	 inline void disableVarBranch(int i){
	 	SAT_DisableVarBranch(mng, i);	
	 }
	 
	inline void deleteClauseGroup(int i){
		SAT_DeleteClauseGroup(mng, i);
	}
	
	inline int solve(){
		int result = SAT_Solve(mng);
		return result;
	}
	
	inline void reset(){
		FileOutput(output<<"#  ======================================="<<endl);
		SAT_Reset(mng);
	}
	
	void printDiagnostics(char c);
};



void SAT_AddClauseSigned(SAT_Manager          mng,
                   int *                clause_lits,
                   int                  num_lits,
                   int                  gid = 0);



inline void SATSolver::annotate(const string& msg){
	Dout( cout<<msg );
	FileOutput(output<<msg<<endl);
}

inline void SATSolver::annotateInput(const string& name, int i, int sz){
	Dout( cout<<"x "<<name<<" ");
	FileOutput(output<<"x "<<name<<" ");
	for(int t=0; t<sz; ++t){
		Dout( cout<<(i+sz)<<" ");
		FileOutput(output<<(i+t)<<" ");
	}
	Dout(cout<<endl);
	FileOutput(output<<endl);
}

//This function encodes x == a ? b:c;
inline void SATSolver::addChoiceClause(int x, int a, int b, int c, int gid){
	Dout( cout<<" "<<x<<"= "<<a<<" ? "<<b<<":"<<c<<";"<<endl );
	FileOutput( output<<x<<" CHOICE "<<a<<" "<<b<<" "<<c<<endl );
	{ int tmp[] = { -(x), -(a), (b) }; SAT_AddClauseSigned(mng, tmp, 3, gid); 	CheckRepeats(tmp, 3);}
	{ int tmp[] = { -(x), (c), (a) }; SAT_AddClauseSigned(mng, tmp, 3, gid); 	CheckRepeats(tmp, 3);}
	{ int tmp[] = { -(x), (c), (b) }; SAT_AddClauseSigned(mng, tmp, 3, gid);	CheckRepeats(tmp, 3);}
	{ int tmp[] = { (x), (a), -(c) }; SAT_AddClauseSigned(mng, tmp, 3, gid);}
	{ int tmp[] = { (x), -(a), -(b) }; SAT_AddClauseSigned(mng, tmp, 3, gid);}
}


//This function encodes x == a xor b;
inline void SATSolver::addXorClause(int x, int a, int b, int gid){
	Dout( cout<<" "<<x<<"= "<<a<<" xor "<<b<<"; "<<endl );
	FileOutput( output<<x<<" XOR "<<a<<" "<<b<<endl );
	{ int tmp[] = { -(x), -(a), -(b) }; SAT_AddClauseSigned(mng, tmp, 3, gid); 	CheckRepeats(tmp, 3);}
	{ int tmp[] = { -(x), (a), (b) }; SAT_AddClauseSigned(mng, tmp, 3, gid);}
	{ int tmp[] = { (x), -(a), (b) }; SAT_AddClauseSigned(mng, tmp, 3, gid);}
	{ int tmp[] = { (x), (a), -(b) }; SAT_AddClauseSigned(mng, tmp, 3, gid);}
}

//This function encodes x == a or b;
inline void SATSolver::addOrClause(int x, int a, int b, int gid){
	Dout( cout<<" "<<x<<"= "<<a<<" or "<<b<<"; "<<endl );
	FileOutput( output<<x<<" OR "<<a<<" "<<b<<endl );
	{ int tmp[] = { (x), -(a)}; SAT_AddClauseSigned(mng, tmp, 2, gid);}
	{ int tmp[] = { (x), -(b)}; SAT_AddClauseSigned(mng, tmp, 2, gid);}
	{ int tmp[] = { -(x), (a), (b)}; SAT_AddClauseSigned(mng, tmp, 3, gid);	CheckRepeats(tmp, 3);}	
}


//This function encodes a[0] == a[1] or a[2] or ... a[size];
inline void SATSolver::addBigOrClause(int* a, int size, int gid){
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
inline void SATSolver::addAndClause(int x, int a, int b, int gid){
	Dout( cout<<" "<<x<<"= "<<a<<" and "<<b<<"; "<<endl );
	FileOutput( output<<x<<" AND "<<a<<" "<<b<<endl );
	{ int tmp[] = { -(x), (a)}; SAT_AddClauseSigned(mng, tmp, 2, gid);}
	{ int tmp[] = { -(x), (b)}; SAT_AddClauseSigned(mng, tmp, 2, gid);}
	{ int tmp[] = { (x), -(a), -(b)}; SAT_AddClauseSigned(mng, tmp, 3, gid); 	CheckRepeats(tmp, 3);}
}

//This function encodes x == a;
inline void SATSolver::addEqualsClause(int x, int a, int gid){
	Dout( cout<<" "<<x<<"= "<<a<<"; "<<flush<<endl );
	FileOutput( output<<x<<" EQ "<<a<<endl );
	{ int tmp[] = { -(x), (a)}; SAT_AddClauseSigned(mng, tmp, 2, gid);}
	{ int tmp[] = { (x), -(a)}; SAT_AddClauseSigned(mng, tmp, 2, gid); 	CheckRepeats(tmp, 2);}
}


//This function encodes x == a;
inline void SATSolver::addEquateClause(int x, int a, int gid){
	Dout( cout<<" "<<x<<"= "<<a<<"; "<<flush<<endl );
	FileOutput( output<<"x OUTXOR "<<x<<" "<<-a<<endl );
	{ int tmp[] = { -(x), (a)}; SAT_AddClauseSigned(mng, tmp, 2, gid);}
	{ int tmp[] = { (x), -(a)}; SAT_AddClauseSigned(mng, tmp, 2, gid); 	CheckRepeats(tmp, 2);}
}


inline void SATSolver::setVarClause(int x, int gid){
	Dout( cout<<" set "<<x<<";"<<endl );
	FileOutput( output<<"x SET "<<x<<" ;"<<endl );
	{ int tmp[] = { (x)}; SAT_AddClauseSigned(mng, tmp, 1, gid);}
}


inline void SATSolver::assertVarClause(int x, int gid){
	Dout( cout<<" assert "<<x<<";"<<endl );
	FileOutput( output<<"x OUTASSERT "<<x<<" ;"<<endl );
	{ int tmp[] = { (x)}; SAT_AddClauseSigned(mng, tmp, 1, gid);}
}

inline void SATSolver::printDiagnostics(char c){
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
    cout << c << "# Total Run Time\t\t\t\t\t" << SAT_GetCPUTime(mng) << endl;	
}


#endif
