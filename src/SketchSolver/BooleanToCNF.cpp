#include <fstream>
#include <iostream>
#include <vector>
#include <queue>
#include <set>
#include <map>
using namespace std;

#include "BooleanToCNF.h"
#include "zchaff_solver.h"
#include "zchaff_clsgen.h"

#ifndef SAT_Manager
#define SAT_Manager void *
#endif

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




int assertVectorsDiffer(SAT_Manager mng, varDir& dir, int v1, int v2, int size){
	int N = size;
	int startIdx = dir.getVarCnt();
	// tmp[i] = SOUT[i] != OUT[i];
	for(int i=0; i<N; ++i){
		addXorClause(mng, dir.newAnonymousVar(), v1+i, v2+i);
	}
	int finIdx = dir.getVarCnt();
	// tmp2[0] = tmp[0];
	addEqualsClause(mng, dir.newAnonymousVar(), startIdx);
	
	// tmp2[i] = tmp2[i-1] | tmp[i]
	for(int i=1; i<N; ++i){
		Assert( dir.getVarCnt() == finIdx + i, "THIS SHOULD NOT HAPPEN");
		addOrClause(mng, dir.newAnonymousVar() , finIdx + i-1, startIdx+i);
	}	
	// assert tmp2[N-1];
	return finIdx + N-1;
}


int select(SAT_Manager mng, varDir& dir, int choices[], int control, int nchoices, int bitsPerChoice){
	int outvar = dir.getVarCnt();	
	for(int i=0; i<bitsPerChoice; ++i){
		dir.newAnonymousVar();
	}	
	for(int j=0; j<bitsPerChoice; ++j){
		setVarClause(mng, -(dir.newAnonymousVar()));
		for(int i=0; i<nchoices; ++i){
			int cvar = dir.newAnonymousVar();
			addAndClause(mng, cvar, control+i, choices[i]+j);
			int cvar2 = dir.newAnonymousVar();
			addOrClause(mng, cvar2, cvar, cvar-1);
		}
		addEqualsClause(mng, outvar+j, dir.getVarCnt()-1);
	}
	return outvar;
}


int selectMinGood(SAT_Manager mng, varDir& dir, int choices[], int control, int nchoices, int bitsPerChoice){
	int outvar = select(mng, dir, choices, control, nchoices, bitsPerChoice);	
	int differences = dir.getVarCnt();	
	int prev = dir.newAnonymousVar();
	setVarClause(mng, -prev);
	for(int i=1; i<nchoices; ++i){
		int different = assertVectorsDiffer(mng, dir, choices[i-1], choices[i], bitsPerChoice);
		int cvar = dir.newAnonymousVar();
		addAndClause(mng, cvar, control+i, different);
		int cvar2 = dir.newAnonymousVar();
		addOrClause(mng, cvar2, cvar, prev);
		prev = cvar2;
	}
	setVarClause(mng, -prev);	
	return outvar;
}




int arbitraryPerm(SAT_Manager mng, varDir& dir, int input, int insize, int controls[], int ncontrols, int csize){
	// ncontrols = sizeof(controls);
	Assert( insize <= csize, "This is an error");
	int OUTSIZE = ncontrols;
	int outvar = dir.getVarCnt(); for(int i=0; i<OUTSIZE; ++i){ dir.newAnonymousVar(); };
	for(int i=0; i<OUTSIZE; ++i){
		int cvarPrev = dir.newAnonymousVar();
		setVarClause(mng, -cvarPrev);
		for(int j=0; j<insize; ++j){
			int cvar = dir.newAnonymousVar();
			addAndClause(mng, cvar, controls[i]+j, input+j);
			int cvar2 = dir.newAnonymousVar();
			addOrClause(mng, cvar2, cvar, cvarPrev);
			cvarPrev = cvar2;
		}
		addEqualsClause(mng, outvar+i, cvarPrev);
	}
	return outvar;
}



