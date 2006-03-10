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



int assertVectorsDiffer(SATSolver& mng, varDir& dir, int v1, int v2, int size){
	int N = size;
	int lastone = 0;
	for(int i=0; i<N; ++i){		
		int cur = dir.addXorClause(v1+i, v2+i);
		if(lastone != 0){
			lastone = dir.addOrClause(lastone, cur);
		}else{
			lastone = cur;
		}		
	}
	return lastone;
}


int select(SATSolver& mng, varDir& dir, int choices[], int control, int nchoices, int bitsPerChoice){
	int outvar = dir.getVarCnt();	
	for(int i=0; i<bitsPerChoice; ++i){
		dir.newAnonymousVar();
	}	
	for(int j=0; j<bitsPerChoice; ++j){
		mng.setVarClause( -(dir.newAnonymousVar()));
		for(int i=0; i<nchoices; ++i){
			int cvar = dir.newAnonymousVar();
			mng.addAndClause( cvar, control+i, choices[i]+j);
			int cvar2 = dir.newAnonymousVar();
			mng.addOrClause( cvar2, cvar, cvar-1);
		}
		mng.addEqualsClause( outvar+j, dir.getVarCnt()-1);
	}
	return outvar;
}


int selectMinGood(SATSolver& mng, varDir& dir, int choices[], int control, int nchoices, int bitsPerChoice){
	int outvar = select(mng, dir, choices, control, nchoices, bitsPerChoice);	
	int differences = dir.getVarCnt();	
	int prev = dir.newAnonymousVar();
	mng.setVarClause(-prev);
	for(int i=1; i<nchoices; ++i){
		int different = assertVectorsDiffer(mng, dir, choices[i-1], choices[i], bitsPerChoice);
		int cvar = dir.newAnonymousVar();
		mng.addAndClause( cvar, control+i, different);
		int cvar2 = dir.newAnonymousVar();
		mng.addOrClause( cvar2, cvar, prev);
		prev = cvar2;
	}
	mng.setVarClause( -prev);	
	return outvar;
}




int arbitraryPerm(SATSolver& mng, varDir& dir, int input, int insize, int controls[], int ncontrols, int csize){
	// ncontrols = sizeof(controls);
	Assert( insize <= csize, "This is an error");
	int OUTSIZE = ncontrols;
	int outvar = dir.getVarCnt(); for(int i=0; i<OUTSIZE; ++i){ dir.newAnonymousVar(); };
	for(int i=0; i<OUTSIZE; ++i){
		int cvarPrev = dir.newAnonymousVar();
		mng.setVarClause( -cvarPrev);
		for(int j=0; j<insize; ++j){
			int cvar = dir.newAnonymousVar();
			mng.addAndClause( cvar, controls[i]+j, input+j);
			int cvar2 = dir.newAnonymousVar();
			mng.addOrClause( cvar2, cvar, cvarPrev);
			cvarPrev = cvar2;
		}
		mng.addEqualsClause( outvar+i, cvarPrev);
	}
	return outvar;
}



