#include <iostream>
#include <fstream>
#include <cstdlib>
#include <cstdio>

#include <set>
#include <vector>
#include <dirent.h>
#include "SAT.h"

int main(int argc, char ** argv)
{
	SAT_Manager mng = SAT_InitManager();	
	

	
	// in = 0;
	// t1' = 1;
	// t1 = 2;
	// t2 = 3;

	int N = 3;
	int IN = 0*N;
	int T1p = 1*N;
	int T1 = 2*N;
	int T2 = 3*N;
	int C = 4*N;

	SAT_SetNumVariables(mng, 5*N);
	
	
	// t1[0] = in[0] & c[0];
	// (-t1[0], in[0])(-t1[0], c[0])(t1[0], -in[0], -c[0])
	
	{ int tmp[] = { -(T1+0), (IN+0)}; SAT_AddClause(mng, tmp, 2);}
	{ int tmp[] = { -(T1+0), (C+0)}; SAT_AddClause(mng, tmp, 2);}
	{ int tmp[] = { (T1+0), -(IN+0), -(C+0)}; SAT_AddClause(mng, tmp, 3);}	
	
	SAT_Solve(mng);
	
	return 0;

}

int maon(int argc, char ** argv)
{
	SAT_Manager mng = SAT_InitManager();
	

	
	// in = 0;
	// t1' = 1;
	// t1 = 2;
	// t2 = 3;

	int N = 3;
	int IN = 0*N;
	int T1p = 1*N;
	int T1 = 2*N;
	int T2 = 3*N;
	int C = 4*N;

	SAT_SetNumVariables(mng, 5*N);
	
	
	// t1[0] = in[0] & c[0];
	// (-t1[0], in[0])(-t1[0], c[0])(t1[0], -in[0], -c[0])
	
	{ int tmp[] = { -(T1+0), (IN+0)}; SAT_AddClause(mng, tmp, 2);}
	{ int tmp[] = { -(T1+0), (C+0)}; SAT_AddClause(mng, tmp, 2);}
	{ int tmp[] = { (T1+0), -(IN+0), -(C+0)}; SAT_AddClause(mng, tmp, 3);}
	
	// t1'[i] = in[i] xor in[i-1];
	// (-t1'[i], -in[i], -in[i-1])(-t1'[i], in[i], in[i-1])(t1'[i], -in[i], in[i-1])(t1'[i], in[i], -in[i-1])
	for(int i=1; i<N; ++i){
		{ int tmp[] = { -(T1p+i +1), -(IN+i +1), -(IN+i-1 +1) }; SAT_AddClause(mng, tmp, 3);}
		{ int tmp[] = { -(T1p+i +1), (IN+i +1), (IN+i-1 +1) }; SAT_AddClause(mng, tmp, 3);}
		{ int tmp[] = { (T1p+i +1), -(IN+i +1), (IN+i-1 +1) }; SAT_AddClause(mng, tmp, 3);}
		{ int tmp[] = { (T1p+i +1), (IN+i +1), -(IN+i-1 +1) }; SAT_AddClause(mng, tmp, 3);}					
	}
	// t1[i] = t1p[i] & c[i]
	for(int i=1; i<N; ++i){
		{ int tmp[] = { -(T1+i +1), (T1p+i +1)}; SAT_AddClause(mng, tmp, 2);}
		{ int tmp[] = { -(T1+i +1), (C+i +1)}; SAT_AddClause(mng, tmp, 2);}
		{ int tmp[] = { (T1+i +1), -(T1p+i +1), -(C+i +1)}; SAT_AddClause(mng, tmp, 3);}
	}
	
	
	// t2[i] = t1[i] xor t1[i+1]
	for(int i=0; i<N-1; ++i){
		{ int tmp[] = { -(T2+i +1), -(T1+i +1), -(T1+i+1 +1) }; SAT_AddClause(mng, tmp, 3);}
		{ int tmp[] = { -(T2+i +1), (T1+i +1), (T1+i+1 +1) }; SAT_AddClause(mng, tmp, 3);}
		{ int tmp[] = { (T2+i +1), -(T1+i +1), (T1+i+1 +1) }; SAT_AddClause(mng, tmp, 3);}
		{ int tmp[] = { (T2+i +1), (T1+i +1), -(T1+i+1 +1) }; SAT_AddClause(mng, tmp, 3);}
	}
		{ int tmp[] = { (T2+0), -(T1+0)}; SAT_AddClause(mng, tmp, 2);}
		{ int tmp[] = { -(T2+0), (T1+0)}; SAT_AddClause(mng, tmp, 2);}
	
	
	// in[i] = t2[i] xor in[i];
	// in[i+2] = t2[i+1] xor in[i+1];
	// in[i+1] = t2[i+2] xor in[i+2];
	for(int i=0; i<N/3; i+=3){
		{ int tmp[] = { -(IN+i+0 +1), -(T2+i+0 +1), -(IN+i+0 +1) }; SAT_AddClause(mng, tmp, 3);}
		{ int tmp[] = { -(IN+i+0 +1), (T2+i+0 +1), (IN+i+0 +1) }; SAT_AddClause(mng, tmp, 3);}
		{ int tmp[] = { (IN+i+0 +1), -(T2+i+0 +1), (IN+i+0 +1) }; SAT_AddClause(mng, tmp, 3);}
		{ int tmp[] = { (IN+i+0 +1), (T2+i+0 +1), -(IN+i+0 +1) }; SAT_AddClause(mng, tmp, 3);}
		
		{ int tmp[] = { -(IN+i+2 +1), -(T2+i+1 +1), -(IN+i+1 +1) }; SAT_AddClause(mng, tmp, 3);}
		{ int tmp[] = { -(IN+i+2 +1), (T2+i+1 +1), (IN+i+1 +1) }; SAT_AddClause(mng, tmp, 3);}
		{ int tmp[] = { (IN+i+2 +1), -(T2+i+1 +1), (IN+i+1 +1) }; SAT_AddClause(mng, tmp, 3);}
		{ int tmp[] = { (IN+i+2 +1), (T2+i+1 +1), -(IN+i+1 +1) }; SAT_AddClause(mng, tmp, 3);}
		
		{ int tmp[] = { -(IN+i+1 +1), -(T2+i+2 +1), -(IN+i+2 +1) }; SAT_AddClause(mng, tmp, 3);}
		{ int tmp[] = { -(IN+i+1 +1), (T2+i+2 +1), (IN+i+2 +1) }; SAT_AddClause(mng, tmp, 3);}
		{ int tmp[] = { (IN+i+1 +1), -(T2+i+2 +1), (IN+i+2 +1) }; SAT_AddClause(mng, tmp, 3);}
		{ int tmp[] = { (IN+i+1 +1), (T2+i+2 +1), -(IN+i+2 +1) }; SAT_AddClause(mng, tmp, 3);}
	}
	
	SAT_Solve(mng);
	
	return 0;
}

