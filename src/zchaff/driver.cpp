#include <iostream>
#include <fstream>
#include <cstdlib>
#include <cstdio>

#include <set>
#include <vector>
#include <dirent.h>
#include "SAT.h"


using namespace std;


class bitSwapSketchCheck{
	int CNT;
	int N;
	int IN ;
	int T1p ;
	int T1;
	int T2;
	int C;
	int T3; 
	int T4;
	int Tout;
	SAT_Manager mng;
	public:
	
	bitSwapSketchCheck(int N_p){
		mng = SAT_InitManager();
		CNT=0;
		N = N_p;
		cout<<"N="<<N;
		C = CNT++*N;
		IN = CNT++*N;
		T1p = CNT++*N;
		T1 = CNT++*N;
		T2 = CNT++*N;
		T3 = CNT++*N;
		T4 = CNT++*N;
		Tout = CNT++*N;
		SAT_SetNumVariables(mng, CNT*N);
	}
	
	bool testControls(int ctrl[], int ctrllen, int inv[]){
		SAT_DeleteClauseGroup(mng, 2);
		for(int i=0; i<N; ++i){
			{ int tmp[] = { (ctrl[i%ctrllen]*(C+i+1)) }; SAT_AddClauseSigned(mng, tmp, 1, 2);}
		}
	    int result = SAT_Solve(mng);
	    if (result != SATISFIABLE) 
	    	return false;
		SAT_Solve(mng);
		for(int i=0; i<N; ++i){
			int val = SAT_GetVarAsgnment(mng, IN+i+1);
			if( val == 1) inv[i]= 1;
			else inv[i]= -1;
		}
		return true;
	}
	
	void setup(){
		
		// t1[0] = in[0] & c[0];
		// (-t1[0], in[0])(-t1[0], c[0])(t1[0], -in[0], -c[0])
		cout<<" t1[0] = in[0] & c[0]; "<<endl;
		{ int tmp[] = { -(T1+0 +1), (IN+0 +1)}; SAT_AddClauseSigned(mng, tmp, 2);}
		{ int tmp[] = { -(T1+0 +1), (C+0 +1)}; SAT_AddClauseSigned(mng, tmp, 2);}
		{ int tmp[] = { (T1+0 +1), -(IN+0 +1), -(C+0 +1)}; SAT_AddClauseSigned(mng, tmp, 3);}
		
		// t1'[i] = in[i] xor in[i-1];
		// (-t1'[i], -in[i], -in[i-1])(-t1'[i], in[i], in[i-1])(t1'[i], -in[i], in[i-1])(t1'[i], in[i], -in[i-1])
		cout<<" t1'[i] = in[i] xor in[i-1]; "<<endl;
		for(int i=1; i<N; ++i){
			{ int tmp[] = { -(T1p+i +1), -(IN+i +1), -(IN+i-1 +1) }; SAT_AddClauseSigned(mng, tmp, 3);}
			{ int tmp[] = { -(T1p+i +1), (IN+i +1), (IN+i-1 +1) }; SAT_AddClauseSigned(mng, tmp, 3);}
			{ int tmp[] = { (T1p+i +1), -(IN+i +1), (IN+i-1 +1) }; SAT_AddClauseSigned(mng, tmp, 3);}
			{ int tmp[] = { (T1p+i +1), (IN+i +1), -(IN+i-1 +1) }; SAT_AddClauseSigned(mng, tmp, 3);}					
		}
		// t1[i] = t1p[i] & c[i]
		cout<<" t1[i] = t1p[i] & c[i] "<<endl;
		for(int i=1; i<N; ++i){
			{ int tmp[] = { -(T1+i +1), (T1p+i +1)}; SAT_AddClauseSigned(mng, tmp, 2);}
			{ int tmp[] = { -(T1+i +1), (C+i +1)}; SAT_AddClauseSigned(mng, tmp, 2);}
			{ int tmp[] = { (T1+i +1), -(T1p+i +1), -(C+i +1)}; SAT_AddClauseSigned(mng, tmp, 3);}
		}
		
		
		// t2[i] = t1[i] xor t1[i+1]
		cout<<" t2[i] = t1[i] xor t1[i+1] "<<endl;
		for(int i=0; i<N-1; ++i){
			{ int tmp[] = { -(T2+i +1), -(T1+i +1), -(T1+i+1 +1) }; SAT_AddClauseSigned(mng, tmp, 3);}
			{ int tmp[] = { -(T2+i +1), (T1+i +1), (T1+i+1 +1) }; SAT_AddClauseSigned(mng, tmp, 3);}
			{ int tmp[] = { (T2+i +1), -(T1+i +1), (T1+i+1 +1) }; SAT_AddClauseSigned(mng, tmp, 3);}
			{ int tmp[] = { (T2+i +1), (T1+i +1), -(T1+i+1 +1) }; SAT_AddClauseSigned(mng, tmp, 3);}
		}
		// t2[N-1] = t1[N-1]
			{ int tmp[] = { (T2+N-1 +1), -(T1+N-1 +1)}; SAT_AddClauseSigned(mng, tmp, 2);}
			{ int tmp[] = { -(T2+N-1 +1), (T1+N-1 +1)}; SAT_AddClauseSigned(mng, tmp, 2);}
		
		
		// T3[i] = t2[i] xor in[i];
		// T3[i+2] = t2[i+1] xor in[i+1];
		// T3[i+1] = t2[i+2] xor in[i+2];
		cout<<" T3[i] = t2[i] xor in[i]; "<<endl;
		for(int i=0; i<N; i+=3){
			{ int tmp[] = { -(T3+i+0 +1), -(T2+i+0 +1), -(IN+i+0 +1) }; SAT_AddClauseSigned(mng, tmp, 3);}
			{ int tmp[] = { -(T3+i+0 +1), (T2+i+0 +1), (IN+i+0 +1) }; SAT_AddClauseSigned(mng, tmp, 3);}
			{ int tmp[] = { (T3+i+0 +1), -(T2+i+0 +1), (IN+i+0 +1) }; SAT_AddClauseSigned(mng, tmp, 3);}
			{ int tmp[] = { (T3+i+0 +1), (T2+i+0 +1), -(IN+i+0 +1) }; SAT_AddClauseSigned(mng, tmp, 3);}
			
			{ int tmp[] = { -(T3+i+2 +1), -(T2+i+1 +1), -(IN+i+1 +1) }; SAT_AddClauseSigned(mng, tmp, 3);}
			{ int tmp[] = { -(T3+i+2 +1), (T2+i+1 +1), (IN+i+1 +1) }; SAT_AddClauseSigned(mng, tmp, 3);}
			{ int tmp[] = { (T3+i+2 +1), -(T2+i+1 +1), (IN+i+1 +1) }; SAT_AddClauseSigned(mng, tmp, 3);}
			{ int tmp[] = { (T3+i+2 +1), (T2+i+1 +1), -(IN+i+1 +1) }; SAT_AddClauseSigned(mng, tmp, 3);}
			
			{ int tmp[] = { -(T3+i+1 +1), -(T2+i+2 +1), -(IN+i+2 +1) }; SAT_AddClauseSigned(mng, tmp, 3);}
			{ int tmp[] = { -(T3+i+1 +1), (T2+i+2 +1), (IN+i+2 +1) }; SAT_AddClauseSigned(mng, tmp, 3);}
			{ int tmp[] = { (T3+i+1 +1), -(T2+i+2 +1), (IN+i+2 +1) }; SAT_AddClauseSigned(mng, tmp, 3);}
			{ int tmp[] = { (T3+i+1 +1), (T2+i+2 +1), -(IN+i+2 +1) }; SAT_AddClauseSigned(mng, tmp, 3);}
		}
		
		// T4[i] = T3[i] != in[i];
		for(int i=0; i<N; ++i){
			{ int tmp[] = { -(T4+i +1), -(T3+i +1), -(IN+i +1) }; SAT_AddClauseSigned(mng, tmp, 3);}
			{ int tmp[] = { -(T4+i +1), (T3+i +1), (IN+i +1) }; SAT_AddClauseSigned(mng, tmp, 3);}
			{ int tmp[] = { (T4+i +1), -(T3+i +1), (IN+i +1) }; SAT_AddClauseSigned(mng, tmp, 3);}
			{ int tmp[] = { (T4+i +1), (T3+i +1), -(IN+i +1) }; SAT_AddClauseSigned(mng, tmp, 3);}
		}
		// Tout[0] = T4[0];
		{ int tmp[] = { -(Tout+0 +1), (T4+0 +1) }; SAT_AddClauseSigned(mng, tmp, 2);}
		{ int tmp[] = { (Tout+0 +1), -(T4+0 +1) }; SAT_AddClauseSigned(mng, tmp, 2);}
		
		// Tout[i] = Tout[i-1] | T4[i]
		for(int i=1; i<N; ++i){
			{ int tmp[] = { (Tout+i +1), -(Tout+(i-1) +1) }; SAT_AddClauseSigned(mng, tmp, 2);}
			{ int tmp[] = { (Tout+i +1), -(T4+i +1) }; SAT_AddClauseSigned(mng, tmp, 2);}
			{ int tmp[] = { -(Tout+i +1), (T4+i +1), (Tout+(i-1) +1) }; SAT_AddClauseSigned(mng, tmp, 3);}
		}
		
		// assert Tout[N-1];
		{ int tmp[] = { ((Tout+(N-1)+1)) }; SAT_AddClauseSigned(mng, tmp, 1);}
		
		for(int i=0; i<N; ++i){
			{ int tmp[] = { ((C+i+1)) }; SAT_AddClauseSigned(mng, tmp, 1, 2);}
		}
	}
};



class bitSwapSketch{
	
	int CNT;
	int N;
	int IN ;
	int T1p ;
	int T1;
	int T2;
	int C;
	SAT_Manager mng;
	public:
	
	bitSwapSketch(int N_p){
		mng = SAT_InitManager();
		CNT=0;
		N = N_p;
		cout<<"N="<<N;
		C = CNT++*N;
		SAT_SetNumVariables(mng, CNT*N);
	}
	
	
	void getControls(int ctrl[]){
		for(int i=0; i<N; ++i){
			int val = SAT_GetVarAsgnment(mng, C+i+1);
			if( val == 1) ctrl[i]= 1;
			else ctrl[i]= -1;
		}
		SAT_Reset(mng);
	}
	
	
	void verify_solution()
	{
	    int num_verified = 0;
	    for ( int cl_idx = SAT_GetFirstClause (mng); cl_idx >= 0; 
	          cl_idx = SAT_GetNextClause(mng, cl_idx)) {
	        int len = SAT_GetClauseNumLits(mng, cl_idx);
	        int * lits = new int[len+1];
	        SAT_GetClauseLits( mng, cl_idx, lits);
	        int i;
	        for (i=0; i< len; ++i) {
	            int v_idx = lits[i] >> 1;
	            int sign = lits[i] & 0x1;
	            int var_value = SAT_GetVarAsgnment( mng, v_idx);
	            if( (var_value == 1 && sign == 0) ||
	                (var_value == 0 && sign == 1) ) break;
	        }
	        if (i >= len) {
	            cerr << "Verify Satisfiable solution failed, please file a bug report, thanks. " << endl;
	            exit(6);
	        }
	        delete [] lits;
	        ++ num_verified;
	    }
	    cout <<"c "<< num_verified << " Clauses are true, Verify Solution successful."<<endl;;
	}
	
	void solve(){
		SAT_Solve(mng);
		/*
		for(int i=0; i<CNT*N; ++i){
			if ( i % N == 0) cout<<"----------------"<<endl;
			if( i==IN) cout<<"IN: "<<endl;
			if( i==T1p) cout<<"T1p: "<<endl;
			if( i==T1) cout<<"T1: "<<endl;
			if( i==T2) cout<<"T2: "<<endl;
			if( i==C) cout<<"C: "<<endl;
			cout<<" Var["<<i+1<<"] = " << SAT_GetVarAsgnment(mng, i+1)<<endl;	
		}
		cout<<"============================"<<endl;	
		*/
	}
	
	void anotherInput(int inv[], int invlen){
		IN = CNT++*N;
		T1p = CNT++*N;
		T1 = CNT++*N;
		T2 = CNT++*N;
		for(int i=0; i<4*N; ++i) SAT_AddVariable(mng);
		// t1[0] = in[0] & c[0];
		// (-t1[0], in[0])(-t1[0], c[0])(t1[0], -in[0], -c[0])
		cout<<" t1[0] = in[0] & c[0]; "<<endl;
		{ int tmp[] = { -(T1+0 +1), (IN+0 +1)}; SAT_AddClauseSigned(mng, tmp, 2);}
		{ int tmp[] = { -(T1+0 +1), (C+0 +1)}; SAT_AddClauseSigned(mng, tmp, 2);}
		{ int tmp[] = { (T1+0 +1), -(IN+0 +1), -(C+0 +1)}; SAT_AddClauseSigned(mng, tmp, 3);}
		
		// t1'[i] = in[i] xor in[i-1];
		// (-t1'[i], -in[i], -in[i-1])(-t1'[i], in[i], in[i-1])(t1'[i], -in[i], in[i-1])(t1'[i], in[i], -in[i-1])
		cout<<" t1'[i] = in[i] xor in[i-1]; "<<endl;
		for(int i=1; i<N; ++i){
			{ int tmp[] = { -(T1p+i +1), -(IN+i +1), -(IN+i-1 +1) }; SAT_AddClauseSigned(mng, tmp, 3);}
			{ int tmp[] = { -(T1p+i +1), (IN+i +1), (IN+i-1 +1) }; SAT_AddClauseSigned(mng, tmp, 3);}
			{ int tmp[] = { (T1p+i +1), -(IN+i +1), (IN+i-1 +1) }; SAT_AddClauseSigned(mng, tmp, 3);}
			{ int tmp[] = { (T1p+i +1), (IN+i +1), -(IN+i-1 +1) }; SAT_AddClauseSigned(mng, tmp, 3);}					
		}
		// t1[i] = t1p[i] & c[i]
		cout<<" t1[i] = t1p[i] & c[i] "<<endl;
		for(int i=1; i<N; ++i){
			{ int tmp[] = { -(T1+i +1), (T1p+i +1)}; SAT_AddClauseSigned(mng, tmp, 2);}
			{ int tmp[] = { -(T1+i +1), (C+i +1)}; SAT_AddClauseSigned(mng, tmp, 2);}
			{ int tmp[] = { (T1+i +1), -(T1p+i +1), -(C+i +1)}; SAT_AddClauseSigned(mng, tmp, 3);}
		}
		
		
		// t2[i] = t1[i] xor t1[i+1]
		cout<<" t2[i] = t1[i] xor t1[i+1] "<<endl;
		for(int i=0; i<N-1; ++i){
			{ int tmp[] = { -(T2+i +1), -(T1+i +1), -(T1+i+1 +1) }; SAT_AddClauseSigned(mng, tmp, 3);}
			{ int tmp[] = { -(T2+i +1), (T1+i +1), (T1+i+1 +1) }; SAT_AddClauseSigned(mng, tmp, 3);}
			{ int tmp[] = { (T2+i +1), -(T1+i +1), (T1+i+1 +1) }; SAT_AddClauseSigned(mng, tmp, 3);}
			{ int tmp[] = { (T2+i +1), (T1+i +1), -(T1+i+1 +1) }; SAT_AddClauseSigned(mng, tmp, 3);}
		}
		// t2[N-1] = t1[N-1]
			{ int tmp[] = { (T2+N-1 +1), -(T1+N-1 +1)}; SAT_AddClauseSigned(mng, tmp, 2);}
			{ int tmp[] = { -(T2+N-1 +1), (T1+N-1 +1)}; SAT_AddClauseSigned(mng, tmp, 2);}
		
		
		// in[i] = t2[i] xor in[i];
		// in[i+2] = t2[i+1] xor in[i+1];
		// in[i+1] = t2[i+2] xor in[i+2];
		cout<<" in[i] = t2[i] xor in[i]; "<<endl;
		for(int i=0; i<N; i+=3){
			{ int tmp[] = { -(IN+i+0 +1), -(T2+i+0 +1), -(IN+i+0 +1) }; SAT_AddClauseSigned(mng, tmp, 3);}
			{ int tmp[] = { -(IN+i+0 +1), (T2+i+0 +1), (IN+i+0 +1) }; SAT_AddClauseSigned(mng, tmp, 3);}
			{ int tmp[] = { (IN+i+0 +1), -(T2+i+0 +1), (IN+i+0 +1) }; SAT_AddClauseSigned(mng, tmp, 3);}
			{ int tmp[] = { (IN+i+0 +1), (T2+i+0 +1), -(IN+i+0 +1) }; SAT_AddClauseSigned(mng, tmp, 3);}
			
			{ int tmp[] = { -(IN+i+2 +1), -(T2+i+1 +1), -(IN+i+1 +1) }; SAT_AddClauseSigned(mng, tmp, 3);}
			{ int tmp[] = { -(IN+i+2 +1), (T2+i+1 +1), (IN+i+1 +1) }; SAT_AddClauseSigned(mng, tmp, 3);}
			{ int tmp[] = { (IN+i+2 +1), -(T2+i+1 +1), (IN+i+1 +1) }; SAT_AddClauseSigned(mng, tmp, 3);}
			{ int tmp[] = { (IN+i+2 +1), (T2+i+1 +1), -(IN+i+1 +1) }; SAT_AddClauseSigned(mng, tmp, 3);}
			
			{ int tmp[] = { -(IN+i+1 +1), -(T2+i+2 +1), -(IN+i+2 +1) }; SAT_AddClauseSigned(mng, tmp, 3);}
			{ int tmp[] = { -(IN+i+1 +1), (T2+i+2 +1), (IN+i+2 +1) }; SAT_AddClauseSigned(mng, tmp, 3);}
			{ int tmp[] = { (IN+i+1 +1), -(T2+i+2 +1), (IN+i+2 +1) }; SAT_AddClauseSigned(mng, tmp, 3);}
			{ int tmp[] = { (IN+i+1 +1), (T2+i+2 +1), -(IN+i+2 +1) }; SAT_AddClauseSigned(mng, tmp, 3);}
		}
		
		for(int i=0; i<N; ++i){
			cout<<" FAFAFA["<<i<<"] ="<<(inv[i%invlen]*(IN+i+1))<<endl;
			{ int tmp[] = { (inv[i%invlen]*(IN+i+1)) }; SAT_AddClauseSigned(mng, tmp, 1, 2);}
		}
	}
};



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

	SAT_SetNumVariables(mng, 3);
	
	
	// t1[0] = in[0] & c[0];
	// (-t1[0], in[0])(-t1[0], c[0])(t1[0], -in[0], -c[0])
	
	{ int tmp[] = { -(1), (2)}; SAT_AddClauseSigned(mng, tmp, 2);}
	{ int tmp[] = { -(1), (3)}; SAT_AddClauseSigned(mng, tmp, 2);}
	{ int tmp[] = { (1), -(2), -(3)}; SAT_AddClauseSigned(mng, tmp, 3);}	
	
	SAT_Solve(mng);
	
	for(int i=0; i<3; ++i){
		cout<<" Var["<<i+1<<"] = " << SAT_GetVarAsgnment(mng, i+1)<<endl;	
	}
		SAT_DeleteClauseGroup(mng, 2);
	cout<<"============================"<<endl;
		{ int tmp[] = { (IN+1) }; SAT_AddClauseSigned(mng, tmp, 1, 2);}
		{ int tmp[] = { -(IN+2) }; SAT_AddClauseSigned(mng, tmp, 1, 2);}
		{ int tmp[] = { -(IN+3) }; SAT_AddClauseSigned(mng, tmp, 1, 2);}
	SAT_Solve(mng);
	for(int i=0; i<5*N; ++i){
		if( i==IN) cout<<"IN: "<<endl;
		if( i==T1p) cout<<"T1p: "<<endl;
		if( i==T1) cout<<"T1: "<<endl;
		if( i==T2) cout<<"T2: "<<endl;
		if( i==C) cout<<"C: "<<endl;
		cout<<" Var["<<i+1<<"] = " << SAT_GetVarAsgnment(mng, i+1)<<endl;	
	}
	
	SAT_DeleteClauseGroup(mng, 2);
	cout<<"============================"<<endl;
		{ int tmp[] = { -(IN+1) }; SAT_AddClauseSigned(mng, tmp, 1, 2);}
		{ int tmp[] = { (IN+2) }; SAT_AddClauseSigned(mng, tmp, 1, 2);}
		{ int tmp[] = { -(IN+3) }; SAT_AddClauseSigned(mng, tmp, 1, 2);}
	SAT_Solve(mng);
	for(int i=0; i<5*N; ++i){
		if( i==IN) cout<<"IN: "<<endl;
		if( i==T1p) cout<<"T1p: "<<endl;
		if( i==T1) cout<<"T1: "<<endl;
		if( i==T2) cout<<"T2: "<<endl;
		if( i==C) cout<<"C: "<<endl;
		cout<<" Var["<<i+1<<"] = " << SAT_GetVarAsgnment(mng, i+1)<<endl;	
	}
	
	return 0;
}

int maun(int argc, char ** argv)
{

	SAT_Manager mng = SAT_InitManager();
	SAT_SetNumVariables(mng, 2);
	{ int tmp[] = { -(1), (2) }; SAT_AddClauseSigned(mng, tmp, 2);}
	{ int tmp[] = { (1), -(2) }; SAT_AddClauseSigned(mng, tmp, 2);}
	SAT_Solve(mng);

	cout<<" Var["<<1<<"] = " << SAT_GetVarAsgnment(mng, 1)<<endl;	
	cout<<" Var["<<2<<"] = " << SAT_GetVarAsgnment(mng, 2)<<endl;	

	SAT_Reset(mng);

	{ int tmp[] = { (1) }; SAT_AddClauseSigned(mng, tmp, 1, 2);}


	SAT_Solve(mng);

	cout<<" Var["<<1<<"] = " << SAT_GetVarAsgnment(mng, 1)<<endl;	
	cout<<" Var["<<2<<"] = " << SAT_GetVarAsgnment(mng, 2)<<endl;	

}


int main(int argc, char ** argv)
{
	//SAT_Manager mng = SAT_InitManager();		
	// in = 0;
	// t1' = 1;
	// t1 = 2;
	// t2 = 3;
	int N = 3*atoi(argv[1]);
	int * ctrl = new int[N];
	int * input = new int[N];
	bitSwapSketch bss(N);
	bitSwapSketchCheck bscheck(N);

	{ int tmp[] = {-1,-1,-1}; bss.anotherInput(tmp, 3); }
	bss.solve();
	
	cout<<"NOW THE GOOD ONE"<<endl;
	bss.getControls(ctrl);
	bscheck.setup();
	int iterations = 0;
	bool isDone;
	do{
		for(int i=0; i<N; ++i) cout<<"		ctrl["<<i<<"]="<<ctrl[i]<<endl; cout<<"-----------------------------"<<endl;
		isDone = bscheck.testControls(ctrl, N, input);
		if(isDone){
			for(int i=0; i<N; ++i) cout<<"		input["<<i<<"]="<<input[i]<<endl; cout<<"-----------------------------"<<endl;
			bss.anotherInput(input, N);
			bss.solve();
			bss.getControls(ctrl);
		}
		cout<<"********  "<<iterations<<endl;
		++iterations;
	}while(isDone);

	cout<<" GOT THE CORRECT ANSWER IN "<<iterations<<" iterations."<<endl;
	return 0;
}

