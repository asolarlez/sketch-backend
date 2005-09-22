#include "BooleanToCNF.h"




class bitSwapSketchCheck{

	int N;
	int SN;
	SAT_Manager mng;
	varDir dir;
	public:
	
	bitSwapSketchCheck(int N_p, int SN_p):dir(mng){
		mng = SAT_InitManager();
		SAT_SetNumVariables(mng, 0);		
		dir.setMng(mng);		
		N = N_p;
		SN = SN_p;
		cout<<"N="<<N;
		dir.declareArr("C", N);
		dir.declareArr("shamt", SN);
		dir.declareArr("IN", N);
		dir.declareArr("T1p", N);
		dir.declareArr("T1", N);
		dir.declareArr("T2", N);
		dir.declareArr("T3", N);
		dir.declareArr("T4", N);
		dir.declareArr("Tsp", N);
		dir.declareArr("Tsm", N);
		dir.declareArr("Tout", N);
	}
	
	bool testControls(int ctrl[], int ctrllen, int inv[]){
		SAT_DeleteClauseGroup(mng, 2);
		for(int i=0; i<N; ++i){
			setVarClause(mng, ctrl[i%ctrllen]*dir.getArr("C", i), 2);
		}
		for(int i=0; i<SN; ++i){
			setVarClause(mng, ctrl[(N+i)%ctrllen]*dir.getArr("shamt", i), 2);
		}
	    int result = SAT_Solve(mng);
	    if (result != SATISFIABLE) 
	    	return false;
		for(int i=0; i<N; ++i){
			int val = SAT_GetVarAsgnment(mng, dir.getArr("IN", i));
			if( val == 1) inv[i]= 1;
			else inv[i]= -1;
		}
		Dout( dir.print() );
		return true;
	}
	
	void setup(){		

		cout<<" t1[0] = in[0] & c[0]; "<<endl;
		addAndClause(mng, dir.getArr("T1", 0), dir.getArr("IN", 0), dir.getArr("C" ,0));


		shiftArrNdet(mng, dir, "Tsm", "IN", "shamt", RIGHT);

	
		// t1'[i] = in[i] xor Tsm[i];
		cout<<" t1'[i] = in[i] xor Tsm[i]; "<<endl;
		for(int i=1; i<N; ++i){
			addXorClause(mng, dir.getArr("T1p", i), dir.getArr("IN", i), dir.getArr("Tsm" ,i));
		}
		// t1[i] = t1p[i] & c[i]
		cout<<" t1[i] = t1p[i] & c[i] "<<endl;
		for(int i=1; i<N; ++i){
			addAndClause(mng, dir.getArr("T1", i), dir.getArr("T1p", i), dir.getArr("C" ,i));
		}

		shiftArrNdet(mng, dir, "Tsp", "T1", "shamt", LEFT);
		
		// t2[i] = t1[i] xor Tsp[i]
		cout<<" t2[i] = t1[i] xor tsp[i] "<<endl;
		for(int i=0; i<N-1; ++i){
			addXorClause(mng, dir.getArr("T2", i), dir.getArr("T1", i), dir.getArr("Tsp" ,i));
		}
		// t2[N-1] = t1[N-1]
		addEqualsClause(mng, dir.getArr("T2", N-1), dir.getArr("T1", N-1));		
		
		// T3[i] = t2[i] xor in[i];
		// T3[i+2] = t2[i+1] xor in[i+1];
		// T3[i+1] = t2[i+2] xor in[i+2];
		cout<<" T3[i] = t2[i] xor in[i]; "<<endl;
		for(int i=0; i<N; i+=3){
			addXorClause(mng, dir.getArr("T3", i), dir.getArr("T2", i), dir.getArr("IN" ,i));
			addXorClause(mng, dir.getArr("T3", i+2), dir.getArr("T2", i+1), dir.getArr("IN" ,i+1));
			addXorClause(mng, dir.getArr("T3", i+1), dir.getArr("T2", i+2), dir.getArr("IN" ,i+2));
		}
		
		// T4[i] = T3[i] != in[i];
		for(int i=0; i<N; ++i){
			addXorClause(mng, dir.getArr("T4", i), dir.getArr("T3", i), dir.getArr("IN" ,i));
		}
		// Tout[0] = T4[0];
		addEqualsClause(mng, dir.getArr("Tout", 0), dir.getArr("T4", 0));
		
		// Tout[i] = Tout[i-1] | T4[i]
		for(int i=1; i<N; ++i){
			addOrClause(mng, dir.getArr("Tout", i), dir.getArr("Tout", i-1), dir.getArr("T4" ,i));
		}
		
		// assert Tout[N-1];
		setVarClause(mng, dir.getArr("Tout", N-1));
		
		for(int i=0; i<N; ++i){
			setVarClause(mng, dir.getArr("C", i), 2);
		}
	}
};




class bitSwapSketch{
	int N;
	int SN;
	SAT_Manager mng;
	varDir dir;
	public:
	
	bitSwapSketch(int N_p, int SN_p):dir(mng){
		mng = SAT_InitManager();
		SAT_SetNumVariables(mng, 0);		
		dir.setMng(mng);
		N = N_p;
		SN = SN_p;
		cout<<"N="<<N;
		dir.declareArr("C", N);
		dir.declareArr("shamt", SN);
	}
	
	
	void getControls(int ctrl[]){
		for(int i=0; i<N; ++i){
			int val = SAT_GetVarAsgnment(mng, dir.getArr("C", i));
			if( val == 1) ctrl[i]= 1;
			else ctrl[i]= -1;
		}
		
		for(int i=0; i<SN; ++i){
			int val = SAT_GetVarAsgnment(mng, dir.getArr("shamt", i));
			if( val == 1) ctrl[i+N]= 1;
			else ctrl[i+N]= -1;
		}
		SAT_Reset(mng);
	}
		
	void solve(){
		SAT_Solve(mng);
		Dout( dir.print() );
	}
	
	void anotherInput(int inv[], int invlen){
		dir.declareArr("IN", N);
		dir.declareArr("T1p", N);
		dir.declareArr("T1", N);
		dir.declareArr("T2", N);
		dir.declareArr("Tsp", N);
		dir.declareArr("Tsm", N);
		
				cout<<" t1[0] = in[0] & c[0]; "<<endl;
		addAndClause(mng, dir.getArr("T1", 0), dir.getArr("IN", 0), dir.getArr("C" ,0));





		shiftArrNdet(mng, dir, "Tsm", "IN", "shamt", RIGHT);

	
		// t1'[i] = in[i] xor Tsm[i];
		cout<<" t1'[i] = in[i] xor Tsm[i]; "<<endl;
		for(int i=1; i<N; ++i){
			addXorClause(mng, dir.getArr("T1p", i), dir.getArr("IN", i), dir.getArr("Tsm" ,i));
		}
		// t1[i] = t1p[i] & c[i]
		cout<<" t1[i] = t1p[i] & c[i] "<<endl;
		for(int i=1; i<N; ++i){
			addAndClause(mng, dir.getArr("T1", i), dir.getArr("T1p", i), dir.getArr("C" ,i));
		}

		shiftArrNdet(mng, dir, "Tsp", "T1", "shamt", LEFT);
		
		// t2[i] = t1[i] xor Tsp[i]
		cout<<" t2[i] = t1[i] xor tsp[i] "<<endl;
		for(int i=0; i<N-1; ++i){
			addXorClause(mng, dir.getArr("T2", i), dir.getArr("T1", i), dir.getArr("Tsp" ,i));
		}

		// t2[N-1] = t1[N-1]
		addEqualsClause(mng, dir.getArr("T2", N-1), dir.getArr("T1", N-1));		
		
		// in[i] = t2[i] xor in[i];
		// in[i+2] = t2[i+1] xor in[i+1];
		// in[i+1] = t2[i+2] xor in[i+2];
		cout<<" IN[i] = t2[i] xor in[i]; "<<endl;
		for(int i=0; i<N; i+=3){
			addXorClause(mng, dir.getArr("IN", i), dir.getArr("T2", i), dir.getArr("IN" ,i));
			addXorClause(mng, dir.getArr("IN", i+2), dir.getArr("T2", i+1), dir.getArr("IN" ,i+1));
			addXorClause(mng, dir.getArr("IN", i+1), dir.getArr("T2", i+2), dir.getArr("IN" ,i+2));
		}
				
		
		for(int i=0; i<N; ++i){
			setVarClause(mng, inv[i%invlen]*dir.getArr("IN", i));
		}
	}
};



int maon(int argc, char ** argv)
{
	SAT_Manager mng = SAT_InitManager();	
	SAT_SetNumVariables(mng, 0);
	
	varDir dir(mng);
	
	dir.declareArr("input", 5);
	dir.declareArr("output", 5);
	dir.declareArr("shamt", 2);	
	setVarClause(mng, -dir.getArr("input", 0));
	setVarClause(mng, -dir.getArr("input", 1));
	setVarClause(mng, dir.getArr("input", 2));
	setVarClause(mng, -dir.getArr("input", 3));
	setVarClause(mng, -dir.getArr("input", 4));
	shiftArrNdet(mng, dir, "output", "input", "shamt", RIGHT);

//	addEqualsClause(mng, dir.getArr("output", 0), dir.getArr("input", 0+1));
//	addEqualsClause(mng, dir.getArr("output", 1), dir.getArr("input", 1-1));
	addEqualsClause(mng, dir.getArr("output", 2), dir.getArr("input", 2-2));
	addEqualsClause(mng, dir.getArr("output", 3), dir.getArr("input", 3-2));
	addEqualsClause(mng, dir.getArr("output", 4), dir.getArr("input", 4-2));
	
    int result = SAT_Solve(mng);
    if (result != SATISFIABLE)
		cout<<"NOT SAT !!"<<endl;
	cout<<" NVARS = "<<SAT_NumVariables(mng)<<endl;
	dir.print();
}





int main(int argc, char ** argv)
{
	//SAT_Manager mng = SAT_InitManager();		
	// in = 0;
	// t1' = 1;
	// t1 = 2;
	// t2 = 3;
	int N = 3*atoi(argv[1]);
	int SN = 2;
	int * ctrl = new int[N+SN];
	int * input = new int[N];
	bitSwapSketch bss(N, SN);
	bitSwapSketchCheck bscheck(N, SN);

	{ int tmp[] = {-1, 1}; bss.anotherInput(tmp, 2); }
	bss.solve();
	
	cout<<"NOW THE GOOD ONE"<<endl;
	bss.getControls(ctrl);
	bscheck.setup();
	int iterations = 0;
	bool isDone;
	do{
		for(int i=0; i<N+SN; ++i) cout<<"		ctrl["<<i<<"]="<<ctrl[i]<<endl; cout<<"-----------------------------"<<endl;
		isDone = bscheck.testControls(ctrl, N+SN, input);
		if(isDone){
			Dout( for(int i=0; i<N; ++i) cout<<"		input["<<i<<"]="<<input[i]<<endl; cout<<"-----------------------------"<<endl );
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

