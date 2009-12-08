#include "FindCheckSolver.h"


class logBase2: public FindCheckSolver{
	int N;
	protected:
	void defineSketch(SAT_Manager mng, varDir& dir);
	void defineSpec(SAT_Manager mng, varDir& dir);
	
	public:
	logBase2(int N_p, int Nout_p, int NS_p){
		N = N_p;
		Nin = N;
		Nout = Nout_p;
		int k=1;
		int size=0;
		while(k<=N){
			k=k*2;	
			++size;
		}
		declareControl("C1", N*Nout);
		declareControl("S1", Nout*Nout);
		nseeds = NS_p;
	}
};





void logBase2::defineSketch(SAT_Manager mng, varDir& dir){
	dir.declareArr(IN, N);
	dir.declareArr(SOUT, Nout);
	dir.declareArr(OUT, Nout);
	
	int ZERO = dir.getVarCnt() ; 
	Dout( cout<<"* zero = ["<<ZERO<<", "<<ZERO+N-1<<"]"<<endl );
	for(int i=0; i<N; ++i){
		setVarClause(mng, -dir.newAnonymousVar());	
	}
	int currc = dir.getVarCnt() ; 
	Dout( cout<<"* currc = ["<<currc<<", "<<currc+Nout-1<<"]"<<endl );
	for(int i=0; i<Nout; ++i){
		setVarClause(mng, -dir.newAnonymousVar());	
	}
	
	int currv = dir.getVarCnt() ; 
	for(int i=0; i<N; ++i){
		addEqualsClause(mng, dir.newAnonymousVar(), dir.getArr(IN, i));
	}
	Dout( cout<<"* currv = ["<<currv<<", "<<currv+N-1<<"] = IN"<<endl );
	
	for(int k=0; k<Nout; ++k){
		int tmp1 = dir.getVarCnt();
		Dout( cout<<"* tmp1 = ["<<tmp1<<", "<<tmp1+N-1<<"] = currv & C1"<<endl );
		for(int i=0; i<N; ++i){
			addAndClause(mng, dir.newAnonymousVar(), currv+i, dir.getArr("C1", k*N+i));			
		}
		int tmp2 = assertVectorsDiffer(mng, dir, tmp1, ZERO, N);
		Dout( cout<<"* tmp2 = ["<<tmp2<<"] = tmp1 != 0"<<endl );
		//tmp2 = v & b[i] == 0;
		dir.declareArr("Stmp", Nout);
		for(int i=0; i<Nout; ++i){
			addEqualsClause(mng, dir.getArr("Stmp", i), dir.getArr("S1", k*Nout+i));	
		}
		Dout( cout<<"* Stmp = S1"<<endl );
		
		dir.declareArr("vtmp", N);
		dir.declareArr("currv", N);
		for(int i=0; i<N; ++i){
			addEqualsClause(mng, dir.getArr("currv", i), currv+i);	
		}
		Dout( cout<<"* currv = currv"<<endl );
		shiftArrNdet(mng, dir, "vtmp", "currv", "Stmp", RIGHT);
		int tmpc = dir.getVarCnt();
		Dout( cout<<"* tmpc = ["<<tmpc<<", "<<tmpc+Nout-1<<"] = currc | Stmp"<<endl );
		for(int i=0; i<Nout; ++i){
			addOrClause(mng, dir.newAnonymousVar(), currc+i, dir.getArr("Stmp", i));	
		}

		int oldcurrv = currv;
		currv = dir.getVarCnt();
		Dout( cout<<"* currv = ["<<currv<<", "<<currv+N-1<<"] = -tmp2? vtmp: oldcurrv"<<endl );
		for(int i=0; i<N; ++i){
			addChoiceClause(mng, dir.newAnonymousVar(), -tmp2 ,dir.getArr("vtmp", i), oldcurrv+i);
		}
		int oldcurrc = currc;
		currc = dir.getVarCnt();
		Dout( cout<<"* currc = ["<<currc<<", "<<currc+Nout-1<<"] = -tmp2? tmpc: oldcurrc"<<endl );
		for(int i=0; i<Nout; ++i){
			addChoiceClause(mng, dir.newAnonymousVar(), -tmp2 , tmpc + i, oldcurrc+i);
		}		
	}
	Dout( cout<<"* SOUT = currc"<<endl );
	for(int i=0; i<Nout; ++i){	
		addEqualsClause(mng, dir.getArr(SOUT, i), currc+i);	
	}
}

void logBase2::defineSpec(SAT_Manager mng, varDir& dir){
	int ONE = dir.getVarCnt();
	setVarClause(mng, dir.newAnonymousVar());
	
	int ZERO = dir.getVarCnt();
	setVarClause(mng, -dir.newAnonymousVar());
	
	int currv = dir.getVarCnt();
	for(int i=0; i<Nout; ++i){
		setVarClause(mng, -dir.newAnonymousVar());
	}
	int val[Nout];
	for(int i=0; i<Nout; ++i){ val[i] = ZERO; };
	for(int i=0; i<N; ++i){
		int lastv = currv;
		currv = dir.getVarCnt();
		for(int j=0; j<Nout; ++j){
			addChoiceClause(mng, dir.newAnonymousVar(), dir.getArr(IN, i), val[j], lastv+j);
		}
		int ov = val[Nout-1];
		val[Nout-1] = val[Nout-1] == ZERO? ONE : ZERO;
		for(int j=1; j<Nout; ++j){
			if(val[Nout-1-(j-1)]==ZERO && ov == ONE){
				ov = val[Nout-1-j];
				val[Nout-1-j] = val[Nout-1-j] == ZERO? ONE:ZERO;
			}else{
				ov = val[Nout-1-j];
			}
		}
	}
	for(int j=0; j<Nout; ++j){
		addEqualsClause(mng, dir.getArr(OUT, j), currv+j);
	}
}






int main(int argc, char ** argv)
{
	
	int N = atoi(argv[1]);	
	int k = N-1;
	int tmp = 0;
	while(k > 0){
		tmp = tmp +1;
		k = k/2;
		cout<<"k ="<<k<<endl;
	}	
	cout<<2*tmp<<" STEPS"<<endl;
	logBase2 bswap(32, 5, atoi(argv[2]));
	bswap.setup();
	bswap.solve();
	return 0;
}
