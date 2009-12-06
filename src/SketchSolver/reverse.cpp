#include "FindCheckSolver.h"


class bitReverse: public FindCheckSolver{
	int N;
	int S;
	protected:
	void defineSketch(SAT_Manager mng, varDir& dir);
	void defineSpec(SAT_Manager mng, varDir& dir);
	int logShift(SAT_Manager mng, varDir& dir, const string& C, int start, int end);
	public:
	bitReverse(int N_p, int S_p, int NS_p){
		N = N_p;
		S = S_p;
		Nin = N;
		Nout = N;
		int k=1;
		int size=0;
		while(k<=N){
			k=k*2;	
			++size;
		}
		declareControl("C1", N*S);
		declareControl("loopvar", size);
		nseeds = NS_p;
	}
};



int bitReverse::logShift(SAT_Manager mng, varDir& dir, const string& C, int start, int end){
	int lastStage = dir.getArr(IN, start);
	int shamt = 1;
	int jj=0;
	int curStage = dir.getVarCnt();
	int idxs[S];
	idxs[0] = lastStage;
	for(int stage = 0; stage < S; ++stage){
		for(int i=0; i<end-start; ++i, ++jj){
			if( start+i+shamt < end && start+i-shamt>=start){
				addChoiceClause(mng, dir.newAnonymousVar(), dir.getArr(C, jj), lastStage+i-shamt, lastStage+i+shamt);
			}else{
				if( start+i+shamt < end ){
					addAndClause(mng, dir.newAnonymousVar(), lastStage+i+shamt , dir.getArr(C, jj));
				}else{
					if(start+i-shamt>=start){
						addAndClause(mng, dir.newAnonymousVar(), lastStage+i-shamt , dir.getArr(C, jj));
					}else{
						cout<<"step "<<stage; setVarClause(mng, -dir.newAnonymousVar());
					}
				}
			}
		}
		shamt = shamt * 2;
		idxs[stage+1] = curStage;
		lastStage = curStage;
		curStage = dir.getVarCnt();		
	}
	varRange vr = getSwitchVars(mng, dir, "loopvar");
	return select(mng, dir, idxs, vr.varID ,S+1, end-start);
}


void bitReverse::defineSketch(SAT_Manager mng, varDir& dir){
	dir.declareArr(IN, N);
	dir.declareArr(SOUT, N);
	dir.declareArr(OUT, N);
	
	int tmp1p = logShift(mng, dir, "C1", 0, N);
	for(int i=0; i<N; ++i){
		addEqualsClause(mng, tmp1p+i, dir.getArr(SOUT, i));	
	}
}

void bitReverse::defineSpec(SAT_Manager mng, varDir& dir){
	Dout( cout<<"defineSpec()"<<endl );
	int jj=N-1;
	for(int i=0; i<N; ++i, --jj){
		addEqualsClause(mng, dir.getArr(OUT, i), dir.getArr(IN, jj));
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
	bitReverse bswap(N, 2*tmp, atoi(argv[2]));
	bswap.setup();
	bswap.solve();
	return 0;
}

