#include "FindCheckSolver.h"


class dropThird: public FindCheckSolver{
	int N;
	int S;
	protected:
	void defineSketch(SAT_Manager mng, varDir& dir);
	void defineSpec(SAT_Manager mng, varDir& dir);
	int logShift(SAT_Manager mng, varDir& dir, const string& C, int start, int end);
	public:
	dropThird(int N_p, int S_p, int NS_p){
		N = N_p;
		S = S_p;
		int k=1;
		int size=0;
		while(k<=N){
			k=k*2;	
			++size;
		}
		declareControl("C1", N*S);
		declareControl("C2", N*S);
		declareControl("C3", N*S);
		declareControl("Co1", N*2);
		declareControl("Co2", N*2);
		declareControl("S1", size);
		declareControl("S2", size);
		declareControl("S3", size);
		declareControl("loopvar", size);
		nseeds = NS_p;
	}
};



int dropThird::logShift(SAT_Manager mng, varDir& dir, const string& C, int start, int end){
	int lastStage = dir.getArr(IN, start);
	int shamt = 1;
	int jj=0;
	int curStage = dir.getVarCnt();
	int idxs[S];
	idxs[0] = lastStage;
	for(int stage = 0; stage < S; ++stage){
		for(int i=0; i<end-start; ++i, ++jj){
			if( start+i+shamt < end){
				addChoiceClause(mng, dir.newAnonymousVar(), dir.getArr(C, jj), lastStage+i, lastStage+i+shamt);
			}else{
				addAndClause(mng, dir.newAnonymousVar(), lastStage+i , dir.getArr(C, jj));
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


void dropThird::defineSketch(SAT_Manager mng, varDir& dir){
	dir.declareArr(IN, N*3);
	dir.declareArr(SOUT, N*2);
	dir.declareArr(OUT, N*2);
	
	int tmp1p = logShift(mng, dir, "C1", 0, N);
	int tmp2p = logShift(mng, dir, "C2", N, 2*N);	
	int tmp3p = logShift(mng, dir, "C3", 2*N, 3*N);	
	
	dir.declareArr("tmp1q", N*2);
	dir.declareArr("tmp2q", N*2);
	dir.declareArr("tmp3q", N*2);
	
	
	for(int i=0; i<N; ++i){
		addEqualsClause(mng, 	dir.getArr("tmp1q", i), tmp1p+i);
		addEqualsClause(mng, 	dir.getArr("tmp2q", i), tmp2p+i);
		addEqualsClause(mng, 	dir.getArr("tmp3q", i), tmp3p+i);
	}
	for(int i=0; i<N; ++i){
		setVarClause(mng, -dir.getArr("tmp1q", N+i));
		setVarClause(mng, -dir.getArr("tmp2q", N+i));
		setVarClause(mng, -dir.getArr("tmp3q", N+i));
	}
	
	dir.declareArr("tmp1r", N*2);
	dir.declareArr("tmp2r", N*2);
	dir.declareArr("tmp3r", N*2);
	
	shiftArrNdet(mng, dir, "tmp1r", "tmp1q", "S1", LEFT);
	shiftArrNdet(mng, dir, "tmp2r", "tmp2q", "S2", RIGHT);
	shiftArrNdet(mng, dir, "tmp3r", "tmp3q", "S3", RIGHT);
	
	dir.declareArr("tmpO", N*2);
	for(int i=0; i<2*N; ++i){
		addChoiceClause(mng, dir.getArr("tmpO", i), dir.getArr("Co1", i), dir.getArr("tmp1r", i), dir.getArr("tmp2r", i));
	}
	
	for(int i=0; i<2*N; ++i){
		addChoiceClause(mng, dir.getArr(SOUT, i), dir.getArr("Co2", i), dir.getArr("tmpO", i), dir.getArr("tmp3r", i));
	}
}

void dropThird::defineSpec(SAT_Manager mng, varDir& dir){
	Dout( cout<<"defineSpec()"<<endl );
	int jj=0;
	for(int i=0; i<3*N; ++i){
		if(i % 3 != 2){
			addEqualsClause(mng, dir.getArr(OUT, jj), dir.getArr(IN, i));	
			jj++;			
		}
	}
	Assert( jj == 2*N, "error, bad counting"<<jj<<", "<<2*N<<endl);
}






int main(int argc, char ** argv)
{
	
	int N = atoi(argv[1]);	
	int k = (N-1)/3 ;
	int tmp = 0;
	while(k > 0){
		tmp = tmp +1;
		k = k/2;
		cout<<"k ="<<k<<endl;
	}
	cout<<tmp<<" STEPS"<<endl;
	dropThird bswap(N, tmp*2, atoi(argv[2]));
	bswap.setup();
	bswap.solve();

	return 0;
}

