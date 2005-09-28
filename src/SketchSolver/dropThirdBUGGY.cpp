#include "FindCheckSolver.h"


class dropThird: public FindCheckSolver{
	int N;
	int S;
	protected:
	void defineSketch(SAT_Manager mng, varDir& dir);
	void defineSpec(SAT_Manager mng, varDir& dir);
	public:
	dropThird(int N_p, int S_p, int NS_p){
		N = N_p;
		S = S_p;
		declareControl("C", N*S);
		nseeds = NS_p;
	}
};

void dropThird::defineSketch(SAT_Manager mng, varDir& dir){
	dir.declareArr(IN, N);
	dir.declareArr(SOUT, N);
	dir.declareArr(OUT, N);
	
	int lastStage = dir.getArr(IN, 0);
	int shamt = 1;
	int jj=0;
	int curStage = dir.getVarCnt();
	for(int stage = 0; stage < S; ++stage){		
		for(int i=0; i<N; ++i, ++jj){
			if( i+shamt < N){
				addChoiceClause(mng, dir.newAnonymousVar(), dir.getArr("C", jj), lastStage+i, lastStage+i+shamt);
			}else{
				addAndClause(mng, dir.newAnonymousVar(), lastStage+i , dir.getArr("C", jj));
			}
		}
		shamt = shamt * 2;
		lastStage = curStage;
		curStage = dir.getVarCnt();		
	}
	
	for(int i=0; i<N; ++i){
		addEqualsClause(mng, dir.getArr(SOUT, i), lastStage+i);
	}
}

void dropThird::defineSpec(SAT_Manager mng, varDir& dir){
	Dout( cout<<"defineSpec()"<<endl );
	int jj=0;
	for(int i=0; i<N; ++i){
		if(i % 3 != 2){
			addEqualsClause(mng, dir.getArr(OUT, jj), dir.getArr(IN, i));	
			jj++;			
		}
	}
	for(;jj<N; ++jj){
		setVarClause(mng, -dir.getArr(OUT, jj));	
	}
}






int main(int argc, char ** argv)
{
	
	int N = atoi(argv[1]);	
	int k = N-1;
	N = 3*N;
	int tmp = 0;
	while(k > 0){
		tmp = tmp +1;
		k = k/2;
		cout<<"k ="<<k<<endl;
	}	
	cout<<tmp<<" STEPS"<<endl;
	dropThird bswap(N, tmp, atoi(argv[2]));
	bswap.setup();
	bswap.solve();

	return 0;
}




