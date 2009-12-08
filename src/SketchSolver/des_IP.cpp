#include "FindCheckSolver.h"


class desIP: public FindCheckSolver{
	int N;
	int logN;
	protected:
	void defineSketch(SAT_Manager mng, varDir& dir);
	void defineSpec(SAT_Manager mng, varDir& dir);

	public:
	desIP(int NS_p){
		Nin = N;
		Nout = N;
		N = 64;
		int k=1;
		int size=0;
		while(k<(N/2)){
			k=k*2;
			++size;
		}
		logN = size;
		declareControl("C1", 2*logN);
		declareControl("C2", 2*logN);
		declareControl("C3", N);	
		declareControl("C4", N);
		declareControl("loopvar", N*logN);
		nseeds = NS_p;
	}
};






void desIP::defineSketch(SAT_Manager mng, varDir& dir){
	dir.declareArr(IN, N);
	dir.declareArr(OUT, N);
	dir.declareArr(SOUT, N);
	
	dir.declareArr("tmp1", N);
	dir.declareArr("tmp2", N);
	shiftArrNdet(mng, dir, "tmp1", IN, "C1", LEFT);
	shiftArrNdet(mng, dir, "tmp2", IN, "C2", RIGHT);
	int tmp3 = dir.getVarCnt();
	for(int i=0; i<N; ++i){
		addChoiceClause(mng, dir.newAnonymousVar(), dir.getArr("C3", i), dir.getArr("tmp1",i), dir.getArr("tmp2",i));
	}
	dir.declareArr("OUTS1", N);
	for(int i=0; i<N; ++i){
		addChoiceClause(mng, dir.getArr("OUTS1", i), dir.getArr("C4", i), tmp3+i, dir.getArr(IN,i));
	}

	int controls[N];
	for(int i=0; i<N; ++i){
		varRange vr = getSwitchVars(mng, dir, dir.getArr("loopvar", i*logN), logN );
		controls[i] = vr.varID;
	}
	
	
	int out1 = arbitraryPerm(mng, dir, dir.getArr("OUTS1", 0) , N/2,  controls, N/2, N/2);
	int out2 = arbitraryPerm(mng, dir, dir.getArr("OUTS1", N/2) , N/2,  controls, N/2, N/2);	
	int ii=0;
	for(; ii<N/2; ++ii){
		addEqualsClause(mng, dir.getArr(SOUT, ii), out1+ii);	
	}
	for(int i=0; ii<N; ++ii, ++i){
		addEqualsClause(mng, dir.getArr(SOUT, ii), out2+i);
	}
}


void desIP::defineSpec(SAT_Manager mng, varDir& dir){
	Dout( cout<<"defineSpec()"<<endl );
	
	int p[64] = { 58, 50, 42, 34, 26, 18, 10, 2,
                   60, 52, 44, 36, 28, 20, 12, 4,
                   62, 54, 46, 38, 30, 22, 14, 6,
                   64, 56, 48, 40, 32, 24, 16, 8,
                   57, 49, 41, 33, 25, 17, 9, 1,
                   59, 51, 43, 35, 27, 19, 11, 3,
                   61, 53, 45, 37, 29, 21, 13, 5,
                   63, 55, 47, 39, 31, 23, 15, 7
    };

    for (int i = 0; i < 64; ++i){
      addEqualsClause(mng, dir.getArr(OUT, i), dir.getArr(IN, p[i] - 1) );
    }
}






int main(int argc, char ** argv)
{
	desIP bswap(atoi(argv[1]));
	bswap.setup();
	bswap.solve();

	return 0;
}

