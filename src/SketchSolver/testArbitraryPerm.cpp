#include "FindCheckSolver.h"


class testA: public FindCheckSolver{
	int N;
	int logN;
	protected:
	void defineSketch(SAT_Manager mng, varDir& dir);
	void defineSpec(SAT_Manager mng, varDir& dir);

	public:
	testA(int NS_p){
		N = 5;
		Nin = N;
		Nout = N;
		int k=1;
		int size=0;
		while(k<(N)){
			k=k*2;
			++size;
		}
		logN = size;
		cout<<"N="<<N<<" logN="<<logN<<endl;
		declareControl("loopVar", N*logN);
		nseeds = NS_p;
	}
};






void testA::defineSketch(SAT_Manager mng, varDir& dir){
	dir.declareArr(IN, N);
	dir.declareArr(OUT, N);
	dir.declareArr(SOUT, N);
	
	int controls[N];
	for(int i=0; i<N; ++i){
		varRange vr = getSwitchVars(mng, dir, dir.getArr("loopVar", i*logN), logN );
		controls[i] = vr.varID;
	}
	
	
	int out1 = arbitraryPerm(mng, dir, dir.getArr(IN, 0) , N,  controls, N, N);

	int ii=0;
	for(; ii<N; ++ii){
		addEqualsClause(mng, dir.getArr(SOUT, ii), out1+ii);	
	}
}


void testA::defineSpec(SAT_Manager mng, varDir& dir){
	Dout( cout<<"defineSpec()"<<endl );
	addEqualsClause(mng, dir.getArr(OUT, 0), dir.getArr(IN, 4) );
	addEqualsClause(mng, dir.getArr(OUT, 1), dir.getArr(IN, 3) );
	addEqualsClause(mng, dir.getArr(OUT, 2), dir.getArr(IN, 2) );
	addEqualsClause(mng, dir.getArr(OUT, 3), dir.getArr(IN, 1) );
	addEqualsClause(mng, dir.getArr(OUT, 4), dir.getArr(IN, 0) );
}






int main(int argc, char ** argv)
{
	testA bswap(atoi(argv[1]));
	bswap.setup();
	bswap.solve();

	return 0;
}

