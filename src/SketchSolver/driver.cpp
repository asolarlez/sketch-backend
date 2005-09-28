
#include "FindCheckSolver.h"


class BitSwap: public FindCheckSolver{
	int N;
	protected:
	void defineSketch(SAT_Manager mng, varDir& dir);
	void defineSpec(SAT_Manager mng, varDir& dir);
	public:
	BitSwap(int N_p, int SN){
		N = N_p;
		declareControl("C", N);
		declareControl("shamt", SN);
	}
};

void BitSwap::defineSketch(SAT_Manager mng, varDir& dir){
	dir.declareArr(IN, N);
	dir.declareArr(SOUT, N);
	dir.declareArr(OUT, N);
	dir.declareArr("T1p", N);
	dir.declareArr("T1", N);
	dir.declareArr("T2", N);
	dir.declareArr("Tsp", N);
	dir.declareArr("Tsm", N);
	
	cout<<" Tsm = IN << shamt;"<<endl;
	shiftArrNdet(mng, dir, "Tsm", IN, "shamt", RIGHT);


	// t1'[i] = in[i] xor Tsm[i];
	cout<<" t1p = in xor Tsm; "<<endl;
	for(int i=0; i<N; ++i){
		addXorClause(mng, dir.getArr("T1p", i), dir.getArr(IN, i), dir.getArr("Tsm" ,i));
	}
	// t1[i] = t1p[i] & c[i]
	cout<<" t1 = t1p & c "<<endl;
	for(int i=0; i<N; ++i){
		addAndClause(mng, dir.getArr("T1", i), dir.getArr("T1p", i), dir.getArr("C" ,i));
	}

	cout<<"Tsp = t1 >> shamt;"<<endl;
	shiftArrNdet(mng, dir, "Tsp", "T1", "shamt", LEFT);
	
	// t2[i] = t1[i] xor Tsp[i]
	cout<<" t2 = t1 xor tsp "<<endl;
	for(int i=0; i<N; ++i){
		addXorClause(mng, dir.getArr("T2", i), dir.getArr("T1", i), dir.getArr("Tsp" ,i));
	}
	
	// SOUTq[i] = t2[i] xor in[i];
	cout<<" SOUT = t2 xor in; "<<endl;
	for(int i=0; i<N; ++i){
		addXorClause(mng, dir.getArr(SOUT, i), dir.getArr("T2", i), dir.getArr(IN ,i));
	}
}

void BitSwap::defineSpec(SAT_Manager mng, varDir& dir){
	Dout( cout<<"defineSpec()"<<endl );
	for(int i=0; i<N; i+=5){
//		addEqualsClause(mng, dir.getArr(OUT, i+0), dir.getArr(IN, i+0));	
//		addEqualsClause(mng, dir.getArr(OUT, i+2), dir.getArr(IN, i+1));	
//		addEqualsClause(mng, dir.getArr(OUT, i+1), dir.getArr(IN, i+2));	
//		addEqualsClause(mng, dir.getArr(OUT, i+3), dir.getArr(IN, i+3));	
		addEqualsClause(mng, dir.getArr(OUT, i+0), dir.getArr(IN, i+0));	
		addEqualsClause(mng, dir.getArr(OUT, i+3), dir.getArr(IN, i+1));	
		addEqualsClause(mng, dir.getArr(OUT, i+4), dir.getArr(IN, i+2));
		addEqualsClause(mng, dir.getArr(OUT, i+1), dir.getArr(IN, i+3));
		addEqualsClause(mng, dir.getArr(OUT, i+2), dir.getArr(IN, i+4));
	}
}






int main(int argc, char ** argv)
{
	int N = 5*atoi(argv[1]);
	BitSwap bswap(N, 3);
	bswap.setup();
	bswap.solve();

	return 0;
}










