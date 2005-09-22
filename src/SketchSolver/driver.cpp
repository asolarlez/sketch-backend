
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
	

	
	cout<<" t1[0] = in[0] & c[0]; "<<endl;
	addAndClause(mng, dir.getArr("T1", 0), dir.getArr(IN, 0), dir.getArr("C" ,0));

	shiftArrNdet(mng, dir, "Tsm", IN, "shamt", RIGHT);


	// t1'[i] = in[i] xor Tsm[i];
	cout<<" t1'[i] = in[i] xor Tsm[i]; "<<endl;
	for(int i=1; i<N; ++i){
		addXorClause(mng, dir.getArr("T1p", i), dir.getArr(IN, i), dir.getArr("Tsm" ,i));
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
	
	// SOUTq[i] = t2[i] xor in[i];
	cout<<" SOUT[i] = t2[i] xor in[i]; "<<endl;
	for(int i=0; i<N; ++i){
		addXorClause(mng, dir.getArr(SOUT, i), dir.getArr("T2", i), dir.getArr(IN ,i));
	}
}

void BitSwap::defineSpec(SAT_Manager mng, varDir& dir){
	Dout( cout<<"defineSpec()"<<endl );
	for(int i=0; i<N; i+=3){
		addEqualsClause(mng, dir.getArr(OUT, i+0), dir.getArr(IN, i+0));	
		addEqualsClause(mng, dir.getArr(OUT, i+2), dir.getArr(IN, i+1));	
		addEqualsClause(mng, dir.getArr(OUT, i+1), dir.getArr(IN, i+2));
	}
}






int main(int argc, char ** argv)
{
	int N = 3*atoi(argv[1]);
	BitSwap bswap(N, 3);
	bswap.setup();
	bswap.solve();

	return 0;
}










