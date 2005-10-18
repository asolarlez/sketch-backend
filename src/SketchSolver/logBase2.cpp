#include "FindCheckSolver.h"


class logBase2: public FindCheckSolver{
	int N;
	int S;
	protected:
	void defineSketch(SAT_Manager mng, varDir& dir);
	void defineSpec(SAT_Manager mng, varDir& dir);
	
	public:
	logBase2(int N_p, int S_p, int NS_p){
		N = N_p;
		S = S_p;
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





void logBase2::defineSketch(SAT_Manager mng, varDir& dir){

}

void logBase2::defineSpec(SAT_Manager mng, varDir& dir){
	
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
	logBase2 bswap(N, 2*tmp, atoi(argv[2]));
	bswap.setup();
	bswap.solve();
	return 0;
}
