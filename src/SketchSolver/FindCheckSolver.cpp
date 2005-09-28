#include "FindCheckSolver.h"
#include <sys/time.h>


#define Dtime( out ) out

//#define WITH_RANDOMNESS 1

FindCheckSolver::FindCheckSolver():IN("_IN"), OUT("_OUT"), SOUT("_SOUT"), dirFind(mngFind), dirCheck(mngCheck){
	cout<<"CONSTRUCTING "<<IN<<", "<<SOUT<<", "<<OUT<<endl;
	mngFind = SAT_InitManager();
	SAT_SetNumVariables(mngFind, 0);		
	dirFind.setMng(mngFind);	
	////////////////////////////
	mngCheck = SAT_InitManager();
	SAT_SetNumVariables(mngCheck, 0);
	dirCheck.setMng(mngCheck);
	controlSize = 0;
	nseeds = 1;
}



void FindCheckSolver::addEqualsClauses(SAT_Manager mng, varDir& dir){
	int N = dir.getArrSize(OUT);
	Assert( N == dir.getArrSize(SOUT), "SIZE MISSMATCH "<<N );
	for(int i=0; i<N; ++i){
		addEqualsClause(mng, dir.getArr(SOUT, i), dir.getArr(OUT, i));	
	}
}

void FindCheckSolver::addDiffersClauses(SAT_Manager mng, varDir& dir){
	int N = dir.getArrSize(OUT);
	int startIdx = dir.getVarCnt();
	// tmp[i] = SOUT[i] != OUT[i];
	for(int i=0; i<N; ++i){
		addXorClause(mng, dir.newAnonymousVar(), dir.getArr(SOUT, i), dir.getArr(OUT ,i));
	}
	int finIdx = dir.getVarCnt();
	// tmp2[0] = tmp[0];
	addEqualsClause(mng, dir.newAnonymousVar(), startIdx);
	
	// tmp2[i] = tmp2[i-1] | tmp[i]
	for(int i=1; i<N; ++i){
		Assert( dir.getVarCnt() == finIdx + i, "THIS SHOULD NOT HAPPEN");
		addOrClause(mng, dir.newAnonymousVar() , finIdx + i-1, startIdx+i);
	}	
	// assert tmp2[N-1];
	setVarClause(mng, finIdx + N-1);
}


int FindCheckSolver::getInSize(){
	return dirFind.getArrSize(IN);
}

int FindCheckSolver::getCtrlSize(){
	return controlSize;
}



void FindCheckSolver::setupCheck(){
	Dout( cout<<"setupCheck()"<<endl );
//Declare the control variables.
	for(map<string, int>::iterator it = controlVars.begin(); it !=controlVars.end(); ++it){
		const string& cname = it->first;
		dirCheck.declareArr(cname, it->second);
	}
	defineSketch(mngCheck, dirCheck);
	defineSpec(mngCheck, dirCheck);
	addDiffersClauses(mngCheck, dirCheck);			
	int jj=0;
	for(map<string, int>::iterator it = controlVars.begin(); it !=controlVars.end(); ++it){
		const string& cname = it->first;
		int cnt = dirCheck.getArrSize(cname);
		for(int i=0; i<cnt; ++i, ++jj){
			setVarClause(mngCheck, -dirCheck.getArr(cname, i), 2);
		}
	}
}


bool FindCheckSolver::check(int controls[], int ctrlsize, int input[]){
	Dout( cout<<"check()"<<endl );
	SAT_DeleteClauseGroup(mngCheck, 2);
	int jj=0;
	for(map<string, int>::iterator it = controlVars.begin(); it !=controlVars.end(); ++it){
		const string& cname = it->first;
		int cnt = dirCheck.getArrSize(cname);
		for(int i=0; i<cnt; ++i, ++jj){
			setVarClause(mngCheck, controls[jj%ctrlsize]*dirCheck.getArr(cname, i), 2);
		}
	}
//	SAT_Reset(mngCheck);
    int result = SAT_Solve(mngCheck);
    if (result != SATISFIABLE) 
    	return false;
    int N = dirCheck.getArrSize(IN);
	for(int i=0; i<N; ++i){
		int val = SAT_GetVarAsgnment(mngCheck, dirCheck.getArr(IN, i));
		if( val == 1) input[i]= 1;
		else input[i]= -1;
	}
	Dout( dirCheck.print() );
	SAT_Reset(mngCheck);
	return true;
}
		
void FindCheckSolver::setupFind(){
	Dout( cout<<"setupFind()"<<endl );
	//Declare the control variables.
	for(map<string, int>::iterator it = controlVars.begin(); it !=controlVars.end(); ++it){
		const string& cname = it->first;
		dirFind.declareArr(cname, it->second);
	}
}


void FindCheckSolver::declareControl(const string& ctrl, int size){
	controlVars[ctrl] = size;
	controlSize += size;
}



void FindCheckSolver::addInputsToTestSet(int input[], int insize){
	Dout( cout<<"find()"<<endl );
	defineSketch(mngFind, dirFind);
	defineSpec(mngFind, dirFind);	
	Dout( cout<<"____"<<endl );
	addEqualsClauses(mngFind, dirFind);
	
	int N = dirFind.getArrSize(IN);
//Set the values of the inputs from input[];
	Dout( cout<<"____"<<endl );
	for(int i=0; i<N; ++i){
		setVarClause(mngFind,
		#ifdef WITH_RANDOMNESS	 
		((rand() & 0x7) > 0? 1 : -1)*
		#endif 	
		input[i%insize]*dirFind.getArr(IN, i));
	}
}



bool FindCheckSolver::find(int input[], int insize, int controls[]){	
	addInputsToTestSet(input, insize);
//Solve
	int result = SAT_Solve(mngFind);
    if (result != SATISFIABLE) 	//If solve is bad, return false.
	    	return false;
	Dout( dirFind.print() );
//Get the values of the Controls.
	int jj=0;
	for(map<string, int>::iterator it = controlVars.begin(); it !=controlVars.end(); ++it){
		const string& cname = it->first;
		int cnt = dirFind.getArrSize(cname);
		Assert( cnt == it->second, "SIZE MISMATCH: "<<cnt<<" != "<<it->second<<endl);
		for(int i=0; i<cnt; ++i, ++jj){
			int val = SAT_GetVarAsgnment(mngFind, dirFind.getArr(cname, i));
			if( val == 1) controls[jj]= 1;
			else controls[jj]= -1;
		}
	}
	SAT_Reset(mngFind);
	return true;
//Return true.
}
	
void FindCheckSolver::setup(){
	setupFind();
	setupCheck();
}

void FindCheckSolver::solve(){
	int ctrlSize = getCtrlSize();
	
	int * ctrl = new int[ctrlSize];
	
	time_t tm;
	
	srand(time(&tm));
	bool fail = false;
 	bool isDone;
	{ 
		int tmp[ctrlSize];
		for(int ns = 0; ns < nseeds; ++ns){
			for(int i=0; i< ctrlSize; ++i){
				tmp[i] = (rand() & 0x1) > 0? -1 : 1;
				Dout( cout<<"seedInput["<<i<<"]=\t"<<tmp[i]<<"; "<<endl );
			}
			if( ns < nseeds-1 ){
				addInputsToTestSet(tmp, ctrlSize);
			}
		}
		{
			struct timeval stime, endtime;
			Dtime(gettimeofday(&stime, NULL) );
		isDone = find(tmp, ctrlSize, ctrl);
			Dtime(gettimeofday(&endtime, NULL) );
			Dtime( unsigned long long tott = 1000000*(endtime.tv_sec - stime.tv_sec)+
				   (endtime.tv_usec - stime.tv_usec) );
	 		Dtime( printf(" *SETUP TIME %d   \n", tott/1000) );
		}
		if(!isDone){
			cout<<"******** FAILED ********"<<endl;	
			fail = true;
		}
	}
	int inputSize = getInSize();
	int * input = new int[inputSize];
	cout<<"inputSize = "<<inputSize<<endl;
	Dtime( unsigned long long check_time=0 );
	Dtime( unsigned long long find_time=0 );
	int iterations = 0;
	while(isDone){
		unsigned long long l_f_time, l_c_time;
		Dout( for(int i=0; i<ctrlSize; ++i) cout<<"		ctrl["<<i<<"]="<<ctrl[i]<<endl; cout<<"-----------------------------"<<endl );
		cout<<"+";
		for(int i=0; i<ctrlSize; ++i) cout<<"\t"<<(ctrl[i]==1?1:0);
		cout<<endl;
		{
			struct timeval stime, endtime;
			Dtime(gettimeofday(&stime, NULL) );
		isDone = check(ctrl, ctrlSize, input);
			Dtime(gettimeofday(&endtime, NULL) );
			Dtime( l_c_time =1000000*(endtime.tv_sec - stime.tv_sec)+
				   (endtime.tv_usec - stime.tv_usec); check_time += l_c_time );
		}		
		
		if(isDone){
			Dout( for(int i=0; i<inputSize; ++i) cout<<"		input["<<i<<"]="<<input[i]<<endl; cout<<"-----------------------------"<<endl );
			bool keepGoing;
			{
				struct timeval stime, endtime;
				Dtime(gettimeofday(&stime, NULL) );
			 	keepGoing = find(input, inputSize, ctrl);
				Dtime(gettimeofday(&endtime, NULL) );
				Dtime( l_f_time =1000000*(endtime.tv_sec - stime.tv_sec)+
				   (endtime.tv_usec - stime.tv_usec); find_time +=l_f_time );
			}
			if( !keepGoing){
				cout<<"******** FAILED ********"<<endl;
				fail = true;
				break;
			}
		}
		cout<<"********  "<<iterations<<"\tftime="<<(l_f_time/1000.0)<<"\tctime="<<(l_c_time/1000.0)<<endl;
		++iterations;
	}
	
	delete [] ctrl;
	delete [] input;	
	if(!fail){
		cout<<" *GOT THE CORRECT ANSWER IN "<<iterations<<" iterations."<<endl;
	}else{
		cout<<" *FAILED IN "<<iterations<<" iterations."<<endl;
	}
	
	cout<<" *"<<"CHECK TIME "<<(check_time/1000)<<", FIND TIME "<<(find_time/1000)<<endl;
	
/*	
	SAT_Reset(mngCheck);
	cout<<" NClauses= "<<SAT_NumClauses(mngCheck)<<endl;
	int cl_idx;
	for (cl_idx = SAT_GetFirstClause (mngCheck); cl_idx >= 0; 
	          cl_idx = SAT_GetNextClause(mngCheck, cl_idx)) {
	        int len = SAT_GetClauseNumLits(mngCheck, cl_idx);
	        int * lits = new int[len+1];
	        SAT_GetClauseLits( mngCheck, cl_idx, lits);
	        int i;
	        for (i=0; i< len; ++i) {
	            int v_idx = lits[i] >> 1;
	            int sign = lits[i] & 0x1;
	            cout<<(sign>0?"":"-")<<v_idx<<", ";
//	            int var_value = SAT_GetVarAsgnment( mng, v_idx);
//	            if( (var_value == 1 && sign == 0) ||
//	                (var_value == 0 && sign == 1) ) break;
	        }
	        cout<<endl;
	        delete [] lits;
	 }
	 cout<<"---"<<cl_idx<<endl;
*/
}


