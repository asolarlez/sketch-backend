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
	ctrl = NULL;
	nseeds = 1;
	Nin = 0;
	Nout = 0;
	randseed = time(NULL);
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
	int status = assertVectorsDiffer(mng, dir, dir.getArr(SOUT, 0), dir.getArr(OUT ,0), N);
	setVarClause(mng, status);
}


int FindCheckSolver::getInSize(){
	return dirCheck.getArrSize(IN);
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
	if(controlSize>0){
		Dout(cout<<"Control vars have size"<<controlSize<<endl);
		SAT_DeleteClauseGroup(mngCheck, 2);
	}
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
    cout<<"# CHECK DIAGNOSTICS"<<endl;
	printDiagnostics(mngCheck, 'c');
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
	Dout(cout<<"DECLARING CONTROL "<<ctrl<<" "<<size<<endl);
	controlVars[ctrl] = size;
	controlSize += size;
}



void FindCheckSolver::addInputsToTestSet(int input[], int insize){
	Dout( cout<<"find()"<<endl );
	defineSketch(mngFind, dirFind);
	defineSpec(mngFind, dirFind);	
	dirFind.makeArrNoBranch(IN);
	Dout( cout<<"____"<<endl );
	addEqualsClauses(mngFind, dirFind);
	
	int N = dirFind.getArrSize(IN);
//Set the values of the inputs from input[];
	Dout( cout<<"____"<<endl );
	for(int i=0; i<N; ++i){
		setVarClause(mngFind,
		input[i%insize]*dirFind.getArr(IN, i));
	}
	Dout( cout<<"done adding inputs"<<flush<<endl);
}



bool FindCheckSolver::find(int input[], int insize, int controls[]){
		
					unsigned long long l_f_time;
					struct timeval stime, endtime;
					Dtime(gettimeofday(&stime, NULL) );
	addInputsToTestSet(input, insize);
					Dtime(gettimeofday(&endtime, NULL) );
					Dtime( l_f_time =1000000*(endtime.tv_sec - stime.tv_sec)+
					   (endtime.tv_usec - stime.tv_usec) );
					Dtime(cout<<"* TIME TO ADD INPUT "<<(l_f_time/1000.0)<<endl);
//Solve
	int result = SAT_Solve(mngFind);
  	cout<<"# FIND DIAGNOSTICS"<<endl;
	printDiagnostics(mngFind, 'f');
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

bool FindCheckSolver::solve(){
	int inputSize = getInSize();
	int ctrlSize = getCtrlSize();
	
	if(ctrl != NULL){
		delete [] ctrl;
		ctrl = NULL;
	}
	ctrl = new int[ctrlSize];

	time_t tm;
	
	srand(randseed);
	bool fail = false;
 	bool isDone;
	{ 
		int tmp[inputSize];
		for(int ns = 0; ns < nseeds; ++ns){
			for(int i=0; i< inputSize; ++i){
				tmp[i] = (rand() & 0x1) > 0? -1 : 1;
				Dout( cout<<"seedInput["<<i<<"]=\t"<<tmp[i]<<"; "<<endl );
			}
			if( ns < nseeds-1 ){
				addInputsToTestSet(tmp, inputSize);
			}
		}
		{
			struct timeval stime, endtime;
			Dtime(gettimeofday(&stime, NULL) );
			isDone = find(tmp, inputSize, ctrl);
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
	int * input = new int[inputSize];
	cout<<"inputSize = "<<inputSize<<endl;
	Dtime( unsigned long long check_time=0 );
	Dtime( unsigned long long find_time=0 );
	int iterations = 0;
	while(isDone){
		unsigned long long l_f_time, l_c_time;
		Dout( for(int i=0; i<ctrlSize; ++i) cout<<"		ctrl["<<i<<"]="<<ctrl[i]<<endl; cout<<"-----------------------------"<<endl );
		cout<<"!+";
		for(int i=0; i<ctrlSize; ++i) cout<<"\t"<<(ctrl[i]==1?1:0);
		cout<<endl;
		{
			struct timeval stime, endtime;
			cout<<"BEG CHECK"<<endl;
			Dtime(gettimeofday(&stime, NULL) );
		isDone = check(ctrl, ctrlSize, input);
			Dtime(gettimeofday(&endtime, NULL) );
			Dtime( l_c_time =1000000*(endtime.tv_sec - stime.tv_sec)+
				   (endtime.tv_usec - stime.tv_usec); check_time += l_c_time );
			cout<<"END CHECK"<<endl;			
		}		
		
		if(isDone){
			cout<<"!%";
			for(int i=0; i<inputSize; ++i) cout<<"\t"<<(input[i]==1?1:0);
			cout<<endl;
			Dout( for(int i=0; i<inputSize; ++i) cout<<"		input["<<i<<"]="<<input[i]<<endl; cout<<"-----------------------------"<<endl );
			bool keepGoing;
			{
				struct timeval stime, endtime;
				cout<<"BEG FIND"<<endl;
				Dtime(gettimeofday(&stime, NULL) );
			 	keepGoing = find(input, inputSize, ctrl);
				Dtime(gettimeofday(&endtime, NULL) );
				Dtime( l_f_time =1000000*(endtime.tv_sec - stime.tv_sec)+
				   (endtime.tv_usec - stime.tv_usec); find_time +=l_f_time );
			   cout<<"END FIND"<<endl;
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
	
	delete [] input;	
	if(!fail){
		cout<<" *GOT THE CORRECT ANSWER IN "<<iterations<<" iterations."<<endl;		
	}else{
		cout<<" *FAILED IN "<<iterations<<" iterations."<<endl;
	}
	
	cout<<" *"<<"FIND TIME "<<(find_time/1000)<<", CHECK TIME "<<(check_time/1000)<<endl;
	return !fail;
}

void FindCheckSolver::printDiagnostics(){
	cout<<"# STATS FOR FINDER"<<endl;
	printDiagnostics(this->mngFind, 'f');	
	cout<<"# STATS FOR CHECKER"<<endl;
	printDiagnostics(this->mngCheck, 'c');	
}


void FindCheckSolver::printDiagnostics(SAT_Manager mng, char c){
    cout << c << "# Random Seed Used\t\t\t\t" << SAT_Random_Seed(mng) << endl;
    cout << c << "# Max Decision Level\t\t\t\t" << SAT_MaxDLevel(mng) << endl;
    cout << c << "# Num. of Decisions\t\t\t\t" << SAT_NumDecisions(mng)<< endl;
    cout << c << "# ( Stack + Vsids + Shrinking Decisions )\t\t" <<SAT_NumDecisionsStackConf(mng);
    cout << c << "#  + " <<SAT_NumDecisionsVsids(mng)<<" + "<<SAT_NumDecisionsShrinking(mng)<<endl;
    cout << c << "# Original Num Variables\t\t\t\t" << SAT_NumVariables(mng) << endl;
    cout << c << "# Original Num Clauses\t\t\t\t" << SAT_InitNumClauses(mng) << endl;
    cout << c << "# Original Num Literals\t\t\t\t" << SAT_InitNumLiterals(mng) << endl;
    cout << c << "# Added Conflict Clauses\t\t\t\t" << SAT_NumAddedClauses(mng)- SAT_InitNumClauses(mng)<< endl;
    cout << c << "# Num of Shrinkings\t\t\t\t" << SAT_NumShrinkings(mng)<< endl;
    cout << c << "# Deleted Conflict Clauses\t\t\t" << SAT_NumDeletedClauses(mng)-SAT_NumDelOrigCls(mng) <<endl;
    cout << c << "# Deleted Clauses\t\t\t\t\t" << SAT_NumDeletedClauses(mng) <<endl;
    cout << c << "# Added Conflict Literals\t\t\t\t" << SAT_NumAddedLiterals(mng) - SAT_InitNumLiterals(mng) << endl;
    cout << c << "# Deleted (Total) Literals\t\t\t" << SAT_NumDeletedLiterals(mng) <<endl;
    cout << c << "# Number of Implication\t\t\t\t" << SAT_NumImplications(mng)<< endl;
    //other statistics comes here
    cout << c << "# Total Run Time\t\t\t\t\t" << SAT_GetCPUTime(mng) << endl;	
}



