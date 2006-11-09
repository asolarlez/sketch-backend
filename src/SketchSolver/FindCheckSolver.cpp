#include "FindCheckSolver.h"
#include "timerclass.h"
#include <ctime>

#ifdef IN
#undef IN
#endif

#ifdef OUT
#undef OUT
#endif

#define Dtime( out ) out

//#define WITH_RANDOMNESS 1

FindCheckSolver::FindCheckSolver(SATSolver& finder, SATSolver& checker):mngFind(finder), mngCheck(checker), IN("_IN"), OUT("_OUT"), SOUT("_SOUT"), dirFind(finder), dirCheck(checker){
	cout<<"CONSTRUCTING "<<IN<<", "<<SOUT<<", "<<OUT<<endl;
	////////////////////////////
	controlSize = 0;
	ctrl = NULL;
	nseeds = 1;
	Nin = 0;
	Nout = 0;
	iterlimit = -1;
	randseed = time(NULL);
	doCheckpoint = false;
}

void FindCheckSolver::setIterLimit(int p_iterlimit){
	iterlimit = p_iterlimit;
	cout<<" terminate after "<<iterlimit<<" iterations no matter what. This option is mainly for debugging"<<endl;
}


void FindCheckSolver::addEqualsClauses(SATSolver& mng, varDir& dir){
	int N = dir.getArrSize(OUT);
	Assert( N == dir.getArrSize(SOUT), "SIZE MISSMATCH "<<N );
	for(int i=0; i<N; ++i){
		dir.addEquateClause(dir.getArr(SOUT, i), dir.getArr(OUT, i));
	}
}


void FindCheckSolver::addDiffersClauses(SATSolver& mng, varDir& dir){
	int N = dir.getArrSize(OUT);
	int status = dir.assertVectorsDiffer(dir.getArr(SOUT, 0), dir.getArr(OUT ,0), N);
	mng.assertVarClause(status);
}


int FindCheckSolver::getInSize(){
	return dirCheck.getArrSize(IN);
}

int FindCheckSolver::getCtrlSize(){
	return controlSize;
}

void FindCheckSolver::buildChecker(){
	mngCheck.clean();
	dirCheck.reset();
	for(map<string, int>::iterator it = controlVars.begin(); it !=controlVars.end(); ++it){
		const string& cname = it->first;
		dirCheck.declareInArr(cname, it->second);
	}
	defineSketch(mngCheck, dirCheck);
	defineSpec(mngCheck, dirCheck);
	addDiffersClauses(mngCheck, dirCheck);				
}


void FindCheckSolver::setupCheck(){
	Dout( cout<<"setupCheck()"<<endl );
//Declare the control variables.
	buildChecker();
	int jj=0;
	for(map<string, int>::iterator it = controlVars.begin(); it !=controlVars.end(); ++it){
		const string& cname = it->first;
		int cnt = dirCheck.getArrSize(cname);
		for(int i=0; i<cnt; ++i, ++jj){
			mngCheck.setVarClause( -dirCheck.getArr(cname, i));
		}
	}
}

void FindCheckSolver::setNewControls(int controls[], int ctrlsize){
	if(controlSize>0){
		Dout(cout<<"Control vars have size"<<controlSize<<endl);
		mngCheck.deleteClauseGroup(2);
	}
	int jj=0;
	for(map<string, int>::iterator it = controlVars.begin(); it !=controlVars.end(); ++it){
		const string& cname = it->first;
		int cnt = dirCheck.getArrSize(cname);
		for(int i=0; i<cnt; ++i, ++jj){
			mngCheck.setVarClause(controls[jj%ctrlsize]*dirCheck.getArr(cname, i));
		}
	}
}


bool FindCheckSolver::check(int controls[], int ctrlsize, int input[]){
	Dout( cout<<"check()"<<endl );
	timerclass tc("* TIME TO ADD CONTROLS ");
	tc.start();				
	setNewControls(controls, ctrlsize);
	tc.stop().print();
	
    int result = mngCheck.solve();
    cout<<"# CHECK DIAGNOSTICS"<<endl;
	printDiagnostics(mngCheck, 'c');
    if (result != SATSolver::SATISFIABLE){
    	if( result != SATSolver::UNSATISFIABLE){
	    	switch( result ){
	    	   	case SATSolver::UNDETERMINED: throw new SolverException(result, "UNDETERMINED"); break;
	    		case SATSolver::TIME_OUT: throw new SolverException(result, "UNDETERMINED"); break;
	    		case SATSolver::MEM_OUT:  throw new SolverException(result, "MEM_OUT"); break;
	    		case SATSolver::ABORTED:  throw new SolverException(result, "ABORTED"); break;
	    	}
    	}
    	return false;
    }
    int N = dirCheck.getArrSize(IN);
	for(int i=0; i<N; ++i){
		int val = mngCheck.getVarVal(dirCheck.getArr(IN, i));
		if( val == 1) input[i]= 1;
		else input[i]= -1;
	}
	Dout( dirCheck.print() );
	mngCheck.reset();
	return true;
}

void FindCheckSolver::setupFind(){
	Dout( cout<<"setupFind()"<<endl );
	//Declare the control variables.
	for(map<string, int>::iterator it = controlVars.begin(); it !=controlVars.end(); ++it){
		const string& cname = it->first;
		dirFind.declareInArr(cname, it->second);
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
		mngFind.setVarClause(input[i%insize]*dirFind.getArr(IN, i));
	}
	Dout( cout<<"done adding inputs"<<flush<<endl);
}



bool FindCheckSolver::find(int input[], int insize, int controls[]){
		
	timerclass tc("* TIME TO ADD INPUT ");
	tc.start();				
	addInputsToTestSet(input, insize);
	tc.stop().print();
	
//Solve
	int result = mngFind.solve();
  	cout<<"# FIND DIAGNOSTICS"<<endl;
	printDiagnostics(mngFind, 'f');
    if (result != SATSolver::SATISFIABLE){ 	//If solve is bad, return false.    	
    	if( result != SATSolver::UNSATISFIABLE){
	    	switch( result ){
	    	   	case SATSolver::UNDETERMINED: throw new SolverException(result, "UNDETERMINED"); break;
	    		case SATSolver::TIME_OUT: throw new SolverException(result, "UNDETERMINED"); break;
	    		case SATSolver::MEM_OUT:  throw new SolverException(result, "MEM_OUT"); break;
	    		case SATSolver::ABORTED:  throw new SolverException(result, "ABORTED"); break;
	    	}    			
    	}
    	return false;
    }
	Dout( dirFind.print() );
	
//Get the values of the Controls.
	int jj=0;
	for(map<string, int>::iterator it = controlVars.begin(); it !=controlVars.end(); ++it){
		const string& cname = it->first;
		int cnt = dirFind.getArrSize(cname);
		Assert( cnt == it->second, "SIZE MISMATCH: "<<cnt<<" != "<<it->second<<endl);
		for(int i=0; i<cnt; ++i, ++jj){
			int val = mngFind.getVarVal(dirFind.getArr(cname, i));
			if( val == 1) controls[jj]= 1;
			else controls[jj]= -1;
		}
	}
	mngFind.reset();
	return true;
//Return true.
}
	
void FindCheckSolver::setup(){
	setupFind();
	setupCheck();
}

void  FindCheckSolver::checkpoint(char nm, int * ar, int sz){
	if( doCheckpoint ){
		cptfile<<nm;
		for(int i=0; i<sz; ++i){
			cptfile<<(ar[i]==1?1:0);
		}
		cptfile<<endl;
	}
}

void FindCheckSolver::setCheckpoint(const string& filename){
	cptfile.open(filename.c_str());
	doCheckpoint = true;
	cout<<"checkpointing to "<<filename<<endl;
}

bool FindCheckSolver::solveFromCheckpoint(istream& in){
	timerclass ctimer("* CHECK TIME");
	int inputSize = getInSize();
	int ctrlSize = getCtrlSize();
	if(ctrl != NULL){
		delete [] ctrl;
		ctrl = NULL;
	}
	ctrl = new int[ctrlSize];
	int * input = new int[inputSize];
	int maxSize = (ctrlSize>inputSize? ctrlSize : inputSize)+2;
	char* buff = new char[maxSize];
	char last = 'n';
	bool unaddedInput = false;
	while(in){
		in.getline(buff, maxSize);
		if( buff[0] == '\0' ) continue;
		if(buff[0] == 'f'){
			if( unaddedInput ){ addInputsToTestSet(input, inputSize); }
			for(int i=0; i<inputSize; ++i){
				Assert( buff[i+1] == '0' || buff[i+1] == '1' , "CORRUPTED FILE f "<<i<<" of "<<inputSize<<"  "<<buff[i+1]);
				input[i] = buff[i+1]=='1'? 1 : -1;
			}
			unaddedInput = true;
			last = 'f';
		}else if(buff[0] == 'c'){
			for(int i=0; i<ctrlSize; ++i){
				Assert( buff[i+1] == '0' || buff[i+1] == '1' , "CORRUPTED FILE c "<<i<<" of "<<ctrlSize<<"  "<<buff[i+1]);
				ctrl[i] = buff[i+1]=='1'? 1 : -1;
			}
			last = 'c';
		}else Assert(false, "CORRUPTED FILE c "<<buff[0]);
	}
	bool succeeded ;
	if( last == 'f' ){
		Assert( unaddedInput , "This is not possible ");
		succeeded = solve(input, inputSize);
	}else if(last == 'c'){
		if( unaddedInput ){ addInputsToTestSet(input, inputSize); }
		bool doMore;
		{ // Check
			cout<<"!+";	for(int i=0; i<ctrlSize; ++i) cout<<"\t"<<(ctrl[i]==1?1:0);	cout<<endl;
			cout<<"BEG CHECK"<<endl; ctimer.restart();
			doMore = check(ctrl, ctrlSize, input);
			ctimer.stop(); cout<<"END CHECK"<<endl;
			ctimer.print();
		}
		
		if(doMore){
			succeeded = solve(input, inputSize);
		}else{
			cout<<" *GOT THE CORRECT ANSWER IN 0 iterations."<<endl;		
			succeeded = true;
		}
	}else Assert(false, "EMPTY FILE");
	return succeeded;
}


bool FindCheckSolver::solve(int * input, int inputSize){
	int ctrlSize = controlSize;
	int iterations = 0;
	bool fail = false;
 	bool doMore=true;
	timerclass ftimer("* FIND TIME");
	timerclass ctimer("* CHECK TIME");
	while(doMore){
		{// Find
			cout<<"!%";	for(int i=0; i<inputSize; ++i) cout<<"\t"<<(input[i]==1?1:0); cout<<endl;
			checkpoint('f', input, inputSize);
			cout<<"BEG FIND"<<endl; ftimer.restart();
			doMore = find(input, inputSize, ctrl);
			ftimer.stop(); cout<<"END FIND"<<endl;
			if(!doMore){
				cout<<"******** FAILED ********"<<endl;	
				ftimer.print();	ctimer.print();
				fail = true;
				break;
			}
		}
		
		{ // Check
			cout<<"!+";	for(int i=0; i<ctrlSize; ++i) cout<<"\t"<<(ctrl[i]==1?1:0);	cout<<endl;
			checkpoint('c', ctrl, ctrlSize);
			cout<<"BEG CHECK"<<endl; ctimer.restart();
			doMore = check(ctrl, ctrlSize, input);
			ctimer.stop(); cout<<"END CHECK"<<endl;
		}
		cout<<"********  "<<iterations<<"\tftime="<<ftimer.get_cur_ms() <<"\tctime="<<ctimer.get_cur_ms()<<endl;
		++iterations;
		if( iterlimit > 0 && iterations >= iterlimit){ cout<<" * bailing out due to iter limit"<<endl; fail = true; break; }
	}

	if(!fail){
		cout<<" *GOT THE CORRECT ANSWER IN "<<iterations<<" iterations."<<endl;		
	}else{
		cout<<" *FAILED IN "<<iterations<<" iterations."<<endl;
	}
	cout<<" *"<<"FIND TIME "<<ftimer.get_tot_ms()<<", CHECK TIME "<<ctimer.get_tot_ms()<<endl;
	return !fail;
}


bool FindCheckSolver::solve(){
	int inputSize = getInSize();
	int ctrlSize = getCtrlSize();
	Assert( controlSize == ctrlSize, "This is crazy!!");
	cout<<"inputSize = "<<inputSize<<"\tctrlSize = "<<ctrlSize<<endl;
	if(ctrl != NULL){
		delete [] ctrl;
		ctrl = NULL;
	}
	ctrl = new int[ctrlSize];
	int * input = new int[inputSize];
	srand(randseed);
	for(int i=0; i< inputSize; ++i){
		input[i] = (rand() & 0x1) > 0? -1 : 1;
	}
	for(int ns = 0; ns < (nseeds-1); ++ns){			
		cout<<"!%";	for(int i=0; i<inputSize; ++i) cout<<"\t"<<(input[i]==1?1:0); cout<<endl;
		checkpoint('f', input, inputSize);
		addInputsToTestSet(input, inputSize);		
		for(int i=0; i< inputSize; ++i){
			input[i] = (rand() & 0x1) > 0? -1 : 1;			
		}
	}
	
	bool succeeded = solve(input, inputSize);
	
	delete [] input;	
	return succeeded;
}


void FindCheckSolver::printDiagnostics(){
	cout<<"# STATS FOR FINDER"<<endl;
	printDiagnostics(this->mngFind, 'f');	
	cout<<"# STATS FOR CHECKER"<<endl;
	printDiagnostics(this->mngCheck, 'c');	
}


void FindCheckSolver::printDiagnostics(SATSolver& mng, char c){
   mng.printDiagnostics(c);
}



