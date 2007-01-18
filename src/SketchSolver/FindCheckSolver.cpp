#include "FindCheckSolver.h"
#include "timerclass.h"
#include <ctime>
#include <queue>


#ifdef IN
#undef IN
#endif

#ifdef OUT
#undef OUT
#endif

#define Dtime( out ) out

//#define WITH_RANDOMNESS 1


////////////////////////////////////////////////////////////
/////////  	Checkpointing
////////////////////////////////////////////////////////////



void Checkpointer::resizeInput(const string& name, int newsz){
	if( doCheckpoint ){
		cptfile<<'r';
		cptfile<<newsz;
		cptfile<<endl;
	}
}


void  Checkpointer::checkpoint(char nm, vector<int>& ar){
	if( doCheckpoint ){
		cptfile<<nm;
		for(int i=0; i<ar.size(); ++i){
			cptfile<<(ar[i]==1?1:0);
		}
		cptfile<<endl;
	}
}


void Checkpointer::setCheckpoint(const string& filename){
	cptfile.open(filename.c_str());
	doCheckpoint = true;
	cout<<"checkpointing to "<<filename<<endl;
}



////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////



////////////////////////////////////////////////////////////
/////////  	FindCheckSolver
////////////////////////////////////////////////////////////


FindCheckSolver::FindCheckSolver(SATSolver& finder, SATSolver& checker):mngFind(finder), mngCheck(checker),  OUT("_OUT"), SOUT("_SOUT"), dirFind(finder), dirCheck(checker){
	cout<<"CONSTRUCTING "<<SOUT<<", "<<OUT<<endl;
	////////////////////////////	
	nseeds = 1;	
	Nout = 0;
	iterlimit = -1;
	randseed = time(NULL);	
}



/////////  	Core Methods
//public
bool FindCheckSolver::solve(){
	int inputSize = getInSize();
	int ctrlSize = getCtrlSize();
	
	cout<<"inputSize = "<<inputSize<<"\tctrlSize = "<<ctrlSize<<endl;
	
	ctrl.resize(ctrlSize);
	input.resize(inputSize);
	srand(randseed);
	for(int i=0; i< inputSize; ++i){
		input[i] = (rand() & 0x1) > 0? -1 : 1;
	}
	for(int ns = 0; ns < (nseeds-1); ++ns){			
		cout<<"!%";	for(int i=0; i<inputSize; ++i) cout<<"\t"<<(input[i]==1?1:0); cout<<endl;
		cpt.checkpoint('f', input);
		addInputsToTestSet(input);		
		for(int i=0; i< inputSize; ++i){
			input[i] = (rand() & 0x1) > 0? -1 : 1;			
		}
	}
	
	bool succeeded = solveCore();
		
	return succeeded;
}

//private
bool FindCheckSolver::solveCore(){
	int ctrlSize = ctrl.size();
	int inputSize = input.size();
	int iterations = 0;
	bool fail = false;
 	bool doMore=true;
	timerclass ftimer("* FIND TIME");
	timerclass ctimer("* CHECK TIME");
	while(doMore){
		{// Find
			cout<<"!%";	for(int i=0; i<inputSize; ++i) cout<<"\t"<<(input[i]==1?1:0); cout<<endl;
			cpt.checkpoint('f', input);
			cout<<"BEG FIND"<<endl; ftimer.restart();
			doMore = find(input, ctrl);
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
			cpt.checkpoint('c', ctrl);
			cout<<"BEG CHECK"<<endl; ctimer.restart();
			doMore = check(ctrl, input);
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



bool FindCheckSolver::solveFromCheckpoint(istream& in){
	timerclass ctimer("* CHECK TIME");
	
	int inputSize = getInSize();
	int ctrlSize = getCtrlSize();
	
	cout<<"inputSize = "<<inputSize<<"\tctrlSize = "<<ctrlSize<<endl;
	
	ctrl.resize(ctrlSize);
	input.resize(inputSize);
	srand(randseed);
	int maxSize = (ctrlSize>inputSize? ctrlSize : inputSize)+2;
	char* buff = new char[maxSize];
	char last = 'n';
	bool unaddedInput = false;
	
	queue<pair<string, int> > resizelist;
	
	while(in){
		in.getline(buff, maxSize);
		if( buff[0] == '\0' ) continue;
		if(buff[0] == 'f'){
			if( unaddedInput ){ 
				while(resizelist.size()>0){
					pair<string, int> p = resizelist.front();
					resizelist.pop();
					declareInput(p.first, p.second);	
				}
				addInputsToTestSet(input); }
			for(int i=0; i<inputSize; ++i){
				Assert( buff[i+1] == '0' || buff[i+1] == '1' , "CORRUPTED FILE f "<<i<<" of "<<inputSize<<"  "<<buff[i+1]);
				input[i] = buff[i+1]=='1'? 1 : -1;
			}
			unaddedInput = true;
			last = 'f';
		}else 
		if(buff[0] == 'c'){
			for(int i=0; i<ctrlSize; ++i){
				Assert( buff[i+1] == '0' || buff[i+1] == '1' , "CORRUPTED FILE c "<<i<<" of "<<ctrlSize<<"  "<<buff[i+1]);
				ctrl[i] = buff[i+1]=='1'? 1 : -1;
			}
			last = 'c';
		}else
		if(buff[0] == 'r'){
			string sbuf(&buff[1]);
			int brk = sbuf.find(' ', 0);
			string name = sbuf.substr(0, brk);
			int size = atoi(sbuf.substr(brk).c_str());
			resizelist.push(make_pair(name, size));			
		}else Assert(false, "CORRUPTED FILE c "<<buff[0]);
	}
	bool succeeded ;
	if( last == 'f' ){
		Assert( unaddedInput , "This is not possible ");
		while(resizelist.size()>0){
			pair<string, int> p = resizelist.front();
					resizelist.pop();
			declareInput(p.first, p.second);	
		}
		succeeded = solveCore();
	}else if(last == 'c'){
		if( unaddedInput ){ 
			while(resizelist.size()>0){
				pair<string, int> p = resizelist.front();
					resizelist.pop();
				declareInput(p.first, p.second);	
			}
			addInputsToTestSet(input); 
		}
		bool doMore;
		{ // Check
			cout<<"!+";	for(int i=0; i<ctrlSize; ++i) cout<<"\t"<<(ctrl[i]==1?1:0);	cout<<endl;
			cout<<"BEG CHECK"<<endl; ctimer.restart();
			doMore = check(ctrl, input);
			ctimer.stop(); cout<<"END CHECK"<<endl;
			ctimer.print();
		}
		
		if(doMore){
			succeeded = solveCore();
		}else{
			cout<<" *GOT THE CORRECT ANSWER IN 0 iterations."<<endl;		
			succeeded = true;
		}
	}else Assert(false, "EMPTY FILE");
	return succeeded;
}



bool FindCheckSolver::find(vector<int>& input, vector<int>& controls){
		
	timerclass tc("* TIME TO ADD INPUT ");
	tc.start();				
	addInputsToTestSet(input);
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
		jj = getCtrlStart(cname);
		for(int i=0; i<cnt; ++i, ++jj){
			Assert( controls.size() > jj , "Out of bounds, BAD!!!");
			int val = mngFind.getVarVal(dirFind.getArr(cname, i));
			if( val == 1) controls[jj]= 1;
			else controls[jj]= -1;
		}
	}
	mngFind.reset();
	return true;
//Return true.
}

bool FindCheckSolver::check(vector<int>& controls, vector<int>& input){
	Dout( cout<<"check()"<<endl );
	timerclass tc("* TIME TO ADD CONTROLS ");
	tc.start();				
	setNewControls(controls);
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
    
    
    int jj=0;
	for(map<string, int>::iterator it = inputVars.begin(); it !=inputVars.end(); ++it){
		const string& cname = it->first;
		int cnt = dirCheck.getArrSize(cname);
		Assert( cnt == it->second, "SIZE MISMATCH: "<<cnt<<" != "<<it->second<<endl);
		jj = getInStart(cname);
		for(int i=0; i<cnt; ++i, ++jj){
			Assert( input.size() > jj , "Out of bounds, BAD!!!");			 
			int val = mngCheck.getVarVal(dirCheck.getArr(cname, i));
			if( val == 1) input[jj]= 1;
			else input[jj]= -1;
			cout<<" input "<<cname<<"  has id "<<jj<<" and value "<<input[jj]<<endl;
		}
	}    
	Dout( dirCheck.print() );
	mngCheck.reset();
	return true;
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



int FindCheckSolver::getInSize(){
	return input.size();
}

int FindCheckSolver::getCtrlSize(){
	return ctrl.size();
}

void FindCheckSolver::buildChecker(){
	mngCheck.clean();
	dirCheck.reset();
	for(map<string, int>::iterator it = controlVars.begin(); it !=controlVars.end(); ++it){
		const string& cname = it->first;
		dirCheck.declareInArr(cname, it->second);
	}
	
	
	for(map<string, int>::iterator it = inputVars.begin(); it !=inputVars.end(); ++it){
		const string& cname = it->first;
		dirCheck.declareInArr(cname, it->second);
	}
	
	dirCheck.declareArr(SOUT, Nout);
	dirCheck.declareArr(OUT, Nout);
	dirCheck.makeArrNoBranch(SOUT);
	dirCheck.makeArrNoBranch(OUT);
	
	defineSketch(mngCheck, dirCheck);
	defineSpec(mngCheck, dirCheck);
	addEqualsClauses(mngCheck, dirCheck);				
}


void FindCheckSolver::buildFinder(){		
	for(map<string, int>::iterator it = inputVars.begin(); it !=inputVars.end(); ++it){
		const string& cname = it->first;
		dirFind.declareInArr(cname, it->second);
	}
	cout<<"** Nout="<<Nout<<endl;
	dirFind.declareArr(SOUT, Nout);
	dirFind.declareArr(OUT, Nout);
	dirFind.makeArrNoBranch(SOUT);
	dirFind.makeArrNoBranch(OUT);
	
		
	defineSketch(mngFind, dirFind);
	defineSpec(mngFind, dirFind);		
	Dout( cout<<"____"<<endl );
	addEqualsClauses(mngFind, dirFind);
}




void FindCheckSolver::addInputsToTestSet(vector<int>& input){
	Dout( cout<<"find()"<<endl );
	buildFinder();
	int jj=0;
	for(map<string, int>::iterator it = inputVars.begin(); it !=inputVars.end(); ++it){
		const string& cname = it->first;
		int cnt = dirFind.getArrSize(cname);
		dirFind.makeArrNoBranch(cname);
		Assert( cnt == it->second, "Size missmatch: "<<cname);
		for(int i=0; i<cnt; ++i, ++jj){
			Assert( jj < input.size(), "OUT OF BOUNDS");
			mngFind.setVarClause(input[jj]*dirFind.getArr(cname, i));
		}
	}

	Dout( cout<<"done adding inputs"<<flush<<endl);
}




void FindCheckSolver::setNewControls(vector<int>& controls){
	if(controls.size()>0){
		Dout(cout<<"Control vars have size"<<controls.size()<<endl);
		mngCheck.deleteClauseGroup(2);
	}
	int jj=0;
	for(map<string, int>::iterator it = controlVars.begin(); it !=controlVars.end(); ++it){
		const string& cname = it->first;
		int cnt = dirCheck.getArrSize(cname);
		for(int i=0; i<cnt; ++i, ++jj){
			Assert( jj < controls.size(), "BAD stuff");
			mngCheck.setVarClause(controls[jj]*dirCheck.getArr(cname, i));
		}
	}
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



void FindCheckSolver::setupFind(){
	Dout( cout<<"setupFind()"<<endl );
	//Declare the control variables.
	for(map<string, int>::iterator it = controlVars.begin(); it !=controlVars.end(); ++it){
		const string& cname = it->first;
		dirFind.declareInArr(cname, it->second);
	}
}







void FindCheckSolver::declareControl(const string& cname, int size){
	Dout(cout<<"DECLARING CONTROL "<<cname<<" "<<size<<endl);
	Assert( controlVars.find(cname) == controlVars.end(), "This control had already been declared!!");
	controlVars[cname] = size;
	controlStarts[cname] = ctrl.size();
	ctrl.resize( ctrl.size() + size);	
}



void FindCheckSolver::declareInput(const string& inname, int size){
	//Inputs can be redeclared to change their sizes, but not controls.
	Dout(cout<<"DECLARING INPUT "<<inname<<" "<<size<<endl);
	cpt.resizeInput(inname, size);
	if( inputVars.find(inname) == inputVars.end() ){
		inputStarts[inname] = input.size();
		inputVars[inname] = size;
		Dout( cout<<" INPUT "<<inname<<" ["<<input.size()<<", "<<input.size()+size<<") "<<endl );
		input.resize( input.size() + size);
	}else{
		int oldsize = inputVars[inname];
		int start = inputStarts[inname];
		
		if( oldsize < size){
			input.resize( input.size() - oldsize + size);	
			Dout( cout<<" increasing from "<<oldsize <<" to "<<size);		
			for(int i=input.size()-1; i >= (start + size); --i){
				input[i] = input[i-(size-oldsize)];					
			}
			for(int i=start + oldsize; i<start+size; ++i){
				input[i] = -1;	
			}
		}else{
			for(int i=start + size; (i-(size-oldsize))<input.size(); ++i){
				input[i] = input[i-(size-oldsize)];
			}
			input.resize( input.size() - oldsize + size);
		}
		for(map<string, int>::iterator it = inputStarts.begin(); it != inputStarts.end(); ++it){
			if( it->second > start ){
				it->second = it->second + (size-oldsize) ;	
			}
		}
		
	}
}


int FindCheckSolver::getInSize(const string& input){
	Assert( inputVars.find(input) != inputVars.end(), "This input has not been declared!!"<< input);
	return inputVars[input];
}
int FindCheckSolver::getCtrlSize(const string& ctrl){
	Assert( controlVars.find(ctrl) != controlVars.end(), "This control has already been declared!!"<< ctrl);
	return controlVars[ctrl];
}
int FindCheckSolver::getInStart(const string& input){
	Assert( inputStarts.find(input) != inputStarts.end(), "This input has not been declared!!"<< input);
	return inputStarts[input];
}
int FindCheckSolver::getCtrlStart(const string& ctrl){
	Assert( controlStarts.find(ctrl) != controlStarts.end(), "This input has not been declared!!" << ctrl);
	return controlStarts[ctrl];
}

	
void FindCheckSolver::setup(){
	setupFind();
	setupCheck();
}


void FindCheckSolver::setCheckpoint(const string& filename){
	cpt.setCheckpoint(filename);
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



