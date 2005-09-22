#include "FindCheckSolver.h"



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
		addOrClause(mng, dir.newAnonymousVar() , finIdx + i-1, startIdx+1);
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







bool FindCheckSolver::find(int input[], int insize, int controls[]){	
	Dout( cout<<"find()"<<endl );
	defineSketch(mngFind, dirFind);
	defineSpec(mngFind, dirFind);	
	Dout( cout<<"____"<<endl );
	addEqualsClauses(mngFind, dirFind);
	
	int N = dirFind.getArrSize(IN);
//Set the values of the inputs from input[];
	Dout( cout<<"____"<<endl );
	for(int i=0; i<N; ++i){
		setVarClause(mngFind, input[i%insize]*dirFind.getArr(IN, i));
	}
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
	

	
	{ int tmp[] = {-1, 1}; 	find(tmp, 2, ctrl); }
	int inputSize = getInSize();
	int * input = new int[inputSize];
	cout<<"inputSize = "<<inputSize<<endl;
	
	int iterations = 0;
	bool isDone;
	do{
		for(int i=0; i<ctrlSize; ++i) cout<<"		ctrl["<<i<<"]="<<ctrl[i]<<endl; cout<<"-----------------------------"<<endl;
		isDone = check(ctrl, ctrlSize, input);
		if(isDone){
			Dout( for(int i=0; i<inputSize; ++i) cout<<"		input["<<i<<"]="<<input[i]<<endl; cout<<"-----------------------------"<<endl );
			bool keepGoing = find(input, inputSize, ctrl);
			if( !keepGoing){
				cout<<"******** FAILED ********"<<endl;
				break;
			}
		}
		cout<<"********  "<<iterations<<endl;
		++iterations;
	}while(isDone);
	
	delete [] ctrl;
	delete [] input;
	cout<<" GOT THE CORRECT ANSWER IN "<<iterations<<" iterations."<<endl;
}


