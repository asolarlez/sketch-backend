#include "BLIFwriter.h"

BLIFwriter::BLIFwriter(const string& name_p, SolverMode smode):SATSolver(name_p, smode), output(name_p.c_str()),off(false)
{
	varNum = 1;
	tmpID = 1;
	output<<".model test"<<endl;
	output<<".outputs OUT"<<endl;
	output<<".inputs IN"<<endl;	

}


void BLIFwriter::addCountingHelperClause(int c[], int sz){

}

BLIFwriter::~BLIFwriter(void)
{
}

void BLIFwriter::annotate(const string& msg){
	Dout( cout<<msg );
	FileOutput(output<<msg<<endl);
}

void BLIFwriter::addHelperClause(int c[], int size){
	
}

void BLIFwriter::annotateInput(const string& name, int i, int sz){
	/*
	output<<"x "<<name<<" ";
	for(int t=0; t<sz; ++t){
		output<<(i+t)<<" ";
	}
	output<<endl;
	*/
}



//This function encodes x == a ? b:c;
 void BLIFwriter::addChoiceClause(int x, int a, int b, int c){
	 if(!off){
		output<<".names "<<nm(a)<<" "<<nm(b)<<" "<<nm(c)<<" "<<nm(x)<<endl;
		output<<sgn(a)<<sgn(b)<<"- 1"<<endl;
		output<<nsgn(a)<<"-"<<sgn(c)<<" 1"<<endl;
	 }
}


//This function encodes x == a xor b;
 void BLIFwriter::addXorClause(int x, int a, int b){
	 if(!off){
		output<<".names "<<nm(a)<<" "<<nm(b)<<" "<<nm(x)<<endl;
		output<<sgn(a)<<nsgn(b)<<" 1"<<endl;
		output<<nsgn(a)<<sgn(b)<<" 1"<<endl;
	 }
}

//This function encodes x == a or b;
 /*
 void BLIFwriter::addOrClause(int x, int a, int b){
	 if(!off){
		output<<".names "<<nm(a)<<" "<<nm(b)<<" "<<nm(x)<<endl;
		output<<sgn(a)<<"-"<<" 1"<<endl;
		output<<"-"<<sgn(b)<<" 1"<<endl;
	 }
}*/


//This function encodes a[0] == a[1] or a[2] or ... a[size];
 void BLIFwriter::addBigOrClause(int* a, int size){
	 if(!off){
		output<<".names ";
		for(int i=0; i<size; ++i){
			output<<nm(a[i+1])<<" ";
			
		}
		output<<nm(a[0])<<endl;
		
		for(int j=0; j<size; ++j){
			for(int i=0; i<size; ++i){
				if(j==i){
					output<<sgn(a[i+1]);
				}else{
					output<<"-";
				}
			}
			output<<" 1"<<endl;
		}
	 }
}


//This function encodes x == a and b;
 void BLIFwriter::addAndClause(int x, int a, int b){
	 if(!off){
		output<<".names "<<nm(a)<<" "<<nm(b)<<" "<<nm(x)<<endl;
		output<<sgn(a)<<sgn(b)<<" 1"<<endl;
	 }
}

//This function encodes x = a;
 void BLIFwriter::addEqualsClause(int x, int a){
	 if(!off){
		output<<".names "<<nm(a)<<" "<<nm(x)<<endl;
		output<<sgn(a)<<" 1"<<endl;
	 }
}


//This function encodes x == a;
 void BLIFwriter::addEquateClause(int x, int a){
	if( !solveNegation  ){
		if(!off){
			string t = outvar();
			output<<".names "<<nm(a)<<" "<<nm(x)<<" "<<t<<endl;
			output<<sgn(a)<<nsgn(x)<<" 1"<<endl;
			output<<nsgn(a)<<sgn(x)<<" 1"<<endl;
		}
	}else{
		int tmp = newVar ();
		addXorClause(tmp, x, a);
		finalOr.push_back(tmp);
	}
}


 void BLIFwriter::setVarClause(int x){
	 if(!off){
		output<<".names "<<nm(x)<<endl;
		output<<sgn(x)<<endl;
	 }
}

void BLIFwriter::hardAssertVarClause(int x){
	if(!off){
		string t = outvar();
		output<<".names "<<nm(x)<<" "<<t<<endl;
		output<<sgn(x)<<" 1"<<endl;		
	}
}


 void BLIFwriter::assumeVarClause(int x){
 	Assert(false, "");
}

 void BLIFwriter::assertVarClause(int x){
 	if( !solveNegation  ){
		if(!off){
			string t = outvar();
			output<<".names "<<nm(x)<<" "<<t<<endl;
			output<<sgn(x)<<" 1"<<endl;		
		}
	}else{	
		finalOr.push_back(-x);
	}
}



 void BLIFwriter::printDiagnostics(char c){

 }






int BLIFwriter::getVarVal(int id){
	return 0;
}
 
int BLIFwriter::newVar(){
 	varNum++;
	return varNum;
}
	 
int BLIFwriter::newInVar(){
 	varNum++;
	return varNum;
}
	 	 
void BLIFwriter::disableVarBranch(int i){

}
 
bool BLIFwriter::ignoreOld(){
	return true;
}
	 


 int BLIFwriter::solve(){
 	if(solveNegation){
		if(finalOr.size() > 0){
			vector<int> tmp;
			tmp.resize(finalOr.size() + 1);
			tmp[0] = newVar();
			for(int i=0; i<finalOr.size(); ++i){
				tmp[i+1] = finalOr[i];
			}
			addBigOrClause(&tmp[0], tmp.size()-1);
			output<<".names "<<nm(tmp[0])<<" OUT"<<endl;
			output<<"1 1"<<endl;
		}else{
			output<<".names ";
			for(int i=0; i<tmpID; ++i){
				output<<"tmp"<<i<<" ";
			}
			output<<" OUT"<<endl;
			for(int i=0; i<tmpID; ++i){
				output<<"1";
			}
			output<<" 1"<<endl;
		}
 	}
	output<<".end"<<endl;
	return SATSolver::UNDETERMINED;
}


void BLIFwriter::retractableAssertClause(int x){

}

void BLIFwriter::retractAssumptions(){

}

 void BLIFwriter::reset(){
 		
}
 /*
 void BLIFwriter::cleanupDatabase(){
	
}
*/
 void BLIFwriter::clean(){
 	
}


