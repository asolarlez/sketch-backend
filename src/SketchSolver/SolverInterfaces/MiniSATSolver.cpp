

#include <iostream>
#include <fstream>
#include <cstdlib>
#include <cstdio>
#include <map>
#include <string>
#include <set>
#include <vector>
#include <queue>
#include "MiniSATSolver.h"


// #define Dout( out )      out 


void MiniSATSolver::markInput(int id){
	if(!solveNegation){
		s->regInput(abs(id));
	}
}

void MiniSATSolver::annotate(const string& msg){
	Dout( cout<<msg );
	FileOutput(output<<msg<<endl);
}

 void MiniSATSolver::annotateInput(const string& name, int i, int sz){
	Dout( cout<<"x "<<name<<" ");
	FileOutput(output<<"x "<<name<<" ");
	for(int t=0; t<sz; ++t){
		Dout( cout<<(i+t)<<" ");
		FileOutput(output<<(i+t)<<" ");
	}
	Dout(cout<<endl);
	FileOutput(output<<endl);
}

void MiniSATSolver::addHelperClause(int c[], int sz){
	vec<Lit> lits;
	Dout(cout<<"@ helper "; for(int i=0; i<sz; ++i){cout<<c[i]<<", ";}cout<<endl;)
	addClause(c, sz, lits);
 }




void MiniSATSolver::addClause(int tmp[], int sz, vec<Lit>& lits){
	lits.clear();
	for(int i=0; i<sz; ++i){	
		int var = abs(tmp[i]);		
		lits.push( (tmp[i] > 0) ? Lit(var) : ~Lit(var) );		
	}
	if(debugout!=NULL){
		for(int i=0; i<sz; ++i){	
			if(debugout!=NULL){ (*debugout)<<tmp[i]<<" "; }
		}		
		(*debugout)<<"0"<<endl; 
	}
	s->addClause(lits);
	++clauseCount;
} 


//This function encodes x == a ? b:c;
 void MiniSATSolver::addChoiceClause(int x, int a, int b, int c){
	Dout( cout<<"@ "<<x<<"= "<<a<<" ? "<<b<<":"<<c<<";"<<endl );
	FileOutput( output<<x<<" CHOICE "<<a<<" "<<b<<" "<<c<<endl );
	vec<Lit> lits;
	{ int tmp[] = { -(x), -(a), (b) }; addClause(tmp, 3, lits);}
	{ int tmp[] = { -(x), (c), (a) }; addClause(tmp, 3, lits);}
	{ int tmp[] = { -(x), (c), (b) }; addClause(tmp, 3, lits);}
	{ int tmp[] = { (x), -(c), -(b) }; addClause(tmp, 3, lits);}
	{ int tmp[] = { (x), (a), -(c) }; addClause(tmp, 3, lits);}
	{ int tmp[] = { (x), -(a), -(b) }; addClause(tmp, 3, lits);}
}


//This function encodes x == a xor b;
 void MiniSATSolver::addXorClause(int x, int a, int b){
	Dout( cout<<"@ "<<x<<"= "<<a<<" xor "<<b<<"; "<<endl );
	FileOutput( output<<x<<" XOR "<<a<<" "<<b<<endl );
	vec<Lit> lits;	
	{ int tmp[] = { -(x), -(a), -(b) }; addClause(tmp, 3, lits);}
	{ int tmp[] = { -(x), (a), (b) }; addClause(tmp, 3, lits);}
	{ int tmp[] = { (x), -(a), (b) }; addClause(tmp, 3, lits);}
	{ int tmp[] = { (x), (a), -(b) }; addClause(tmp, 3, lits);}
}

//This function encodes x == a or b;
 void MiniSATSolver::addOrClause(int x, int a, int b){
	Dout( cout<<"@ "<<x<<"= "<<a<<" or "<<b<<"; "<<endl );
	FileOutput( output<<x<<" OR "<<a<<" "<<b<<endl );
	vec<Lit> lits;	
	{ int tmp[] = { (x), -(a)}; addClause(tmp, 2, lits);}
	{ int tmp[] = { (x), -(b)}; addClause(tmp, 2, lits);}
	{ int tmp[] = { -(x), (a), (b)}; addClause(tmp, 3, lits);}
}


//This function encodes a[0] == a[1] or a[2] or ... a[size];
 void MiniSATSolver::addBigOrClause(int* a, int size){
	Dout( cout<<"@ "<<a[0]<<"= " );
	FileOutput( output<<a[0]<<" BOR "<<size<<" " );
	vec<Lit> lits;
	for(int i=0; i<size; ++i){
		Dout(cout<<a[i+1]<<" or ");
		{ int tmp[] = { (a[0]), -(a[i+1])}; addClause(tmp, 2, lits);}
		FileOutput( output<<a[i+1]<<" " );
	}
	FileOutput( output<<endl );
	Dout(cout<<"; "<<endl);
	a[0] = -a[0];
	addClause(a, size+1, lits);
}


//This function encodes x == a and b;
 void MiniSATSolver::addAndClause(int x, int a, int b){
	Dout( cout<<"@ "<<x<<"= "<<a<<" and "<<b<<"; "<<endl );
	FileOutput( output<<x<<" AND "<<a<<" "<<b<<endl );
	vec<Lit> lits;	
	{ int tmp[] = { -(x), (a)}; addClause(tmp, 2, lits);}
	{ int tmp[] = { -(x), (b)}; addClause(tmp, 2, lits);}
	{ int tmp[] = { (x), -(a), -(b)}; addClause(tmp, 3, lits);}
}

//This function encodes x = a;
 void MiniSATSolver::addEqualsClause(int x, int a){
	Dout( cout<<"@ "<<x<<"= "<<a<<"; "<<flush<<endl );
	FileOutput( output<<x<<" EQ "<<a<<endl );
	vec<Lit> lits;	
	{ int tmp[] = { -(x), (a)}; addClause(tmp, 2, lits);}
	{ int tmp[] = { (x), -(a)}; addClause(tmp, 2, lits);}
}


//This function encodes x == a;
 void MiniSATSolver::addEquateClause(int x, int a){
	if( !solveNegation  ){
		Dout( cout<<"@ "<<x<<"= "<<a<<"; "<<flush<<endl );
		FileOutput( output<<"x OUTXOR "<<x<<" "<<-a<<endl );
		vec<Lit> lits;
		{ int tmp[] = { -(x), (a)}; addClause(tmp, 2, lits);}
		{ int tmp[] = { (x), -(a)}; addClause(tmp, 2, lits);}
	}else{
		int tmp = newVar ();
		addXorClause(tmp, x, a);
		finalOr.push_back(tmp);
	}
}


 void MiniSATSolver::setVarClause(int x){
	Dout( cout<<"@ set "<<x<<";"<<endl );
	FileOutput( output<<"x SET "<<x<<" ;"<<endl );
	//cout<<x<<endl;
	vec<Lit> lits;
	{ int tmp[] = { x }; addClause(tmp, 1, lits);}
}


 void MiniSATSolver::assertVarClause(int x){
 	if( !solveNegation  ){
		Dout( cout<<"@ assert "<<x<<";"<<endl );
		FileOutput( output<<"x OUTASSERT "<<x<<" ;"<<endl );
		vec<Lit> lits;
		{ int tmp[] = { x }; addClause(tmp, 1, lits);}		
	}else{	
		finalOr.push_back(-x);
	}
}

 void MiniSATSolver::retractableAssertClause(int x){
	 Assert(!solveNegation, "You can only add retractable clauses in Synthesis mode");
	 int var = abs(x);		
	 assumptions.push( (x > 0) ? Lit(var) : ~Lit(var) );	 
 }

void MiniSATSolver::retractAssumptions(){
	assumptions.clear();
}


void MiniSATSolver::hardAssertVarClause(int x){
	Dout( cout<<"@ assert "<<x<<";"<<endl );
	FileOutput( output<<"x OUTASSERT "<<x<<" ;"<<endl );	
	vec<Lit> lits;
	{ int tmp[] = { x }; addClause(tmp, 1, lits);}	
}



 void MiniSATSolver::printDiagnostics(char c){
/*	 if(c=='f'){
		s->printSmallLearnts();
	 }*/
	 cout << c <<"# %assign: "<<((100*s->nAssigns())/s->nVars());
 	cout <<" clauses: "<<s->nClauses();
   	cout <<" learn: "<<s->nLearnts(); 	
	cout <<" restart: "<<s->starts;
	cout <<" decision: "<<s->decisions; 
	cout <<" propagated: "<<s->propagations<<endl;		
	/*
 	cout << c <<"# trl                : "<<s->nAssigns()<<endl;
 	cout << c <<"# clauses                : "<<s->nClauses()<<endl;
   	cout << c <<"# learnts                : "<<s->nLearnts()<<endl; 	
	cout << c << "# restarts              : "<<s->starts<<endl;
	cout << c << "# decisions             : "<<s->decisions<<endl; 
	cout << c << "# propagations          : "<<s->propagations<<endl;	
	*/
 }






int MiniSATSolver::getVarVal(int id){
	return s->model[id].toInt();
}
 
int MiniSATSolver::newVar(){
 	s->newVar();
	return s->nVars()-1;
}
	 
int MiniSATSolver::newInVar(){
 	s->newVar();
	return s->nVars()-1;
}
	 	 
void MiniSATSolver::disableVarBranch(int i){

}
 
bool MiniSATSolver::ignoreOld(){
	return false;	
}
	 


 int MiniSATSolver::solve(){
 	if(solveNegation){
 		vec<Lit> lits;
		Dout(cout<<"@asserting "; for(int i=0; i<finalOr.size(); ++i){ cout<<finalOr[i]<<", ";} cout<<endl; )
		addClause(finalOr.size() > 0 ? (&finalOr[0]) : NULL  , finalOr.size(), lits);
 	} 
 	if( ! s->okay() ){ /* cout<<"FOUND UNSAT BEFORE SIMPLIFYING"<<endl; */ }
 	s->simplify();
 	if( ! s->okay() ){ /* cout<<"FOUND UNSAT BEFORE SIMPLIFYING"<<endl; */ return UNSATISFIABLE; }		
	bool result = s->solve(assumptions);
 	if( ! s->okay() ){ /*cout<<" NOT OKAY2 "<<endl; */}	
	if( result) {
		//cout<<" Returned SAT"<<endl;
		return SATISFIABLE;	
	}else{
		//cout<<"Returned UNSAT"<<endl;
		return UNSATISFIABLE;
	}
}

 void MiniSATSolver::reset(){
 	finalOr.clear();	
	//cout<<"clause count = "<<clauseCount<<endl;
	clauseCount=0;
}



 void MiniSATSolver::clean(){
 	finalOr.clear();
 	Dout( cout<<" CLEANING UP "<<endl );
 	delete s;
 	s = new Solver();
 	s->newVar();
	Dout(cout<<"clause count = "<<clauseCount<<endl;)
	clauseCount=0;
	if(lsolve){
		s->makeIncomplete();
	}
}

void MiniSATSolver::lightSolve(){
	s->makeIncomplete();
	lsolve = true;
}


 MiniSATSolver::~MiniSATSolver(){
	delete s;
 }

