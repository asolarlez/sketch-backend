

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


#define Dout( out )  /*    out   */


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


 /**
addExPairConstraints:
pairs is an array of length 2*npairs that contains consecutive pairs [a0,b0, a1,b1,...ak,bk]
The output will be true if any of these pairs are both true. 
There is an assumption that no two ai,aj can be true for i!=j and 
similarly for bi bj. 
Concretely, it encodes the following implications:
ai&bi => out
out&x => y   
out&y => x
(forall i !ai) => !out
(forall i !bi) => !out
*/
 void MiniSATSolver::addExPairConstraint(int* pairs, int npairs, int out){
	int* tb = new int[2*npairs+2];
	vec<Lit> lits;
	tb[0] = -out;
	tb[npairs+1] = -out;
	for(int i=0; i<npairs; ++i){
		int x = pairs[2*i];
		int y = pairs[2*i+1];
		{ int tmp[] = {-x, -y, out}; addClause(tmp, 3, lits); }
		{ int tmp[] = {-out, -x, y}; addClause(tmp, 3, lits); }
		{ int tmp[] = {-out, x, -y}; addClause(tmp, 3, lits); }
		Dout(cout<<"@ ExPair : "<<out<<" <- ("<<x<<", "<<y<<")"<<endl;)
		tb[i+1] = x;
		tb[npairs+i+2] = y;
	}
	addClause(tb, npairs+1, lits);
	addClause(tb+npairs+1, npairs+1, lits);
	delete[] tb;
 }

 void MiniSATSolver::addCountingHelperClause(int c[], int sz){
	Assert(sz>2, "addCountingHelperClause: too small sz=" << sz);
	vec<Lit> lits;
	Dout(cout<<"@ C-helper "; for(int i=0; i<sz; ++i){cout<<c[i]<<", ";}cout<<endl;)
	lits.clear();
	for(int i=0; i<sz; ++i){	
		int var = abs(c[i]);		
		lits.push( (c[i] > 0) ? Lit(var) : ~Lit(var) );		
	}
	s->addClause(lits, SINGLESET);
	++clauseCount;
 }


 void MiniSATSolver::addHelper2Clause(int l1, int l2){
	 Dout(cout<<"@ helper "<<l1<<", "<<l2<<endl;)
	s->addCNFBinary((l1 > 0) ? Lit(l1) : ~Lit(-l1),
					(l2 > 0) ? Lit(l2) : ~Lit(-l2));
 }

void MiniSATSolver::addHelperClause(vec<Lit>& vl){
	s->addClause(vl);
}

void MiniSATSolver::addHelperClause(int c[], int sz){
	vec<Lit> lits;
	Dout(cout<<"@ helper "; for(int i=0; i<sz; ++i){cout<<c[i]<<", ";}cout<<endl;)
	addClause(c, sz, lits);
 }

#define mklit(x) ((x)>0?Lit(x):Lit(-(x),true) )

//This function encodes x == a and b;
 void MiniSATSolver::addAndClause(int x, int a, int b){
	Dout( cout<<"@ "<<x<<"= "<<a<<" and "<<b<<"; "<<endl );
	FileOutput( output<<x<<" AND "<<a<<" "<<b<<endl );
	vec<Lit> lits(3);	
	lits[0] = mklit(x); lits[1] = mklit(-a); lits[2] = mklit(-b); s->addClause(lits); ++clauseCount;
	lits.pop();
	lits[0] = mklit(-x); lits[1] = mklit(a); s->addClause(lits); ++clauseCount;
	lits[0] = mklit(-x); lits[1] = mklit(b); s->addClause(lits); ++clauseCount;

	// { int tmp[] = { -(x), (a)}; addClause(tmp, 2, lits);}
	// { int tmp[] = { -(x), (b)}; addClause(tmp, 2, lits);}
	//{ int tmp[] = { (x), -(a), -(b)}; addClause(tmp, 3, lits);}
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

bool MiniSATSolver::assertIfPossible(int a){
	return s->assertIfPossible(a > 0 ? Lit(a) : ~Lit(-a));

}


bool MiniSATSolver::isOK(){
	return s->okay();
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
 /*
 void MiniSATSolver::addOrClause(int x, int a, int b){
	Dout( cout<<"@ "<<x<<"= "<<a<<" or "<<b<<"; "<<endl );
	FileOutput( output<<x<<" OR "<<a<<" "<<b<<endl );
	vec<Lit> lits;	
	{ int tmp[] = { (x), -(a)}; addClause(tmp, 2, lits);}
	{ int tmp[] = { (x), -(b)}; addClause(tmp, 2, lits);}
	{ int tmp[] = { -(x), (a), (b)}; addClause(tmp, 3, lits);}
}*/


//This function encodes a[0] == a[1] or a[2] or ... a[size];
 void MiniSATSolver::addBigOrClause(int* a, int size){
	Dout( cout<<"@ "<<a[0]<<"= " );
	FileOutput( output<<a[0]<<" BOR "<<size<<" " );
	vec<Lit> lits;
	
	for(int i=0; i<size; ++i){
		Dout(cout<<a[i+1]<<" or "; )
		{ int tmp[] = { (a[0]), -(a[i+1])}; addClause(tmp, 2, lits);}
		FileOutput( output<<a[i+1]<<" " );
	}
	//	
		//
	
	FileOutput( output<<endl );
	Dout(cout<<"; "<<endl);
	/*	
	LAZYOR clauses proved not to be very helpful.
	for(int i=0; i<size+1; ++i){	
		int var = abs(a[i]);		
		lits.push( (a[i] > 0) ? Lit(var) : ~Lit(var) );		
	}
	s->addClause(lits, LAZYOR);
	lits.clear();*/
	a[0] = -a[0];
	addClause(a, size+1, lits);
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
		if(finalOr.size() == 0){
			Dout( cout<<"@ assert "<<x<<";"<<endl );
			FileOutput( output<<"x OUTASSERT "<<x<<" ;"<<endl );
			vec<Lit> lits;
			{ int tmp[] = { x }; addClause(tmp, 1, lits);}		
		}else{
			vec<Lit> lits;		
			int sz = finalOr.size();
			for(int i=0; i<sz; ++i){	
				int var = abs(finalOr[i]);		
				lits.push( (finalOr[i] > 0) ? Lit(var) : ~Lit(var) );		
			}
			lits.push( (x > 0) ? Lit(x) : ~Lit(-x) );	
			s->addClause(lits);
		}
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

void MiniSATSolver::assumeVarClause(int x){
	if(solveNegation){
		// I am in the verification phase		
		vec<Lit> lits;
		if(finalOr.size() == 0){
			//If no asserts yet, we need to ensure counterexamples satisfy the assume
			{ int tmp[] = { x }; addClause(tmp, 1, lits);}	
		}else{
			//If there are some asserts already, those can fail evne if the assume fails.
			int sz = finalOr.size();
			for(int i=0; i<sz; ++i){	
				int var = abs(finalOr[i]);		
				lits.push( (finalOr[i] > 0) ? Lit(var) : ~Lit(var) );		
			}
			lits.push( (x > 0) ? Lit(x) : ~Lit(-x) );	
			s->addClause(lits);
		}		
	}else{
		finalOr.push_back(-x);
	}
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
	lbool result = s->solve(assumptions);
 	if( ! s->okay() ){ /*cout<<" NOT OKAY2 "<<endl; */}		

	if( result == l_True) {
		//cout<<" Returned SAT"<<endl;
		return SATISFIABLE;	
	}
	if(result==l_False){
		//cout<<"Returned UNSAT"<<endl;
		return UNSATISFIABLE;
	}	
	return UNDETERMINED;
}

 void MiniSATSolver::reset(){
 	finalOr.clear();	
	s->cancelUntil(0);
	//cout<<"clause count = "<<clauseCount<<endl;
	clauseCount=0;
}



 void MiniSATSolver::clean(){
 	finalOr.clear();
 	Dout( cout<<" CLEANING UP "<<endl );
 	delete s;
 	s = new Solver();
	if(isNegated()){ s->polarity_mode = Solver::polarity_false; }
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

