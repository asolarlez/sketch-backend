#ifndef MINISATSOLVER_H
#define MINISATSOLVER_H

#include "SATSolver.h"
#include <vector>
#include <iostream>
using namespace std;

#include "MSolver.h"
#include "SolverTypes.h"
#include "Vec.h"
#include "Tvalue.h"

using namespace MSsolverNS;

inline void MiniSolverStart(){ cout<<" STARTING SAT "<<endl; }
inline void MiniSolverEnd(){cout<<" ENDING SAT"<<endl;  }

namespace MSsolverNS{

extern uint32_t INTSPECIAL;
}

UfunSummary* newUfun(vec<Lit>& equivs, vector<Tvalue>& out, int outsize, SolverHelper& dir);


class MiniSATSolver : public SATSolver{
protected:
	int solveCount;
	bool outputProblems;
	vector<int> finalOr;
	vec<Lit> assumptions;
	Solver* s;
	void addClause(int tmp[], int sz, MSsolverNS::vec<Lit>& lits);
	int clauseCount;
	bool lsolve;
public:
	ostream* debugout;
	 MiniSATSolver(const string& name_p,  SolverMode smode):SATSolver(name_p, smode){
	 	s = new Solver();
		if(isNegated()){ s->polarity_mode = Solver::polarity_false; }	
		FileOutput( string nm = name; nm += ".circuit"; );
		FileOutput( output.open(nm.c_str()) );	
		s->newVar();
		clauseCount=0;
		debugout = NULL;
		lsolve = false;
		solveCount = 0;
		outputProblems = false;

	 }

	 SynthInSolver* addSynth(int inputs, int outputs, Synthesizer* syn) {
		 return s->addSynth(inputs, outputs, syn);
	 }

	 void addSynSolvClause(SynthInSolver* syn, int instid, int inputid, int value, Lit var) {
		 s->addSynSolvClause(syn, instid, inputid, value, var);
	 }

	 void addUfun(int funid, UfunSummary* ufs){
		 s->addUfun(funid, ufs);
	 }

	 virtual void outputSAT(){
		 cout<<"Outputing problems"<<endl;
		 outputProblems = true;
	 }
	 virtual void addHelperClause(int c[], int sz);


	 virtual bool isIntVarKnown(int id, int& out){
		 Val v = s->intsolve->getVal(id);
		 bool rv = v.isDef();
		 if(v.isDef()){
			 out = v.v();
		 }
		 return rv;
	 }

	 virtual int isValKnown(int i){
		 int var = abs(i);
		 int rv = 1;
		 if(i<0){
			 rv = -1;
		 }
		 if(s->value(var) != MSsolverNS::l_Undef){
			 return (s->value(var)==MSsolverNS::l_True) ? rv : -rv;
		 }
		 return 0; 
	 }
	 virtual bool isOK();
	 virtual bool assertIfPossible(int a);
	 virtual ~MiniSATSolver();
	 virtual void annotate(const string& msg);
	 virtual void annotateInput(const string& name, int i, int sz);
	 virtual void addChoiceClause(int x, int a, int b, int c);
	 virtual void addXorClause(int x, int a, int b);
	 // virtual void addOrClause(int x, int a, int b);
	 virtual void addBigOrClause(int* a, int size);
	 virtual void addAndClause(int x, int a, int b);
	 virtual void addEqualsClause(int x, int a);
	 virtual void addEquateClause(int x, int a);
	 virtual void setVarClause(int x);
     virtual void assertVarClause(int x);
	 virtual void assumeVarClause(int x);


	 virtual int nextIntVar(){
		 return s->intsolve->nvars();
	 }

	 virtual iVar plus(iVar x, iVar y){
		 iVar rv = s->intsolve->addVar();
		 s->intsolve->addPlus(x, y, rv);
		 return rv;
	 }
	 virtual iVar minus(iVar x, iVar y){
		 iVar rv = s->intsolve->addVar();
		 s->intsolve->addMinus(x, y, rv);
		 return rv;
	 }
	  virtual iVar times(iVar x, iVar y){
		 iVar rv = s->intsolve->addVar();
		 s->intsolve->addTimes(x, y, rv);
		 return rv;
	 }

	 virtual iVar mod(iVar x, iVar y){
		 iVar rv = s->intsolve->addVar();
		 s->intsolve->addMod(x, y, rv);
		 return rv;
	 }

	 virtual iVar div(iVar x, iVar y){
		 iVar rv = s->intsolve->addVar();
		 s->intsolve->addDiv(x, y, rv);
		 return rv;
	 }

	 virtual void inteq(iVar x, iVar y, iVar rv){		
		 s->intsolve->addEq(x, y, rv);		 
	 }
	 virtual void intlt(iVar x, iVar y, iVar rv){		 
		 s->intsolve->addLt(x, y, rv);		 
	 }
	 virtual iVar intmux(iVar cond, int len, iVar* choices){
		 iVar rv = s->intsolve->addVar();
		 s->intsolve->addBMux(cond, len, choices, rv);
		 return rv;
	 }

	 virtual const Range& getRange(iVar id) {
		 return s->intsolve->getRange(id);
	 }

	 virtual void addHelper2Clause(int l1, int l2);
	 virtual void addHelperClause(vec<Lit>& vl);

	 virtual bool tryAssignment(int a){
		 return s->tryAssignment(lfromInt(a));
	 }

	 virtual bool checkIfPossible(int a, int& outlv) {
		 Lit la = lfromInt(a);		 
		 return s->checkIfPossible(la, outlv);
	 }

	 virtual void popCheckIfPossible(int lv) {
		 s->popCheckIfPossible(lv);
	 }

	 virtual void retractableAssertClause(int x);
	 void addCountingHelperClause(int c[], int sz);

	 virtual void addExPairConstraint(int* pairs, int npairs, int out);

	 virtual void hardAssertVarClause(int x);
	 
	 virtual void markInput(int id);

	 virtual int getVarVal(int id);
	 
	 virtual int newVar();
	 
	 virtual int newInVar();
	 virtual void disableVarBranch(int i);
	 
	 virtual bool ignoreOld();
	 
	 virtual SATSolver::SATSolverResult solve();
	
	 virtual void reset();
	 virtual void retractAssumptions();
	
	 virtual void finish(){
		if(solveNegation){
			MSsolverNS::vec<Lit> lits;
			addClause(finalOr.size() > 0 ? (&finalOr[0]) : NULL  , finalOr.size(), lits);
		}
	 }
	 
	 virtual void printDiagnostics(char c);
	 virtual void lightSolve();
	 virtual void writeDIMACS(ofstream& dimacs_file){
		 s->writeDIMACS(dimacs_file);
	 }

	 void dump(){
		 s->dump();
	 }

	 /*!
	 Fills single and dble with all the unary and binary clauses that 
	 are in the SAT solver but are not part of baseline.
	 */
	 void getShareable(set<int>& single, set<pair<int, int> >& dble, set<pair<int, int> >& baseline){
		 s->getShareable(single, dble, baseline);
	 }

	 virtual void intSpecialClause(vec<Lit>& ps){
		 s->addClause(ps, INTSPECIAL);
	 }


	 virtual bool iVarHasBitMapping(iVar id, int& out){		 
		 Lit lout;
		 s->intsolve->checkLegal(id, 1, lout);
		 if(lout != lit_Undef){
			 out = sign(lout) ? -var(lout) : var(lout);
			return true;
		 }

		 return false;
	 }

	 virtual int addIntVar(){
		 return s->intsolve->addVar();
	 }
	 virtual int addIntVar(Tvalue& tv){
		 int id = s->intsolve->addVar(tv);

		 const gvvec gv = tv.num_ranges;
		 for(int i=0; i<gv.size(); ++i){
			 const guardedVal& cur = gv[i];
			 int vv = this->isValKnown(cur.guard);
			 if(vv==1){
				 s->intsolve->setVal(id, cur.value, 0); 
			 }
		 }

		 return id;
	 }

	 virtual void addMapping(int id, Tvalue& tv){
		 s->intsolve->addMapping(id, tv);
	 }

	 virtual void setIntVal(int vr, int val){
		 s->intsolve->setVal(vr, val, 0);
		 Intclause* confl = s->intsolve->propagate();
		 Assert(confl == NULL, "CAN NOT RESOLVE!");
	 }
};



#endif
