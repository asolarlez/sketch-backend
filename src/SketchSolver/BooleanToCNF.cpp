#include <fstream>
#include <iostream>
#include <vector>
#include <queue>
#include <set>
#include <map>
#include <unordered_map>

#include <utility>
#include "Sort.h"
#include <math.h>
using namespace std;

#include "BooleanToCNF.h" 
#include "BooleanDAG.h"
#include "MiniSATSolver.h"
#include "DagOptim.h"
#include "CommandLineArgs.h"
#include "VarStore.h"
#include "NodeEvaluator.h"
#include "NumericalSolver.h"
#include "GTPredicateSolver.h"
#include "EntityResolutionSolver.h"
#include "ArithmeticExpressionSolver.h"
#include "SwapperPredicateSolver.h"
//map < string , map < int, set < ArithExpression* > > >  ArithExprBuilder::ASetMap;
map < string, ArithExpression *> ArithExprBuilder::ASigMap;
unordered_map < string, SwapperPredicate *> PredicateBuilder::PSigMap;
int SwapperPredicate::numvars;
#ifndef SAT_Manager
#define SAT_Manager void *
#endif



void SolverHelper::writeDIMACS(ofstream& dimacs_file){	
	for(map<string, int>::iterator fit = varmap.begin(); fit != varmap.end(); ++fit){
		dimacs_file<<"c hole "<<fit->first<<" "<<(fit->second+1);
		if(arrsize.count(fit->first)>0){
			dimacs_file<<" - "<<(fit->second + 1 + arrsize[fit->first]-1);
		}
		dimacs_file<<endl;
	}
	dimacs_file<<"c YES="<<YES+1<<endl;	
	mng.writeDIMACS(dimacs_file);

}

int
SolverHelper::assertVectorsDiffer (int v1, int v2, int size)
{
    int N = size;
    int lastone = 0;
    for(int i=0; i<N; ++i){		
	int cur = addXorClause(v1+i, v2+i);
	if(lastone != 0){
	    lastone = addOrClause(lastone, cur);
	}else{
	    lastone = cur;
	}		
    }
    return lastone;

}

namespace MSsolverNS {
	int ID;
}


Synthesizer* SolverHelper::newSynthesizer(const string& name, FloatManager& _fm) {
	string arithExpr = "_GEN_arithexpr";
	string swapperExpr = "_GEN_swapperpred";
	if (name == "_GEN_gtp") {
		return new GtpSyn(_fm);
	} else if (name == "_GEN_eratom") {
    return new ERAtomSyn(_fm);
  } else if (name.find("_GEN_NUM_SYNTH") == 0) {
    return new NumericalSolver(_fm, numericalAbsMap[name]);
  }else if (mismatch(arithExpr.begin(), arithExpr.end(), name.begin()).first == arithExpr.end()){
	  ArithExprSyn* ret =  new ArithExprSyn(_fm);
	  ret->setupParams(name);
	  return ret;
  }
  else if (mismatch(swapperExpr.begin(), swapperExpr.end(), name.begin()).first == swapperExpr.end()) {
	  SwapperPredicateSyn* ret = new SwapperPredicateSyn(_fm);
	  ret->setupParams(name);
	  return ret;
  }
  else{

	  Assert(false,"Invalid synthesizer name: " + name);
  }
	
	
	return NULL;
}


void SolverHelper::addSynthSolver(const string& name, const string& syntype, vector<Tvalue>& inputs, vector<Tvalue>& outputs, FloatManager& _fm) {
	auto sit = sins.find(name);
	SynthInSolver* sin;
	if (sit == sins.end()) {
		sin = ((MiniSATSolver&)mng).addSynth(inputs.size(), outputs.size(), newSynthesizer(syntype, _fm));
		sins[name] = sin;
	}
	else {
		sin = sit->second;
	}
	int instid = sin->newInstance(inputs, outputs);

	int inputid = 0;
	for (auto it = inputs.begin(); it != inputs.end(); ++it, ++inputid) {
		Tvalue& tv = *it;
		if (tv.isBvect()) {
			((MiniSATSolver&)mng).addSynSolvClause(sin, instid, inputid, 1, lfromInt(tv.getId()));
			((MiniSATSolver&)mng).addSynSolvClause(sin, instid, inputid, 0, lfromInt(-tv.getId()));
			continue;
		}
		const gvvec& vec = tv.num_ranges;
		for (gvvec::const_iterator ci = vec.begin(); ci != vec.end(); ++ci) {
			((MiniSATSolver&)mng).addSynSolvClause(sin, instid, inputid, ci->value, lfromInt(ci->guard));
		}
	}
	int outid = 0;
	for (auto it = outputs.begin(); it != outputs.end(); ++it, ++outid) {
		Tvalue& tv = *it;
		if (tv.isBvect()) {
			((MiniSATSolver&)mng).addSynSolvClause(sin, instid, inputid + outid, 1, lfromInt(tv.getId()));
			((MiniSATSolver&)mng).addSynSolvClause(sin, instid, inputid + outid, 0, lfromInt(-tv.getId()));
			continue;
		}
		const gvvec& vec = tv.num_ranges;
		for (gvvec::const_iterator ci = vec.begin(); ci != vec.end(); ++ci) {
			((MiniSATSolver&)mng).addSynSolvClause(sin, instid, inputid + outid, ci->value, lfromInt(ci->guard));
		}
	}
}

void SolverHelper::addHelperC(Tvalue& tv){
	if(tv.isSparse() ){
		gvvec& gv = tv.num_ranges;
		int size = gv.size();
		if(size == 1){ return; }
		if(size == 2){
			addHelperC(-gv[0].guard, -gv[1].guard);
			return;
		}
		int* x = new int[size];
		for(int i=0; i<size; ++i){
			x[i] = -gv[i].guard;
		}		
		MSsolverNS::sort(x, size);
		int l = this->setStrBO(x, size, ':', 0);		
		int rv;
		if(!this->memoizer.condAdd(&tmpbuf[0], l, 0, rv)){
			mng.addCountingHelperClause(x, gv.size());
		}
		delete[] x;

		/*
		gvvec& gv = tv.num_ranges;
		if(gv.size()<7){
			for(int i=0; i<gv.size()-1; ++i){
				for(int j=i+1; j < gv.size(); ++j){
					addHelperC(-gv[i].guard, -gv[j].guard);
				}
			}
		}else{
			addHelperC(-gv[0].guard, -gv[1].guard);
			int t = addOrClause(gv[0].guard, gv[1].guard);
			for(int i=2; i<gv.size()-1; ++i){
				addHelperC(-t, -gv[i].guard);
				t = addOrClause(t, gv[i].guard);
			}
			addHelperC(-t, -gv[gv.size()-1].guard);
		}
		*/
		
	}
	
}

Tvalue& SolverHelper::getControl(CTRL_node* ctrlnode){	
	Assert(!ctrlnode->get_Angelic(), "not allowed");
	string name = ctrlnode->get_name();
	map<string, Tvalue>::iterator mp = controls.find(name);	
	Assert(mp != controls.end(), "Not here");
	return mp->second;
}

Tvalue& SolverHelper::declareControl(CTRL_node* ctrlnode){
	Assert(!ctrlnode->get_Angelic(), "not allowed");
	string name = ctrlnode->get_name();
	map<string, Tvalue>::iterator mp = controls.find(name);
	if(mp != controls.end()){
		return mp->second;
	}else{
		int nbits = ctrlnode->get_nbits();
		declareInArr(name, nbits);
		Tvalue& rv = controls[name];
		rv = getArr(name, 0);
		if(nbits > 1){
			rv.setSize(nbits);
			rv.makeSparse(*this);
		}
		return rv;
	}
}




void SolverHelper::addHelperC(int l1, int l2){
	if(l1 == -l2)
		return;
	mng.addHelper2Clause(l1, l2);
}

/*
int
SolverHelper::select (int choices[], int control, int nchoices, int bitsPerChoice)
{
    int outvar = getVarCnt();	
    for(int i=0; i<bitsPerChoice; ++i){
	newAnonymousVar();
    }	
    for(int j=0; j<bitsPerChoice; ++j){
	mng.setVarClause( -(newAnonymousVar()));
	for(int i=0; i<nchoices; ++i){
	    int cvar = newAnonymousVar();
	    mng.addAndClause( cvar, control+i, choices[i]+j);
	    int cvar2 = newAnonymousVar();
	    mng.addOrClause( cvar2, cvar, cvar-1);
	}
	mng.addEqualsClause( outvar+j, getVarCnt()-1);
    }
    return outvar;
}

int
SolverHelper::selectMinGood (int choices[], int control, int nchoices, int bitsPerChoice)
{
    int outvar = select(choices, control, nchoices, bitsPerChoice);	
    int differences = getVarCnt();	
    int prev = newAnonymousVar();
    mng.setVarClause(-prev);
    for(int i=1; i<nchoices; ++i){
	int different = assertVectorsDiffer(choices[i-1], choices[i], bitsPerChoice);
	int cvar = newAnonymousVar();
	mng.addAndClause( cvar, control+i, different);
	int cvar2 = newAnonymousVar();
	mng.addOrClause( cvar2, cvar, prev);
	prev = cvar2;
    }
    mng.setVarClause( -prev);	
    return outvar;
}

int
SolverHelper::arbitraryPerm (int input, int insize, int controls[], int ncontrols, int csize)
{
    // ncontrols = sizeof(controls);
    Assert( insize <= csize, "This is an error");
    int OUTSIZE = ncontrols;
    int outvar = getVarCnt(); for(int i=0; i<OUTSIZE; ++i){ newAnonymousVar(); };
    for(int i=0; i<OUTSIZE; ++i){
	int cvarPrev = newAnonymousVar();
	mng.setVarClause( -cvarPrev);
	for(int j=0; j<insize; ++j){
	    int cvar = newAnonymousVar();
	    mng.addAndClause( cvar, controls[i]+j, input+j);
	    int cvar2 = newAnonymousVar();
	    mng.addOrClause( cvar2, cvar, cvarPrev);
	    cvarPrev = cvar2;
	}
	mng.addEqualsClause( outvar+i, cvarPrev);
    }
    return outvar;
}

*/
