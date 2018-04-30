#include <fstream>
#include <iostream>
#include <vector>
#include <queue>
#include <set>
#include <map>
#include <utility>
#include <Sort.h>
#include <math.h>
using namespace std;

#include "BooleanToCNF.h"
#include "BooleanDAG.h"
#include "MiniSATSolver.h"
#include "DagOptim.h"
#include "CommandLineArgs.h"
#include "VarStore.h"
#include "NodeEvaluator.h"
#include "NumericalSynthesizer.h"
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
class GtpSyn : public Synthesizer {
	int theta;
public:
	GtpSyn(FloatManager& _fm) :Synthesizer(_fm) {

	}
	
	virtual void backtrack(int level) {}
	
	virtual bool synthesis(vec<Lit>& suggestions) {
		conflict.clear();
		int gtmin = 1000000;
		int gtid = -1;
		int ltmax = -10000000;
		int ltid = -1;
		InputMatrix& im = *inout;
		int inpt = 0;
		int outpt = 1;
		for (int i = 0; i < inout->getNumInstances(); ++i) {
			int out = im.getVal(i, outpt);
			int in = im.getVal(i, inpt);
			if (out == EMPTY || in == EMPTY) {
				continue;
			}
			if (out == 1) {
				if (in < gtmin) { gtmin = in; gtid = i; }
			}
			else {
				if (in > ltmax) { ltmax = in; ltid = i; }
			}
		}		
		//im.print();
		//cout << ltmax << "-" << gtmin << endl;
		if (ltmax < gtmin) {
			theta = (ltmax + gtmin) / 2;
			return true;
		}
		else {
			if (gtid == -1) {
				theta = ltmax + 1;
				return true;
			}
			if (ltid == -1) {
				theta = gtmin - 1;
				return true;
			}
			conflict.push(im.valueid(gtid, inpt));
			conflict.push(im.valueid(gtid, outpt));
			conflict.push(im.valueid(ltid, inpt));
			conflict.push(im.valueid(ltid, outpt));
			return false;
		}
	}
	virtual void newInstance() {

	}

	virtual void finalize() {

	}

	virtual bool_node* getExpression(DagOptim* dopt, const vector<bool_node*>& params) {
		return dopt->addGT(params[0], dopt->getCnode(theta));
	}

	virtual void print(ostream& out) {
		out << "( " << theta << "< IN_0" << ")";
	}
	
  
  virtual void getControls(map<string, string>& values) {
    stringstream str;
    print(str);
    values["_GEN_gtp"] = str.str();
  }
};

class ERAtomSyn : public Synthesizer {
	int theta;
	int simfn;
	const int tupidin = 0;
    const int attrin = 1;
    const int outpt = 2;
    const int MaxTheta = 1000; //3 precision after decimal
    const int MinTheta = 0; //positive values allowed
public:
    map < int , map < int, map < int , int > > > eval; //(int tupid, int attr, int simfn)
    set < int > simFns;
    set < int > attrIds;
    vector < int > finalAttrs;
    vector < int > finalTupids;
    map < int , vector < pair < int , int > > > bestThresholds;
    vector < int > bestAtom;
    //attr -> [(simfn,threshold)] 
    void addEval(int tupid, int attr, int simfn, int val){
        if (eval.find(tupid) == eval.end()){
            map < int, map < int , int > > masv;
            eval[tupid] = masv;
        }
        if (eval[tupid].find(attr) == eval[tupid].end()){
            map < int , int > msv;
            eval[tupid][attr] = msv;
        }
        if (simFns.find(simfn) == simFns.end()){
            simFns.insert(simfn);
        }
        if (attrIds.find(attr) == attrIds.end()){
            attrIds.insert(attr);
        }
        eval[tupid][attr][simfn] = val;
    }

	void addBestThresholds(int attr, int simfn, int th){
        if (bestThresholds.find(attr) == bestThresholds.end()){
            vector < pair < int , int > > msv;
            bestThresholds[attr] = msv;
        }
        bestThresholds[attr].push_back(pair<int,int>(simfn,th));
    }
	ERAtomSyn(FloatManager& _fm) :Synthesizer(_fm) {
        //initEval();//get this from a file
        
        string simfile = PARAMS->erSimEvalFName;
        Assert(simfile != "","ErAtomSynthesizer needs a SimEval File using --er-simeval-file flag to Sketch BE");
        ifstream fin(simfile);
        int tupid, attr, simfn, val;
        while(fin>>tupid){
            if (tupid >= 0 ){
	            fin>>attr>>simfn>>val;
	            addEval(tupid,attr,simfn,val);
        	}
        	else{
        		Assert(tupid == -1,"Convention for sending hardcoded thresholds here")
        		fin>>attr>>simfn>>val; // val is the threshold
        		if(bestAtom.size() == 0){
        			bestAtom.push_back(attr);
        			bestAtom.push_back(simfn);
    				bestAtom.push_back(val);
        		}
        		addBestThresholds(attr, simfn, val);
        	}
        }
        //cout<<"simFns:"<<simFns.size()<<endl;
	}
    
    virtual void finalize() {
        //Called after inout matrix is final but before it is destroyed
        InputMatrix& im = *inout;
        finalAttrs.clear();
        finalTupids.clear();
        for (int i = 0; i < inout->getNumInstances(); ++i) {
            int tupid = im.getVal(i, tupidin);
            int attr = im.getVal(i, attrin);
            finalAttrs.push_back(attr);
            finalTupids.push_back(tupid);
        }

    }
    
    void addConflicts(set< int > &conflictIds, InputMatrix& im ){
        for(auto conflictId: conflictIds){
			//cout<<"("<<im.getVal(conflictId, tupidin)<<","<<im.getVal(conflictId, attrin)<<") ";
			conflict.push(im.valueid(conflictId, tupidin));
            conflict.push(im.valueid(conflictId, attrin));
            conflict.push(im.valueid(conflictId, outpt));
		}
    }

	void addSingleConflict(int conflictId, InputMatrix& im ){
    	conflict.push(im.valueid(conflictId, tupidin));
        conflict.push(im.valueid(conflictId, attrin));
        conflict.push(im.valueid(conflictId, outpt));
    }


	virtual void backtrack(int level) {}
    //In[0] = tupleid, In[1] = attr , In[2] = output (bit)
	virtual bool synthesis(vec<Lit>& suggestions) {
		//ROHIT TODO: check if attr is set on all of them - start checking from end
		//If not, return conflict as the empty one? 
		conflict.clear();
		InputMatrix& im = *inout;
		int internalAttr = -1;
		int nI = inout->getNumInstances();
		for (int i = nI-1; i >=0 ; --i) {
			int attr = im.getVal(i, attrin);
			if (attr == EMPTY){
				//continue;
				//addSingleConflict(i,im);
				simfn=0;
				theta=500;
				return true;
			}
			else{
				Assert(internalAttr==-1 || internalAttr==attr,"Multiple attr values not allowed")
				internalAttr=attr;
				if (attrIds.find(attr) == attrIds.end()){
					//Invalid attribute ID
					conflict.push(im.valueid(i, attrin));
					return false;
				}
			}
		}

		set< int > conflictIds;
		//Assert(internalAttr>=0,"internalAttr should be +ve");
        	
        //separating the heuristic out
        if (bestThresholds.size() >0){
        	//go over these one by one in order to see which one fits all examples

        	//cout<<"bestThresholds.size()="<<bestThresholds.size()<<",internalAttr="<<internalAttr<<endl;
        	for(auto p: bestThresholds[internalAttr]){
        		int sfn = p.first;
        		int th = p.second;
        		bool atomWorks = true;
        		for(auto i: conflictIds){
        			int out = im.getVal(i, outpt);
		            int tupid = im.getVal(i, tupidin);
		            int val = eval[tupid][internalAttr][sfn];
	            	if ((out==1 && val < th) || (out==0 && val >= th)){
	            		atomWorks = false;
	            		break;
	            	}
        		}
        		if (atomWorks){
	        		for (int i = 0; i < inout->getNumInstances(); ++i) {
	        			if(conflictIds.find(i) != conflictIds.end()){
	        				continue;
	        			}
			            int out = im.getVal(i, outpt);
			            int tupid = im.getVal(i, tupidin);
			            if (out == EMPTY || tupid == EMPTY) {
			                continue;
			                //Assert(false, "no EMPTY values allowed at this point");
			            }
			            int val = eval[tupid][internalAttr][sfn];
		            	if ((out==1 && val < th) || (out==0 && val >= th)){
		            		atomWorks = false;
		            		conflictIds.insert(i);
		            		break;
		            	}
			        }
		    	}
		        if (atomWorks){
		        	simfn = sfn;
		        	theta = th;
		        	return true;
		        }
        	}
        	addConflicts(conflictIds,im);
			return false;
        }

        for (auto sfn:simFns){
        	simfn = sfn;
        	int atmost = MaxTheta;
	        int amid = -1;
	        int atleast = MinTheta;
	        int alid = -1;
	        bool simFnWorks = true;
	        for(auto i: conflictIds){
    			int out = im.getVal(i, outpt);
	            int tupid = im.getVal(i, tupidin);
	            int val = eval[tupid][internalAttr][simfn];
	            if (out == 1) { //add it to atmost computation
	                if (val <= atmost) { atmost = val; amid = i; }
	            }
	            else {//add it to atleast computation
	                if (val >= atleast) { atleast = val+1; alid = i; }
                    //theta has to be at least val + 1 for this to be a negative example
	            }
	            if (atleast > atmost && amid !=-1 && alid !=-1) {
		            conflictIds.insert(amid);
                	conflictIds.insert(alid);
                	simFnWorks=false;
                	break;
		        }
    		}
    		if(simFnWorks){
		        for (int i = 0; i < inout->getNumInstances(); ++i) {
		            if(conflictIds.find(i) != conflictIds.end()){
		            	continue;
		            }
		            int out = im.getVal(i, outpt);
		            int tupid = im.getVal(i, tupidin);
		            if (out == EMPTY || tupid == EMPTY ) {
		                continue;
		            }
		            
		            /*if (internalAttr != -1 && attr != internalAttr){
			            for (int j = 0; j < inout->getNumInstances(); ++j) {
			            	Tvalue& Tattr = inout->getTval(im.valueid(j, attrin));
			            	cout<<"j:"<<j<<" "<<Tattr<<endl;
			            }
		        	}*/
		            
		            int val = eval[tupid][internalAttr][simfn];
		            if (out == 1) { //add it to atmost computation
		                if (val <= atmost) { atmost = val; amid = i; }
		            }
		            else {//add it to atleast computation
		                if (val >= atleast) { atleast = val+1; alid = i; }
	                    //theta has to be at least val + 1 for this to be a negative example
		            }
		            if (atleast > atmost && amid !=-1 && alid !=-1) {
			            conflictIds.insert(amid);
	                	conflictIds.insert(alid);
	                	simFnWorks=false;
	                	break;
			        }
		        }
		    }
		    if(simFnWorks){
		        if (atleast <= atmost) {
		            theta = (atleast + atmost) / 2;
		            return true;
		        }
		        else {
					if (amid == -1) {
	                    //if atleast > maxTheta? It is FALSE.
	                    theta= atleast;
						return true;
					}
					if (alid == -1) {
						theta= atmost;
						return true;
					}
					//auto cpair = make_pair(gtid,ltid);
					//conflictPairs[cpair].insert(simfn);
					//if (amid != -1 && alid != -1){
					conflictIds.insert(amid);
	                conflictIds.insert(alid);
	            	//}
				}
			}
		}
		//Control comes here only if there's a conflict pair for each simFn
		//TODO: Find a greedy set cover of pairs that covers all simFns
		//For now, use naive method of taking all representative pairs
		/*for(auto cpair: conflictPairs){
			conflictIds.insert(cpair.first.first);
			conflictIds.insert(cpair.first.second);
		}*/
		//cout<<"Added Conflicts:"<<conflictIds.size()<<endl;
		
		//cout<<endl;
        addConflicts(conflictIds,im);
		return false;

	}
	virtual void newInstance() {

	}
	bool_node* getPred(int tupid, int attr, DagOptim* dopt, const vector<bool_node*>& params){
		bool_node* eq1 = new EQ_node();
		eq1->mother = params[tupidin];
		eq1->father = dopt->getCnode(tupid);
		eq1->addToParents();
		eq1 = dopt->optAdd(eq1);

		bool_node* eq2 = new EQ_node();
		eq2->mother = params[attrin];
		eq2->father = dopt->getCnode(attr);
		eq2->addToParents();
		eq2 = dopt->optAdd(eq2);
		
		bool_node* andn = new AND_node();
		andn->mother = eq1;
		andn->father = eq2;
		andn = dopt->optAdd(andn);
		
		return andn;
	}

	bool_node* getITE(int tupid, int attr, bool_node* dfun,DagOptim* dopt, const vector<bool_node*>& params){
		int val = eval[tupid][attr][simfn];
        //value of out doesn't matter to generate the function f(attr,tupid)
        ARRACC_node* an = new ARRACC_node();
        an->mother = getPred(tupid,attr,dopt,params);
        an->multi_mother.push_back(dfun);
        an->multi_mother.push_back(dopt->getCnode(val));
        an->addToParents();
        an = (ARRACC_node*)(dopt->optAdd(an));
        //cout<<"TABLE(tupid="<<tupid<<",attr="<<attr<<",simfn="<<simfn<<",val="<<val<<")"<<endl;
        return an;
	}
	virtual bool_node* getExpression(DagOptim* dopt, const vector<bool_node*>& params) {
		//Generate ITE like ARRACC nodes and use only the tupleid, attr values from the inputMatrix table
		//TODO: build this based on table stored in finalize function 
		/*InputMatrix& im = *inout;
		bool_node* dfun = dopt->getCnode(-1);

        for (int i = 0; i < inout->getNumInstances(); ++i) {
            int out = im.getVal(i, outpt);
            int tupid = im.getVal(i, tupidin);//params[0]
            int attr = im.getVal(i, attrin);//params[1]
            if (out == EMPTY || attr == EMPTY || tupid == EMPTY) {
                cout<<"out="<<out<<",tupid="<<tupid<<",attr="<<attr<<endl;
                Assert(false, "Cannot be called when any of the matrix values are empty");
            }
            dfun = getITE(tupid,attr,dfun,dopt,params);
        }*/
        //map < int , map < int, map < int , int > > > eval; //(int tupid, int attr, int simfn)
        bool_node* dfun = dopt->getCnode(-1);
        //#define FULLTABLE 1
        #ifdef FULLTABLE
        for (auto tupiditr: eval){
        	int tupid = tupiditr.first;
        	for(auto attritr: tupiditr.second){
        		int attr = attritr.first;
        		//simfn and theta is fixed
        		dfun = getITE(tupid,attr,dfun,dopt,params);
        	}
        }
        #else
        //Keeping only attr, tupid entries from the matrix
        int Nattr = finalAttrs.size();
        for(int i=0;i<Nattr;i++){
            int attr = finalAttrs[i];
            int tupid = finalTupids[i];
            dfun = getITE(tupid,attr,dfun,dopt,params);
        }
        #endif
        return dopt->addGE(dfun, dopt->getCnode(theta));
        
	}

	
	virtual void print(ostream& out) {
		out << "( SIMTH_SYNTH ( "<< simfn <<" , "<< theta << " ) )"; //IN_0 and IN_1 are two inputs
		//Just text when printing, frontend language 
        //Can be any valid Sketch code e.g. an uninterp function

	}
  
  virtual void getControls(map<string, string>& values) {
    stringstream str;
    print(str);
    values["_GEN_eratom"] = str.str();
  }
};


Synthesizer* SolverHelper::newSynthesizer(const string& name, FloatManager& _fm) {
	if (name == "_GEN_gtp") {
		return new GtpSyn(_fm);
	} else if (name == "_GEN_eratom") {
        return new ERAtomSyn(_fm);
    }
	return NULL;
}


void SolverHelper::createNumericalSynthesizer(FloatManager& _fm, BooleanDAG* dag, Interface* interf) {
    int specialVar = ((MiniSATSolver&)mng).addSpecialAssumption();
    lastVar = specialVar;
    ((MiniSATSolver&) mng).setMaxSoftLearntRestarts(PARAMS->maxRestarts);
    NumericalSynthesizer* ns = new NumericalSynthesizer(_fm, dag, interf, lfromInt(specialVar));
    numsin = ((MiniSATSolver&)mng).addSynth(ns);
    sins["NUMSIN"] = numsin;
}

void SolverHelper::addNumSynSolvClause(int inputid, int tvId) {
    ((MiniSATSolver&)mng).addSynSolvClause(numsin, 0, inputid, 1, lfromInt(tvId));
    ((MiniSATSolver&)mng).addSynSolvClause(numsin, 0, inputid, 0, lfromInt(-tvId));
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
