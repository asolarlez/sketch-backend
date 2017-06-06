#include <fstream>
#include <iostream>
#include <vector>
#include <queue>
#include <set>
#include <map>
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
			conflict.push(getLit(im.valueid(conflictId, tupidin)));
			conflict.push(getLit(im.valueid(conflictId, attrin)));
			conflict.push(getLit(im.valueid(conflictId, outpt)));
		}
	}
	
	void addSingleConflict(int conflictId, InputMatrix& im ){
		conflict.push(getLit(im.valueid(conflictId, tupidin)));
		conflict.push(getLit(im.valueid(conflictId, attrin)));
		conflict.push(getLit(im.valueid(conflictId, outpt)));
	}
	
	
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
					conflict.push(getLit(im.valueid(i, attrin)));
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
};
