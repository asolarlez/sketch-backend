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
	virtual bool synthesis() {
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
    vector < int > finalAttrs;
    vector < int > finalTupids;
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
        eval[tupid][attr][simfn] = val;
    }
  
	ERAtomSyn(FloatManager& _fm) :Synthesizer(_fm) {
        //initEval();//get this from a file
        
        string simfile = PARAMS->erSimEvalFName;
        Assert(simfile != "","ErAtomSynthesizer needs a SimEval File using --er-simeval-file flag to Sketch BE");
        ifstream fin(simfile);
        int tupid, attr, simfn, val;
        while(fin>>tupid){
            fin>>attr>>simfn>>val;
            addEval(tupid,attr,simfn,val);
        }
        cout<<"simFns:"<<simFns.size()<<endl;
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

    //In[0] = tupleid, In[1] = attr , In[2] = output (bit)
	virtual bool synthesis() {
		conflict.clear();
        //Iterate over all possible simFn's and then over inout
        //For a simFm s: compare max(eval over all non-matching tupleIds) and min(eval over all matching tupleIds)
        //If found at least one:
            //collect all simFn s where it is possible to find a value between these max and min 
            //choose one randomly (?) - some heuristic can be used here
        //If not found any, generate conflict clause
        set< int > conflictIds;
		
        InputMatrix& im = *inout;
        //map < pair < int, int >, set <int> > conflictPairs;
        /*cout<<"ERSYNTH BEGIN"<<endl;
        for (int i = 0; i < inout->getNumInstances(); ++i) {
            int out = im.getVal(i, outpt);
            int tupid = im.getVal(i, tupidin);
            int attr = im.getVal(i, attrin);
            if (out == EMPTY || attr == EMPTY || tupid == EMPTY) {
                continue;
            }
            cout<<"ERSYNTH:(out="<<out<<",attr="<<attr<<",tupid="<<tupid<<")"<<endl;
        }
        cout<<"ERSYNTH END"<<endl;*/

        for (auto sfn:simFns){
        	simfn = sfn;
        	int atmost = MaxTheta;
	        int amid = -1;
	        int atleast = MinTheta;
	        int alid = -1;
	        for (int i = 0; i < inout->getNumInstances(); ++i) {
	            int out = im.getVal(i, outpt);
	            int tupid = im.getVal(i, tupidin);
	            int attr = im.getVal(i, attrin);
	            if (out == EMPTY || attr == EMPTY || tupid == EMPTY) {
	                continue;
	            }
	            int val = eval[tupid][attr][simfn];
	            if (out == 1) { //add it to atmost computation
	                if (val <= atmost) { atmost = val; amid = i; }
	            }
	            else {//add it to atleast computation
	                if (val >= atleast) { atleast = val+1; alid = i; }
                    //theta has to be at least val + 1 for this to be a negative example
	            }
	        }
	        if (atleast <= atmost) {
	            theta = (atleast + atmost) / 2;
	            return true;
	        }
	        else {
				if (amid == -1) {
                    theta= atleast;
					return true;
				}
				if (alid == -1) {
					theta= atmost;
					return true;
				}
				//auto cpair = make_pair(gtid,ltid);
				//conflictPairs[cpair].insert(simfn);
				conflictIds.insert(amid);
                conflictIds.insert(alid);
				/*conflict.push(im.valueid(gtid, tupidin));
	            conflict.push(im.valueid(gtid, attrin));
	            conflict.push(im.valueid(gtid, outpt));
				conflict.push(im.valueid(ltid, tupidin));
	            conflict.push(im.valueid(ltid, attrin));
	            conflict.push(im.valueid(ltid, outpt));
				return false; */
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

class NumericalSolver : public Synthesizer { // TODO: fix this
  BooleanDAG* dag;
  map<string, float> ctrlValMap;
  int ninputs;
  int noutputs;
  int ncontrols;
  int NUM_STEPS = 100;
  map<string, BooleanDAG*> empty;
public:
  NumericalSolver(FloatManager& _fm, BooleanDAG* _dag) :Synthesizer(_fm), dag(_dag) {
    ninputs = dag->getNodesByType(bool_node::SRC).size();
    noutputs = (dynamic_cast<TUPLE_CREATE_node*>(dag->get_node("OUTPUT")->mother))->multi_mother.size();
    ncontrols = dag->getNodesByType(bool_node::CTRL).size();
    vector<bool_node*> ctrls = dag->getNodesByType(bool_node::CTRL);
    for (int i = 0; i < ctrls.size(); i++) {
      ctrlValMap[ctrls[i]->get_name()] = 0.0;
    }
  }
  
  double acceptanceProb(double e, double e1, double T) {
    if (e1 < e) {
      return 1.0;
    }
    return exp((e - e1)/T);
  }
  
  vector<double> getNeighboringState(const vector<double>& curState) {
    vector<double> newState;
    
    for (int i = 0; i < curState.size(); i++) {
      double c = 0;
      while(true) {
        c = curState[i] - 5.0 + (rand() %100)/10.0; // Randomly permutate cur value with +/- 5 (TODO: magic numbers)
        if (c >= 0.0 && c <= 32.0) break;
      }
      newState.push_back(c);
    }
    return newState;
  }
  
  double getEnergy(vector<double> state, vector<vector<int>> allInputs, vector<vector<int>> allOutputs) {
    // TODO: fix this
    double energy = 0;
    NodeEvaluator eval(empty, *dag, fm);
    for (int i = 0; i < allInputs.size(); i++) {
      VarStore inputStore;
      // Store all inputs
      vector<bool_node*> inputs = dag->getNodesByType(bool_node::SRC);
      for (int j = 0; j < allInputs[i].size(); j++) {
        inputStore.setVarVal(inputs[j]->get_name(), allInputs[i][j]);
      }
      // Store all controls
      vector<bool_node*> ctrls = dag->getNodesByType(bool_node::CTRL);
      for (int j = 0; j < state.size(); j++) {
        inputStore.setVarVal(ctrls[j]->get_name(), fm.getIdx(state[j]));
      }
      
      eval.run(inputStore);
      /*for (int k = 0; k < dag->size(); k++) {
        bool_node* n = (*dag)[k];
        cout << n->lprint() << endl;
        if (n->type == bool_node::SRC || n->getOtype() == OutType::FLOAT) {
          cout << fm.getFloat(eval.getValue(n)) << endl;
        } else {
          cout << eval.getValue(n) << endl;
        }
      }*/
      bool_node* output = dag->get_node("OUTPUT")->mother;
      vector<bool_node*> outputmothers = ((TUPLE_CREATE_node*)output)->multi_mother;
      vector<int> outputs = eval.getTuple(eval.getValue(output));
      for (int j = 0; j < outputs.size(); j++) {
        //cout << outputs[j] << endl;
        if (outputs[j] != allOutputs[i][j]) {
          float m1 = fm.getFloat(eval.getValue(outputmothers[j]->mother)); // TODO: this assumes all values are floats which is probably true, but still we need to check
          float m2 = fm.getFloat(eval.getValue(outputmothers[j]->father));
          energy += pow((m1 - m2), 2);
        }
      }
      
    }
    //cout << energy << endl;
    return energy;
  }
  
  virtual bool synthesis() {
    conflict.clear();
    InputMatrix& im = *inout;

    vector<vector<int>> allInputs;
    vector<vector<int>> allOutputs;
    vector<int> conflictids;
    
    for (int i = 0; i < inout->getNumInstances(); ++i) {
      vector<int> inputs;
      vector<int> outputs;
      bool notset = false;
      for (int j = 0; j < ninputs; j++) {
        int val = im.getVal(i, j);
        if (val == EMPTY) {
          notset = true;
          break;
        }
        inputs.push_back(val);
      }
      if (notset) continue;
      for (int j = ninputs; j < ninputs + noutputs; j++) {
        int val = im.getVal(i, j);
        if (val == EMPTY) {
          notset = true;
          break;
        }
        outputs.push_back(val);
      }
      if (notset) continue;
      allInputs.push_back(inputs);
      allOutputs.push_back(outputs);
      conflictids.push_back(i);
      for (int k = 0; k < inputs.size(); k++) {
        //cout << "Input" << k << ": " << inputs[k] << endl;
      }
      for (int k = 0; k < outputs.size(); k++) {
        //cout << "Output" << k << ": " << outputs[k] << endl;
      }
      if (!notset) {
        cout << "Found a input output pair" << endl;
      }
      
    }
    
    Assert(allInputs.size() == allOutputs.size(), "This should not be possible");
    
    if (allInputs.size() == 0) return true;
    // Minimize Sum((dag(inputs, ctrls) - outputs)**2)
    
    double T = 10;
    double coolingRate = 0.1;
    
    vector<double> curState;
    for (int i = 0; i < ncontrols; i++) {
      double c = (rand() % 320) / 10.0; // random float from 0 to 32 (TODO: magic number)
      curState.push_back(c);
    }
    double curEnergy = getEnergy(curState, allInputs, allOutputs);
    
    for (int i = 0; i < NUM_STEPS; i++) {
      //cout << "state: " << curState[0] << " energy: " << curEnergy << endl;
      if (curEnergy == 0.0) break;
      vector<double> nextState = getNeighboringState(curState);
      double nextEnergy = getEnergy(nextState, allInputs, allOutputs);
      //cout << "next state: " << nextState[0] << " energy: " << nextEnergy << endl;
      double prob = acceptanceProb(curEnergy, nextEnergy, T);
      double randflip = (rand()%10)/10.0;
      //cout << "prob: " << prob << " randflip: " << randflip << endl;
      if (prob >= randflip) {
        //cout << "Transitioning to next state" << endl;
        curState = nextState;
        curEnergy = nextEnergy;
      }
      T = T*(1 - coolingRate);
    }

    if (curEnergy == 0.0) {
       // Update the controls
      vector<bool_node*> ctrls = dag->getNodesByType(bool_node::CTRL);
      for (int i = 0; i < ctrls.size(); i++) {
        ctrlValMap[ctrls[i]->get_name()] = curState[i];
      }
      return true;
    } else {
      cout << "Found a conflict" << endl;
      //dag->lprint(cout);
      // For now, just treat everything as conflict
      for(auto conflictId: conflictids) {
        for (int j = 0; j < ninputs + noutputs; j++) {
          conflict.push(im.valueid(conflictId, j));
        }
      }
      return false;
    }
  }
  virtual void newInstance() {
    
  }
  
  virtual void finalize() {
    
  }
  
  virtual bool_node* getExpression(DagOptim* dopt, const vector<bool_node*>& params) {
    // Add the appropriate expression from the dag after replacing inputs with params and ctrls with synthesized parameters
    BooleanDAG newdag = *(dag->clone());
    int src_counter = 0;
    for (int i = 0; i < newdag.size(); i++) {
      if (newdag[i]->type == bool_node::SRC) {
        newdag.replace(i, params[src_counter++]); // TODO: is this correct?
      } else if (newdag[i]->type == bool_node::CTRL) {
        newdag.replace(i, dopt->getCnode(ctrlValMap[newdag[i]->get_name()]));
      } else {
        if (newdag[i]->type != bool_node::DST) {
          bool_node* n = dopt->computeOptim(newdag[i]);
          if (n == newdag[i]) {
            dopt->addNode(n);
          }
          if (newdag[i] != n) {
            newdag.replace(i, n);
          }
        }
      }
    }
    return newdag.get_node("OUTPUT")->mother;
  }
  
  virtual void print(ostream& out) {
    out << "( ";
    vector<bool_node*> ctrls = dag->getNodesByType(bool_node::CTRL);
    for (int i = 0; i < ctrls.size(); i++) {
      out << ctrlValMap[ctrls[i]->get_name()] << ", ";
    }
    out << " )";
  }
};

Synthesizer* SolverHelper::newSynthesizer(const string& name, FloatManager& _fm) {
	if (name == "_GEN_gtp") {
		return new GtpSyn(_fm);
	} else if (name == "_GEN_eratom") {
    return new ERAtomSyn(_fm);
  } else if (name.find("_GEN_NUM_SYNTH") == 0) {
    return new NumericalSolver(_fm, numericalAbsMap[name]);
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
