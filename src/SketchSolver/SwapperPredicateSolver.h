#include <fstream>
#include <iostream>
#include <vector>
#include <queue>
#include <set>
#include <unordered_map>
#include <algorithm>
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
#include "SwapperPredicateBuilder.h"
#define DBGCUSTOM false

using namespace SwapperPredicateNS;
class SwapperPredicateSyn : public Synthesizer {
	SwapperPredicate* expr;
	int depth;
	vector<int> bits;
	vector<int> intsOrbits;
	set<int> consts;
	//set <NodeType> ops;
	PredicateBuilder* ab;
	vector< vector<int> > prevInputs;
	bool prevWasUNSAT;
	string name;
  timerclass stimer;

public:
	
	SwapperPredicateSyn(FloatManager& _fm) :Synthesizer(_fm) {
		expr = NULL;
		depth = 2;
		consts = set<int>();
		ab = NULL;
		prevWasUNSAT=false;
	}
	~SwapperPredicateSyn(){
		if (ab != NULL){
			delete ab;
		}
	}
	void setupParams(string name){
		this->name = name;
		string ncopy = name;
		replace(ncopy.begin(), ncopy.end(), '_', ' ');
		stringstream ss(ncopy);
		string s;
		ss>>s;
		Assert(s=="GEN","invalid name: " + name);
		ss>>s;
		Assert(s=="swapperpred","invalid name: " + name);
		//swapperpred_b_b_i_b_i_c0_c1_2d
		int ind = 0;
		while(ss>>s){
			if (s == "b") {
				bits.push_back(ind);
				intsOrbits.push_back(ind);
				ind++;
			}
			else if (s == "i") {
				intsOrbits.push_back(ind);
				ind++;
			}
			else if (s[0] == 'c') {
				consts.insert(atoi(s.substr(1, s.length() - 1).c_str()));
			}
			else if(s[s.length()-1] == 'd'){
				depth = atoi(s.substr(0,s.length()-1).c_str());
				Assert(depth >= 1,"invalid depth: " + s);
			}
		}
		
		setupBuilder();
	}
	void addConst(int c){
		consts.insert(c);
	}
	void setDepth(int d){
		depth = d;
	}

	void setupBuilder(){
		ab = new PredicateBuilder(bits, intsOrbits, consts, depth);
	}
    
    virtual void finalize() {
        //Called after inout matrix is final but before it is destroyed
    }
    
    void addConflicts(vector< int > &conflictIds, InputMatrix& im ){
        for(auto conflictId: conflictIds){
			//cout<<"("<<im.getVal(conflictId, tupidin)<<","<<im.getVal(conflictId, attrin)<<") ";
			addSingleConflict(conflictId,im);
		}
    }

	void addSingleConflict(int &conflictId, InputMatrix& im ){
    	for(int j=0;j<=intsOrbits.size();j++){
			conflict.push(getLit(im.valueid(conflictId, j)));
		}
    }

    bool tryExpressions(int d){
    	//Try all expressions bounded by depth d
    	InputMatrix& im = *inout;
		int nI = inout->getNumInstances();
		vector < vector < int > > inputs;
		vector < bool > outputs; 
		vector<int> exampleIds;
		int numvars = intsOrbits.size();
		unordered_map<string, int> inputsMap; // track input string to idx to get rid of repeated inputs
		for (int i = 0; i < nI; ++i) {
			stringstream ss;
			int out = im.getVal(i, numvars);
			if (out == EMPTY){
				continue;
			}
			vector<int> vals;
			bool foundEmptyVariable = false;
			for(int j=0;j<numvars;j++){
				int v = im.getVal(i, j);
				if (v == EMPTY){
					foundEmptyVariable = true;
					break;
				}
				ss << v << "#";
				vals.push_back(v);
			}
			if (foundEmptyVariable){
				continue;
			}
			Assert(out == 0 || out == 1, "Only boolean output expected!");
			string s = ss.str();
			if (inputsMap.find(s) != inputsMap.end()) {
				int idx = inputsMap[s];
				if (im.getVal(idx, numvars) != out) {
					//found a conflict
					addSingleConflict(idx, im);
					addSingleConflict(i, im);
					if (DBGCUSTOM) {
					cout << "CONFLICT: same input different output" << endl;
					}
					return false;
				}
			} else {
				exampleIds.push_back(i);
				inputs.push_back(vals);
				outputs.push_back(out);
				inputsMap[s] = i;
			}
		}
		inputsMap.clear();
		int nExamples = (int)outputs.size();
		/*
		for(int io=0;io<nExamples;io++){
			cout<<"Inputs: ";
			for(int inp: inputs[io]){
				cout<<inp<<" ";
			}
			cout<<" Output: "<<outputs[io]<<endl;
		}*/

		if(DBGCUSTOM) cout << "IOs: " << ab->getIOStrings(inputs, outputs) << endl;
		
		//Evaluate previous expr on each example and return true if they all satisfy
		if (expr != NULL){
			if(nExamples == 0) return true;
			bool allSatisfied = true;
			for(int io=0;io<nExamples;io++){
				bool outv = expr->evaluate(inputs[io]);
				if (outv != outputs[io]){
					allSatisfied = false;
					break;
				}
			}
			if(allSatisfied){
				if (DBGCUSTOM) cout << "Used " << nExamples << " examples. Previous SwapperPred: " << expr->getSig() << endl;
				return true;
			}
		}
		else{
			Assert(nExamples == 0,"expr == NULL Can only happen in the case when nExamples == 0.");
			expr=(ab->getExpression(1,inputs,outputs));
			if(expr == NULL){
				expr=ab->getFirstExpression();
			}
			return true;
		}

    /*if (DBGCUSTOM) {
		//if there are two equal inputs
		for (int j1 = 0; j1 < nExamples; j1++) {
			for (int j2 = 0; j2 < j1; j2++) {
				if (inputs[j1] == inputs[j2]) {
					Assert(false,"This should have been taken catre of")
				}
			}
		}
    }*/
		
		//if we see the same inputs as last time, we check if there's an expression for these outputs
		if(prevInputs.size() > 0 && prevInputs == inputs){
			SwapperPredicate* aePrev = ab->getExpressionForOutputs(outputs);
			if (aePrev != NULL){
				expr = aePrev;
				if (DBGCUSTOM) cout << "Used " << nExamples << " examples. Previous Inputs based SwapperPred: " << expr->getSig() << endl;
				return true;
			}else{
				if(prevWasUNSAT){
					addConflicts(exampleIds,im);//TODO:Can do smarter
          if (DBGCUSTOM) cout << "Conflict: Previous inputs based." << endl;
					return false;
				}
			}
		}
		prevInputs=inputs;
		Assert(d>=1, "depth should be at least 1");
		Assert(ab != NULL, "need Arithmetic Builder Instantiated by now");
		
		//clear depth -> expr map
		ab->clearSetMap();
		for(int cDepth = 1; cDepth <=d; cDepth++){
			
			SwapperPredicate* currExpr = ab->getExpression(cDepth,inputs,outputs);
			
			if (currExpr !=NULL){
				expr = currExpr;
				if (DBGCUSTOM) cout<<"Used "<<nExamples<<" examples. SwapperPred: "<<expr->getSig()<<endl;
				return true;
			}
			
		}
		//if control comes here i.e.
		//if no expressions work, TODO: try greedy set cover of examples as conflict
		//for now, return all examples as the conflict
      //cout <<"Total exprs:" << (ab->PSetMap[1].size() +  ab->PSetMap[2].size()) << endl;
    if (DBGCUSTOM) cout << "CONFLICT" << endl;
		addConflicts(exampleIds,im);//TODO:Can do smarter
		prevWasUNSAT = true;
		return false;
    }
  
  void getSuggestions(vec<Lit>& suggestions) {
    int numvars = intsOrbits.size();
    suggestions.clear();
    InputMatrix& im = *inout;
    int nI = inout->getNumInstances();
    
    for (int i = 0; i < nI; ++i) {
      int out = im.getVal(i, numvars);
      vector<int> vals;
      bool foundEmptyVariable = false;
      for(int j=0;j<numvars;j++){
        int v = im.getVal(i, j);
        if (v == EMPTY){
          foundEmptyVariable = true;
          break;
        }
        vals.push_back(v);
      }
      if (foundEmptyVariable){
        continue;
      }
      bool eout = expr->evaluate(vals);
      if (out == EMPTY || out != (int)eout) {
        Lit v = getLit(im.valueid(i, numvars), eout);
        suggestions.push(v);
        if (DBGCUSTOM) {
          cout << "Suggesting (" << i << ", " << numvars << ") = " << eout << " Lit: " << toInt(v) << endl;
        }
      }
    }
  }
    //In[0] = tupleid, In[1] = attr , In[2] = output (bit)
	virtual bool synthesis(vec<Lit>& suggestions) {
		//x_index = 0, y_index = 1, z_index = 2,...
		//0 -> numvars-1 : indices for variables
		//numvars : index for output
    if (DBGCUSTOM) stimer.restart();
		conflict.clear();
		bool b = tryExpressions(depth);
		getSuggestions(suggestions);
		if (b) prevWasUNSAT = false;
		if (DBGCUSTOM) stimer.stop().print("SwapperTime: ");
		return b;
	}
	
	virtual bool_node* getExpression(DagOptim* dopt, const vector<bool_node*>& params) {
		
		//params correspond to the variables
		//generate the arithmetic expression

		return expr->getDag(dopt,params);
	}
	virtual void newInstance() {

	}

	virtual void print(ostream& out) {
		//cout<<"Giving to FE: "<<expr->getSig()<<" || "<< expr->getFEOut()<<endl;
		out<<expr->getFEOut();
		//out << "( SIMTH_SYNTH ( "<< simfn <<" , "<< theta << " ) )"; //IN_0 and IN_1 are inputs
		//Just text when printing, frontend language 
        //Can be any valid Sketch code e.g. an uninterp function

	}
};
