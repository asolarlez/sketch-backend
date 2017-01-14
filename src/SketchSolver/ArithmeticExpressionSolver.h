#include <fstream>
#include <iostream>
#include <vector>
#include <queue>
#include <set>
#include <map>
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
#include "ArithmeticExpressionBuilder.h"

class ArithExprSyn : public Synthesizer {
	ArithExpression* expr;
	int depth;
	int numvars;
	set<int> consts;
	set <ArithType> ops;
	ArithExprBuilder* ab;public:
	inline string getIOString(vector<int> &input, int output){
		string s="";
		for(auto inp:input){
			s+="#"+to_string(inp);
		}
		s+="#"+to_string(output);
		return s;
	}
	ArithExprSyn(FloatManager& _fm) :Synthesizer(_fm) {
		expr = NULL;
		depth = 3;
		numvars = 3;
		ops.insert(Plus);
		ops.insert(Times);
		consts = set<int>();
		ab = NULL;
		//TODO: Set these params based on name of the uninterp function
		//while registering the function
	}
	~ArithExprSyn(){
		if (ab != NULL){
			delete ab;
		}
	}
	void addConst(int c){
		consts.insert(c);
	}
	void setOps(set<ArithType> opsinp){
		ops = opsinp;
	}
	void setDepth(int d){
		depth = d;
	}

	void setNumVariables(int nv){
		numvars = nv;
	}
	void setupBuilder(){
		ab = new ArithExprBuilder(numvars, consts, ops, depth);
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
    	for(int j=0;j<=numvars;j++){
			conflict.push(im.valueid(conflictId, j));
		}
    }

    bool tryExpressions(int d){
    	//Try all expressions bounded by depth d
    	InputMatrix& im = *inout;
		int nI = inout->getNumInstances();
		vector < vector < int > > inputs;
		vector < int > outputs; 
		vector<int> exampleIds;
		
		for (int i = 0; i < nI; ++i) {
			
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
				vals.push_back(v);
			}
			if (foundEmptyVariable){
				continue;
			}
			
			exampleIds.push_back(i);
			inputs.push_back(vals);
			outputs.push_back(out);
			
		}
		int nExamples = (int)outputs.size();
		/*
		for(int io=0;io<nExamples;io++){
			cout<<"Inputs: ";
			for(int inp: inputs[io]){
				cout<<inp<<" ";
			}
			cout<<" Output: "<<outputs[io]<<endl;
		}*/
		
		//Evaluate previous expr on each example and return true if they all satisfy
		if (expr != NULL){
			if(nExamples == 0) return true;
			bool allSatisfied = true;
			for(int io=0;io<nExamples;io++){
				int outv = expr->evaluate(inputs[io]);
				if (outv != outputs[io]){
					allSatisfied = false;
					break;
				}
			}
			if(allSatisfied){
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

		Assert(d>=1, "depth should be at least 1");
		Assert(ab != NULL, "need Arithmetic Builder Instantiated by now");
		
		//clear depth -> expr map
		ab->clearSetMap();
		for(int cDepth = 1; cDepth <=d; cDepth++){
			
			ArithExpression* currExpr = ab->getExpression(cDepth,inputs,outputs);
			
			if (currExpr !=NULL){
				expr = currExpr;
				cout<<"Used "<<nExamples<<" examples. ArithExpr: "<<expr->getSig()<<endl;
				return true;
			}
			
		}
		//if control comes here i.e.
		//if no expressions work, TODO: try greedy set cover of examples as conflict
		//for now, return all examples as the conflict	
		addConflicts(exampleIds,im);//TODO:Can do smarter
		return false;
    }
    //In[0] = tupleid, In[1] = attr , In[2] = output (bit)
	virtual bool synthesis() {
		//x_index = 0, y_index = 1, z_index = 2,...
		//0 -> numvars-1 : indices for variables
		//numvars : index for output
		conflict.clear();
		bool b = tryExpressions(depth);
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
