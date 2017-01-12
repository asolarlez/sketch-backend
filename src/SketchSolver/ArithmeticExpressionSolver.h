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
#include "ArithmeticExpressionBuilder.h"

class ArithExprSyn : public Synthesizer {
	ArithExpression* expr;
	int depth;
	int numvars;
	set<int> consts;
	set <ArithType> ops;
	ArithExprBuilder* ab;
	int prevNExamples;
public:
    
	ArithExprSyn(FloatManager& _fm) :Synthesizer(_fm) {
		expr = NULL;
		depth = 3;
		numvars = 3;
		ops.insert(Plus);
		ops.insert(Times);
		consts = set<int>();
		ab = NULL;
		prevNExamples=0;
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
		ab = new ArithExprBuilder(numvars, consts, ops);
	}
    
    virtual void finalize() {
        //Called after inout matrix is final but before it is destroyed
    }
    
    void addConflicts(set< int > &conflictIds, InputMatrix& im ){
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
		Assert(d>=1, "depth should be at least 1");
		Assert(ab != NULL, "need Arithmetic Builder Instantiated by now");
		if (nExamples == prevNExamples){
			if (expr == NULL){
				expr=*(ab->getExpressions(1).begin());
			}
			return true;
		}
		else prevNExamples = nExamples;
		
		set<int> conflictIds;
		for(int cDepth = 1; cDepth <=d; cDepth++){
			
			set<ArithExpression*>&possibleExpressions = ab->getExpressions(cDepth);
			//Go over all current expressions
			auto it_ae = possibleExpressions.begin();
			while (it_ae != possibleExpressions.end()){
				//Go over all examples, try expression on all examples
				ArithExpression* ae = *it_ae;
				bool expWorks = true;
				for(int ei=0;ei<nExamples;ei++){
					int evalout = ae->evaluate(inputs[ei]);
					
					if (evalout == ArithExpression::Undef || evalout != outputs[ei]){
						//this expression doesn't work
						expWorks = false;
						conflictIds.insert(exampleIds[ei]);
						break;
					}			
				}
				if (expWorks){
					//return true if Found an expression that works on the examples
					expr = ae;
					cout<<"Used "<<nExamples<<" examples. ArithExpr: "<<ae->getSig()<<endl;
					return true;
				}
				++it_ae;
				/*else{ //Not sound if there are more than one instances of the same synthesizer and they share this set of expressions
				 //Also, removing from main source limits the next level : UNSOUND
					it_ae = possibleExpressions.erase(it_ae);
				}*/
			}
			
			
		}
		//if control comes here i.e.
		//if no expressions work, TODO: try greedy set cover of examples as conflict
		//for now, return all examples as the conflict	
		addConflicts(conflictIds,im);//TODO:Can do smarter
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
		out<<expr->getFEOut();
		//out << "( SIMTH_SYNTH ( "<< simfn <<" , "<< theta << " ) )"; //IN_0 and IN_1 are inputs
		//Just text when printing, frontend language 
        //Can be any valid Sketch code e.g. an uninterp function

	}
};
