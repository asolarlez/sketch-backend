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

enum ArithType {Variable, Plus, Times, Minus, Div, Mod, Const};


class ArithExpression {
	ArithType type;
	ArithExpression* mother;
	ArithExpression* father;
	string name;
	int val;
	bool isdead;

public:
	static const int Undef = -4798773;
	static string AT2str(ArithType at){
		switch(at){
			case Variable: return "V";
			case Const: return "C";
			case Plus: return "+";
			case Times: return "*";
			case Minus: return "-";
			case Div: return "/";
			case Mod: return "%";
		}
	}
	static string ATset2str(set <ArithType> atSet){
		string s = "";
		for (auto at: atSet ){
			s += AT2str(at);
		}
		return s;
	}
	bool isCommutative(ArithType at){
		return (at == Plus || at == Times);
	}
	ArithExpression(ArithType at,int varid){
		Assert(at == Variable || at == Const,"Should be Variable or Const");
		type = at;
		val = varid;
		mother = NULL;
		father = NULL;
		name = getSig();
	}
	ArithExpression(ArithType at, ArithExpression* m, ArithExpression* f){
		type = at;
		mother = m;
		father = f;
		name = getSig();
		val = -333;
	}

	bool_node* getDag(DagOptim* dopt, const vector<bool_node*>& params) {
		bool_node* mnode;
		bool_node* fnode;
		if (mother != NULL){
			mnode = mother->getDag(dopt,params);
		}
		if (father != NULL){
			fnode = father->getDag(dopt,params);
		}

		bool_node* rnode;
		switch(type){
			case Const: {
				return dopt->getCnode(val);
			}
			case Variable: {
				return params[val];
			}
			case Plus: {
				rnode = new PLUS_node(); break;
			}
			case Times: {
				rnode = new TIMES_node(); break;
			}
			case Minus: {
				rnode = new PLUS_node();
				rnode->mother =mnode;

				rnode->father = new NEG_node();
				rnode->father->mother = fnode;
				rnode->father->addToParents();
				rnode->father = dopt->optAdd(rnode->father);
				
				rnode->addToParents();
				rnode = dopt->optAdd(rnode);
				return rnode;
			}
			case Div: {
				rnode = new DIV_node(); break;
			}
			case Mod: {
				rnode = new MOD_node(); break;
			}
		}
		rnode->mother = mnode;
		rnode->father = fnode;
		rnode->addToParents();
		rnode = dopt->optAdd(rnode);
		return rnode;
	}
	string getFEOut(){
		if (type == Const){
			return to_string(val);
		}
		else if (type == Variable){
			return "( IN_" + to_string(val) +" )";
		}
		
		
		string msig = "";
		string fsig = "";
		if (mother != NULL){
			msig = mother->getFEOut();
		}
		if (father != NULL){
			fsig = father->getFEOut();
		}
		if (isCommutative(type)){
			if (msig > fsig){
				string tmp = msig;
				msig = fsig;
				fsig = tmp;
			}
		}
		return "( " + msig + " " + AT2str(type) + " " + fsig + ")";
	}
	string getSig(){
		if (this->name.size() > 0 ) return this->name;
		if (type == Const){
			return to_string(val);
		}
		else if (type == Variable){
			return "v" + to_string(val);
		}


		string msig = "";
		string fsig = "";
		if (mother != NULL){
			msig = mother->getSig();
		}
		if (father != NULL){
			fsig = father->getSig();
		}
		if (isCommutative(type)){
			if (msig > fsig){
				string tmp = msig;
				msig = fsig;
				fsig = tmp;
			}
		}
		return "(" + msig + AT2str(type) + fsig + ")"; 
	}
	int evaluate(vector<int> &values){
		int mval = -333;
		int fval = -333;
		if (mother != NULL){
			mval = mother->evaluate(values);
			if (mval == Undef){
				return Undef;
			}
		}
		if (father != NULL){
			fval = father->evaluate(values);
			if (fval == Undef){
				return Undef;
			}
		}
		
		switch (type){
			case Const: return this->val;
			case Variable: return values[this->val];
			case Plus: return mval + fval;
			case Times: return mval * fval;
			case Minus: return mval - fval;
			case Div: {
				if (fval == 0){
					return Undef;
				}
				else return mval / fval;
			}
			case Mod: {
				if (fval <= 0){
					return Undef;
				}
				else return mval % fval;
			}
		}
	}
};

//TODO: Arith Expr Builder - should resuse as many nodes (vars, consts, ops etc)
//based on signatures of nodes. Maintaing a map sig -> AithExpression*
//Building in a layered manner - make all depth 1. Then 2 and then 3 and so on.
//Take two candidates from previous level; take an Op. Make potentially two expressions 
// (one if op is commutative)
// for V variables, C constants, K ops. Level 1 = V+C exprs, Level 2=  (L1^C_2)*2 (U-bound)
// Level3 = O((V+C)^4), Level4 = O((V+C)^8) ...

class ArithExprBuilder {
	//for given numvars, consts, set of ArithTypes (all represented as a string), 
	//a given level of depth - a static set of expressions
	//TODO: Avoid this memory leak!?
	static map < string , map < int, set < ArithExpression* > > >  ASetMap;
	//A static map of signature to Expressions to avoid recomputation
	static map < string, ArithExpression *> ASigMap;
	int numvars;
	set<int>& consts;
	set <ArithType>& ops;
	string accstr;
public:
	static void clearStaticMapMemory(){
		for(auto& kv: ASigMap){
			delete kv.second; // all expressions ever generated & not deleted immediately should be here in the map
		}
	} 
	ArithExprBuilder(int numvars, set<int> &consts, set <ArithType> &ops): consts(consts), ops(ops) {
		string opstr = ArithExpression::ATset2str(ops);
		this->numvars = numvars;
		string constsStr = "$";
		for (int c: consts){
			constsStr  += to_string(c) + "$";
		}
		this->accstr = to_string(numvars)+ "#" + constsStr + "#" + opstr;
		if (ASetMap.find(accstr) == ASetMap.end()){
			ASetMap[accstr] = map < int, set < ArithExpression* > >();
		}
	}
	void addToMaps(ArithType op, ArithExpression* ae1, ArithExpression* ae2, int d){
		string msig =ae1->getSig();
		string fsig =ae2->getSig();
		if (ae1->isCommutative(op)){
			if (msig > fsig){
				string tmp = msig;
				msig = fsig;
				fsig = tmp;
			}
		}
		string checkSig = "(" + msig + ArithExpression::AT2str(op) + fsig + ")";
		if (ASigMap.find(checkSig) == ASigMap.end()){
			//cannot find sig
			ArithExpression* ae = new ArithExpression(op,ae1,ae2);
			ASigMap[checkSig] = ae;
			ASetMap[accstr][d].insert(ae);
		}
	}
	
	void addToMapsD1(ArithType op, int cival){
		string checkSig;
		if (op == Variable){
			checkSig = ArithExpression::AT2str(op)+to_string(cival);
		}
		else if (op == Const){
			checkSig = to_string(cival);
		}
		else{
			Assert(false, "depth 1 can onlye be var or const");
		}

		if (ASigMap.find(checkSig) == ASigMap.end()){
			//cannot find sig
			ArithExpression* ae = new ArithExpression(op,cival);
			ASigMap[checkSig] = ae;
			ASetMap[accstr][1].insert(ae);
		}
	}

	
	void getExpressionsUpto(int dm1, set < ArithExpression* >  &s){
		for(int d=1;d<=dm1;d++){
			Assert(ASetMap[accstr].find(d) != ASetMap[accstr].end(),"This depth should have been worked out already");
			
			for(ArithExpression* ae: ASetMap[accstr][d]){
				s.insert(ae);
			}
		}
	}
	set < ArithExpression* > & getExpressions(int depth){
		if (ASetMap[accstr].find(depth) != ASetMap[accstr].end()){
			return ASetMap[accstr][depth];
		}
		else {
			ASetMap[accstr][depth] = set <ArithExpression*>();
		}
		cout<<"AithExprCustomSynth: Generating all expressions of depth: "<<depth<<endl;
		

		Assert(depth >= 1, "depth should be GTE 1");
		if (depth == 1){
			//variables and constants
			for (int i=0;i<numvars;i++){
				addToMapsD1(Variable,i);
			}
			for(int c: consts){
				addToMapsD1(Const,c);
			}
		}
		else{
			set < ArithExpression* > aeSetdm1;
			getExpressionsUpto(depth-1,aeSetdm1);
			for(ArithExpression* ae1: aeSetdm1){
				for(ArithExpression* ae2: aeSetdm1){
					for(ArithType op : ops ){
						addToMaps(op, ae1, ae2, depth);
					}
				}
			}
		}
		cout<<"AithExprCustomSynth: Generated all expressions of depth: "<<depth<< ", Total Expressions:"<< ASetMap[accstr][depth].size()<<endl;
		
		return ASetMap[accstr][depth];
	}
};
