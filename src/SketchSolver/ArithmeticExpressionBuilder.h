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
	
	bool isdead;
	int depth;

public:
	int val;
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
		depth=1;
	}
	ArithExpression(ArithType at, ArithExpression* m, ArithExpression* f){
		type = at;
		mother = m;
		father = f;
		name = getSig();
		val = -333;
		depth = max(m->getDepth(),f->getDepth())+1;
	}
	int getDepth(){
		return depth;
	}
	ArithExpression* getMother(){
		return mother;
	}
	ArithExpression* getFather(){
		return father;
	}
	ArithType getOp(){
		return type;
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
		return "( " + msig + " " + AT2str(type) + " " + fsig + " )";
	}
	
	void getOutputs(vector< vector<int> > &inputs, vector<int> &outputs){
		outputs.clear();
		for(auto input: inputs){
			outputs.push_back(this->evaluate(input));
		}
	}
	
	string getSig(){
		if (this->name.size() > 0 ) return this->name;
		if (type == Const){
			return to_string(val);
		}
		else if (type == Variable){
			return "V" + to_string(val);
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
//based on signatures of nodes. Maintaing a map sig -> ArithExpression*
//Building in a layered manner - make all depth 1. Then 2 and then 3 and so on.
//Take two candidates from previous level; take an Op. Make potentially two expressions 
// (one if op is commutative)
// for V variables, C constants, K ops. Level 1 = V+C exprs, Level 2=  (L1^C_2)*2 (U-bound)
// Level3 = O((V+C)^4), Level4 = O((V+C)^8) ...

class ArithExprBuilder {
	//For a given level of depth - a set of expressions
	map < int, set < ArithExpression* > >  ASetMap;
	//A static map of signature to Expressions to avoid recomputation
	static map < string, ArithExpression *> ASigMap;
	int numvars;
	set<int>& consts;
	set <ArithType>& ops;
	int maxDepth;
	map < vector< int > , ArithExpression* > outputsAMap;
public:
	static void clearStaticMapMemory(){
		for(auto& kv: ASigMap){
			delete kv.second; // all expressions ever generated & not deleted immediately should be here in the map
		}
	}
	ArithExprBuilder(int numvars, set<int> &consts, set <ArithType> &ops, int mDepth): consts(consts), ops(ops) {
		string opstr = ArithExpression::ATset2str(ops);
		this->numvars = numvars;
		string constsStr = "";
		for (int c: consts){
			constsStr  += to_string(c) + "c";
		}
		string accstr = to_string(numvars)+ "v" + constsStr + opstr;
		maxDepth = mDepth;
		cout<<"ArithExprBuilder Built for: "<<accstr<<endl;
	}
	ArithExpression* addToMaps(ArithType op, ArithExpression* ae1, ArithExpression* ae2, int d, vector< vector<int> > &inputs, vector<int> & neededOutputs){
		ArithType lop = ae1->getOp();
		ArithType rop = ae2->getOp();
		
		//TODO: Check if the expression is "simplifiable" or
		// "canonicalizable" -> reject those
		
		if (ae1->isCommutative(op)){
			//(a+b)+(c+d) - make sure a<=b<=c<=d (sig) - issue with depth? nope.
			// + can be replaced with *
			if(lop == op && rop == op){
				string b = ae1->getFather()->getSig();
				string c = ae2->getMother()->getSig();
				if (b>c) {
					return NULL;
				}
			}
			//(a+b)+c - make sure a<=b<=c (sig) - issue with depth? -> check c->depth != maxdepth-1
			if(lop == op && ae2->getDepth() < maxDepth-1 ){
				string b = ae1->getFather()->getSig();
				string c = ae2->getSig();
				if(b>c){
					return NULL;
				}
			}
			//a+(b+c) - make sure a<=b<=c (sig) - issue with depth? -> check a->depth != maxdepth-1
			if(rop == op && ae1->getDepth() < maxDepth-1 ){
				string a = ae1->getSig();
				string b = ae2->getMother()->getSig();
				if(a>b){
					return NULL;
				}
			}
		}
		if (op == Plus){
			//(a*b)+(c*b) -> symmetries?
			if(lop == rop && lop == Times){
				string a = ae1->getMother()->getSig();
				string b = ae1->getFather()->getSig();
				string c = ae2->getMother()->getSig();
				string d = ae2->getFather()->getSig();
				if (a == c || a == d || b == c || b ==d) {
					return NULL;
				}
			}
			//(a%b)+(c%b)
			//(a/b)+(c/b)
			if(lop == rop && (lop == Mod || lop == Div) && ae1->getFather()->getSig() == ae2->getFather()->getSig() ){
				return NULL;
			}
			//a+a if 2 \in consts and Times \in ops
			if(ae1->getSig() == ae2->getSig()) return NULL;
		}
		if (op == Div){
			// x / x (if C1 in consts)
			if(ae1->getSig() == ae2->getSig()){
				return NULL;
			}
			// (a/b)/c = a/(b*c) "canonical" OR a/(b/c) = (a*c)/b
			if (lop == Div || rop == Div) {
				return NULL;
			}
			// x/C1
			if(rop == Const && ae2->val == 1) return NULL;
			
			//(a%b) / b = a%b
			if(lop == Mod && ae1->getFather()->getSig() == ae2->getSig()){
				return NULL;
			}
			
			//1/x is 0 or 1
			if(lop == Const && (ae1->val ==1 || ae1->val == 0)){
				return NULL;
			}
			
		}
		if(op == Mod){
			// x % x (if C0 in consts)
			if(ae1->getSig() == ae2->getSig()){
				return NULL;
			}
			// (a%b)%c = a%c "canonical"
			if (lop == Mod) {
				return NULL;
			}
			// x%C1 or C0 (note x%C1 == 0 but if we needed 0, we will just add it)
			if(rop == Const && (ae2->val == 1 || ae2->val == 0)) return NULL;
		
			//const % x?
			if(lop == Const && (ae1->val == 1 || ae1->val == 0)) return NULL;
			
			//(a/b) % b = a
			if(lop == Div && ae1->getFather()->getSig() == ae2->getSig()){
				return NULL;
			}
		}
		if (op == Times){
			//(a/b) * c not canon
			if (lop == Div || rop == Div) return NULL;
			//a * 0 or a* 1
			if(lop == Const && (ae1->val == 1 || ae1->val == 0)) return NULL;
			if(rop == Const && (ae2->val == 1 || ae2->val == 0)) return NULL;
		}
		string msig =ae1->getSig();
		string fsig =ae2->getSig();
		if (ae1->isCommutative(op)){
			if (msig > fsig){
				return NULL;
			}
		}
		string checkSig = "(" + msig + ArithExpression::AT2str(op) + fsig + ")";
		ArithExpression* ae;
		if (ASigMap.find(checkSig) == ASigMap.end()){
			//cannot find sig
			ae = new ArithExpression(op,ae1,ae2);
			ASigMap[checkSig] = ae;
		}else{
			ae=ASigMap[checkSig];
		}
		vector<int> outputs;
		ae->getOutputs(inputs, outputs);
		Assert(inputs.size() == outputs.size(),"outputs not correctly evaluated");
		if (outputs == neededOutputs){
			return ae;
		}
		auto it =outputsAMap.find(outputs);
		if(it != outputsAMap.end()){
			//found an expression with equivalent outputs
			return NULL;
		}else{
			outputsAMap[outputs] = ae;
			ASetMap[d].insert(ae);
		}
		return NULL;
		
	}
	ArithExpression* getFirstExpression(){
		Assert(ASetMap.find(1) != ASetMap.end(), "depth = 1 should have been populated");
		return *(ASetMap[1].begin());
	}
	void clearSetMap(){
		ASetMap.clear();
		outputsAMap.clear();
	}
	ArithExpression* getExpressionForOutputs(vector<int> &outputs){
		auto it =outputsAMap.find(outputs);
		if( it != outputsAMap.end()){
			return it->second;
		}
		else{
			return NULL;
		}
	}
	ArithExpression* addToMapsD1(ArithType op, int cival, vector< vector<int> > &inputs, vector<int> & neededOutputs){
		string checkSig;
		if (op == Variable){
			checkSig = ArithExpression::AT2str(op)+to_string(cival);
		}
		else if (op == Const){
			checkSig = to_string(cival);
		}
		else{
			Assert(false, "depth 1 can only be var or const");
		}
		ArithExpression* ae;
		if (ASigMap.find(checkSig) == ASigMap.end()){
			//cannot find sig
			ae = new ArithExpression(op,cival);
			ASigMap[checkSig] = ae;
		}else{
			ae=ASigMap[checkSig];
		}
		vector<int> outputs;
		ae->getOutputs(inputs, outputs);
		Assert(inputs.size() == outputs.size(),"outputs not correctly evaluated");
		if (outputs == neededOutputs){
			return ae;
		}
		auto it =outputsAMap.find(outputs);
		if(it != outputsAMap.end()){
			//found an expression with equivalent outputs
			return NULL;
		}else{
			outputsAMap[outputs] = ae;
			ASetMap[1].insert(ae);
		}
		return NULL;
	}

	
	void getExpressionsUpto(int dm1, set < ArithExpression* >  &s){
		for(int d=1;d<=dm1;d++){
			Assert(ASetMap.find(d) != ASetMap.end(),"This depth should have been worked out already");
			
			for(ArithExpression* ae: ASetMap[d]){
				s.insert(ae);
			}
		}
	}
	ArithExpression* getExpression(int depth, vector< vector<int> > &inputs, vector<int> & neededOutputs){
		if (ASetMap.find(depth) != ASetMap.end()){
			Assert(false,"This shouldn't happen since we clear the map");
		}
		else {
			ASetMap[depth] = set <ArithExpression*>();
		}
		//cout<<"ArithExprCustomSynth: "<<accstr<<" Generating all expressions of depth: "<<depth<<endl;
		

		Assert(depth >= 1, "depth should be GTE 1");
		if (depth == 1){
			//variables and constants
			for (int i=0;i<numvars;i++){
				ArithExpression* ae =  addToMapsD1(Variable,i,inputs,neededOutputs);
				if(ae != NULL) return ae;
			}
			for(int c: consts){
				ArithExpression* ae = addToMapsD1(Const,c, inputs,neededOutputs);
				if(ae != NULL) return ae;
			}
		}
		else{
			set < ArithExpression* > aeSetdm1;
			getExpressionsUpto(depth-1,aeSetdm1);
			for(ArithExpression* ae1: aeSetdm1){
				for(ArithExpression* ae2: aeSetdm1){
					if(ae1->getDepth() == depth-1 || ae2->getDepth() == depth-1){
						for(ArithType op : ops ){
							ArithExpression* ae =  addToMaps(op, ae1, ae2, depth, inputs, neededOutputs);
							if(ae != NULL) return ae;
						}
					}
				}
			}
		}
		cout<<"ArithExprCustomSynth: Under "<<inputs.size()<<" examples. Generated expressions of depth: "<<depth<< ", Total Expressions:"<< ASetMap[depth].size()<<endl;
		return NULL;
	}
};
