#ifndef ARITHMETICEXPRESSIONBUILDER_H_
#define ARITHMETICEXPRESSIONBUILDER_H_

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



enum ArithType {Variable, Plus, Times, Minus, Div, Mod, Lt, Gt, And, Or, Const, Xor, Eq, Not, Neg};
//Xor, Eq, Not , Neg, //LATER:ArrAcc, ArrAss
#define SWAPPER
//#define PRINTDEBUG
class ArithExprSyn;
class ArithExprBuilder;
//using boost::hash_combine
template <typename T>
inline void hash_combine(size_t& seed, T const& v)
{
	seed ^= hash<T>()(v) + 0x9e3779b9 + (seed << 6) + (seed >> 2);
}

template <typename T>
struct hashVec
{
	size_t operator()(vector<T> const& in) const
	{
		size_t size = in.size();
		size_t seed = 0;
		for (size_t i = 0; i < size; i++)
			//Combine the hash of the current vector with the hashes of the previous ones
			hash_combine(seed, in[i]);
		return seed;
	}
};

class ArithExpression {
	
	ArithExpression* mother;
	ArithExpression* father;
	
	
	bool getOutputBit() {
		switch (type) {
			case Const: {
				if (val == 0 || val == 1) {
					return true;
				}
				return false;
			}
			case Variable: {
				Assert(false, "cannot be called here with a Variable");
			}
			case Neg:
			case Plus:
			case Times:
			case Minus:
			case Div:
			case Mod: {
				return false;
			}
			case Not:
			case Eq:
			case Lt:
			case Gt:
			case Xor:
			case And:
			case Or: {
				return true;
			}
		}
		Assert(false, "Control shouldn't reach here");
	}
	
public:
#ifdef SWAPPER
	set<ArithExpression*> dagOps; //keeps the set of all
	//Unique ArithNodes in its parents and includes itself
	//Doesn't count leaf (var/const) nodes
	//This is used to implement a DagNode Cuttoff
	//By rejecting any expressions with dagOps.size > cutoff
	//And, replacing a larger dagOps.size expression with
	//a smaller one whenever possible in observational equivalence
	//algorithm
#endif
	int depth;
	string name;
	bool repeated;
	bool isBoolean;
	int val;
	ArithType type;
	bool isDead;
	static const int Undef = -4798773;
	static bool isUnary(ArithType at) {
		return (at == Neg || at == Not);
	}
	static string AT2str(ArithType at){
		switch(at){
			case Variable: return "V";
			case Const: return "C";
			case Plus: return "+";
			case Times: return "*";
			case Minus: return "-";
			case Div: return "/";
			case Mod: return "%";
			case Lt: return "<";
			case Gt: return ">";
			case And: return "&&";
			case Or: return "||";
			case Xor: return "^";
			case Eq: return "==";
			case Not: return "!";
			case Neg: return "-";
				//case ArrAcc: return "ite";
				//case ArrAss: return "itec";
			default: Assert(false, "ArithType not supported here.");
		}
	}
	static string ATset2str(set <ArithType> atSet){
		string s = "";
		for (auto at: atSet ){
			s += AT2str(at);
		}
		return s;
	}
	
	static bool isCommutative(ArithType at){
		return (at == Plus || at == Times || at == And || at == Or || at == Eq || at == Xor);
	}
	ArithExpression(ArithType at,int varid, bool isBool){
		Assert(at == Variable || at == Const,"Should be Variable or Const");
		type = at;
		val = varid;
		mother = NULL;
		father = NULL;
		name = getSig();
		depth=1;
		repeated = false;
#ifdef SWAPPER
		if(at==Const) dagOps.insert(this); //since this is a Const
#endif
		if (at == Const) this->isBoolean = getOutputBit();
		else this->isBoolean = isBool; //need to supply this for variables
		isDead = false; // for marking this node unusable for the iteration, need to reset it after ecery synthesis call
	}
	ArithExpression(ArithType at, ArithExpression* m, ArithExpression* f, bool checkRepeat = false){
		type = at;
		mother = m;
		
		father = f;
		name = getSig();
		val = -333;
#ifdef SWAPPER
		dagOps.insert(this); //since this is an op and not Var/Const
		dagOps.insert(m->dagOps.begin(), m->dagOps.end());
#endif
		if (f != NULL) {//for unary nodes Not, Neg
			depth = max(m->getDepth(), f->getDepth()) + 1;
#ifdef SWAPPER
			dagOps.insert(f->dagOps.begin(), f->dagOps.end());
#endif
		}
		else {
			depth = m->getDepth() + 1;
			
		}
		if (checkRepeat) {
			set<int> vars;
			repeated = checkRepeated(vars);
		} else {
			repeated = false;
		}
		isDead = false; // for marking this node unusable for the iteration, need to reset it after ecery synthesis call
		isBoolean = getOutputBit();
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
	bool_node* getDag(DagOptim* dopt, const vector<bool_node*>& params,const vector<bool> &isBit) {
		bool_node* mnode=NULL;
		bool_node* fnode=NULL;
		if (mother != NULL){
			mnode = mother->getDag(dopt,params,isBit);
		}
		if (father != NULL){
			fnode = father->getDag(dopt,params,isBit);
		}
		
		bool_node* rnode = NULL;
		switch(type){
			case Const: {
				return dopt->getCnode(val);
			}
			case Variable: {
				return params[val];
			}
			case Neg: {
				rnode = new NEG_node();
				rnode->mother = mnode;
				rnode->addToParents();
				rnode = dopt->optAdd(rnode);
				return rnode;
			}
			case Not: {
				rnode = new NOT_node();
				Assert(mother->isBoolean, "node should output a bit ");
				rnode->mother = mnode;
				rnode->addToParents();
				rnode = dopt->optAdd(rnode);
				return rnode;
			}
			case Eq: {
				rnode = new EQ_node(); break;
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
			case Lt: {
				rnode = new LT_node(); break;
			}
			case Gt: {// a> b ==> b < a;
				rnode = mnode;
				mnode = fnode;
				fnode = rnode;
				rnode = new LT_node(); break;
			}
			case Xor: {
				Assert(mother->isBoolean, "m node should output a bit ");
				Assert(father->isBoolean, "f node should output a bit ");
				rnode = new XOR_node();
				break;
			}
			case And: {
				Assert(mother->isBoolean, "m node should output a bit ");
				Assert(father->isBoolean, "f node should output a bit ");
				rnode = new AND_node();
				break;
			}
			case Or: {
				Assert(mother->isBoolean, "m node should output a bit ");
				Assert(father->isBoolean, "f node should output a bit ");
				rnode = new OR_node();
				break;
			}
			default: Assert(false, "");
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
		if (father == NULL) {
			//unary node
			return "( "  + AT2str(type) + " " + msig + " )";
		}
		return "( " + msig + " " + AT2str(type) + " " + fsig + " )";
	}
	
	bool getOutputs(const vector< vector<int> > &inputs, vector<int> &outputs){
		outputs.clear();
		for(auto input: inputs){
			int ev = this->evaluate(input);
			if (ev == Undef) {
				return false;
			}
			outputs.push_back(ev);
		}
		return true;
	}
	
	int eval(int mval, int fval, ArithType type) {
		if (mval == Undef || (fval == Undef && (type != Neg && type != Not) )) return Undef;
		switch (type) {
			case Plus: return mval + fval;
			case Times: return mval * fval;
			case Minus: return mval - fval;
			case Div: {
				if (fval == 0) {
					return Undef;
				}
				else return mval / fval;
			}
			case Mod: {
				if (fval <= 0) {
					return Undef;
				}
				else return mval % fval;
			}
			case Gt: return mval > fval;
			case Lt: return mval < fval;
			case And: {
				if (mval != 0 && mval != 1) return Undef;
				else if (fval != 0 && fval != 1) return Undef;
				else return (mval == 1) && (fval == 1);
			}
			case Or: {
				if (mval != 0 && mval != 1) return Undef;
				else if (fval != 0 && fval != 1) return Undef;
				else return (mval == 1) || (fval == 1);
			}
			case Xor: {
				if (mval != 0 && mval != 1) return Undef;
				else if (fval != 0 && fval != 1) return Undef;
				else return (mval == 1) ^ (fval == 1);
			}
			case Eq: return (mval == fval);
			case Neg: return (-mval);
			case Not: {
				if (mval != 0 && mval != 1) return Undef;
				else return (mval != 1);
			}
			default: Assert(false, "should have an eval def");
		}
	}
	
	bool getOutputs(const unordered_map<ArithExpression*, vector<int>>& outsmap, vector<int> &outputs) {
		Assert(mother != NULL, "Mother should not be null");
		//Assert(father != NULL, "Father should not be null");
		auto mit = outsmap.find(mother);
		Assert(mit != outsmap.end(), "Mother should already be evaluated");
		const vector<int>& mvals = mit->second;
		outputs.clear();
		if (type != Neg && type != Not) {
			auto fit = outsmap.find(father);
			Assert(fit != outsmap.end(), "Father should already be evaluated: " + getSig() + " isDead: " << to_string(father->isDead));
			const vector<int>& fvals = fit->second;
			Assert(type != Const && type != Variable, "Type should not be const or variable");
			Assert(mvals.size() == fvals.size(), "Something is wrong");
			
			for (int i = 0; i < mvals.size(); i++) {
				int ev = eval(mvals[i], fvals[i], type);
				if (ev == Undef) {
					return false;
				}
				outputs.push_back(ev);
			}
		}
		else {
			for (int i = 0; i < mvals.size(); i++) {
				int ev = eval(mvals[i], Undef, type);
				if (ev == Undef) {
					return false;
				}
				outputs.push_back(ev);
			}
		}
		return true;
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
		
		if (father == NULL) {
			return  "(" + AT2str(type) + msig + ")";
		}
		return "(" + msig + AT2str(type) + fsig + ")";
	}
	bool checkRepeated(set<int>& vars) {
		if (type == Const){
			return false;
		}
		else if (type == Variable){
			if (vars.find(val) != vars.end()) {
				return true;
			}
			vars.insert(val);
			return false;
		}
		
		if (mother != NULL){
			if (mother->checkRepeated(vars)) {
				return true;
			}
		}
		if (father != NULL){
			if (father->checkRepeated(vars)) {
				return true;
			}
		}
		return false;
	}
	
	int evaluate(vector<int> &values) {
		int mval = -333;
		int fval = -333;
		if (mother != NULL) {
			mval = mother->evaluate(values);
			if (mval == Undef) {
				return Undef;
			}
		}
		if (father != NULL) {
			fval = father->evaluate(values);
			if (fval == Undef) {
				return Undef;
			}
		}
		
		switch (type) {
			case Const: return this->val;
			case Variable: return values[this->val];
			case Plus: return mval + fval;
			case Times: return mval * fval;
			case Minus: return mval - fval;
			case Div: {
				if (fval == 0) {
					return Undef;
				}
				else return mval / fval;
			}
			case Mod: {
				if (fval <= 0) {
					return Undef;
				}
				else return mval % fval;
			}
			case Lt: return mval < fval;
			case Gt: return mval > fval;
			case And: {
				if (mval != 0 && mval != 1) return Undef;
				else if (fval != 0 && fval != 1) return Undef;
				else return (mval == 1) && (fval == 1);
			}
			case Or: {
				if (mval != 0 && mval != 1) return Undef;
				else if (fval != 0 && fval != 1) return Undef;
				else return (mval == 1) || (fval == 1);
			}
			case Xor: {
				if (mval != 0 && mval != 1) return Undef;
				else if (fval != 0 && fval != 1) return Undef;
				else return (mval == 1) ^ (fval == 1);
			}
			case Eq: return (mval == fval);
			case Neg: return (-mval);
			case Not: {
				if (mval != 0 && mval != 1) return Undef;
				else return (mval != 1);
			}
			default: {
				Assert(false, "Control shouldn't reach here")
			}
		}
	}
};

#ifdef SWAPPER
struct AE_compare {
	bool operator() (const ArithExpression* lhs, const ArithExpression* rhs) const {
		//int ldsz = lhs->dagOps.size();
		//int rdsz = rhs->dagOps.size();
		
		//if (ldsz < rdsz) return true;
		//else if (rdsz < ldsz) return false;
		//else {
		int ld = lhs->depth;
		int rd = rhs->depth;
		if (ld < rd) return true;
		else if (rd < ld) return false;
		else return lhs->name < rhs->name;
		//}
	}
};
#endif

//Arith Expr Builder - should resuse as many nodes (vars, consts, ops etc)
//based on signatures of nodes. Maintaing a map sig -> ArithExpression*
//Building in a layered manner - make all depth 1. Then 2 and then 3 and so on.
//Take two candidates from previous level; take an Op. Make potentially two expressions
// (one if op is commutative)
// for V variables, C constants, K ops. Level 1 = V+C exprs, Level 2=  (L1^C_2)*2 (U-bound)
// Level3 = O((V+C)^4), Level4 = O((V+C)^8) ...

class ArithExprBuilder {
	//For a given level of depth - a set of expressions
	map < int, set < ArithExpression* > >  ASetMap;
	unordered_map<ArithExpression*, vector<int>> exprOutMap;
	
	int numvars;
	vector<bool> &isBit;
	bool outIsBit;
	set<int>& consts;
	vector <ArithType>& ops;
	int maxDepth;
	unordered_map < vector< int >, ArithExpression*, hashVec<int> > outputsABigMap;
	unordered_map< vector<int>, vector<vector<int>>, hashVec<int> > outputsASmallMap;
	bool repeatVars;
	ArithExprSyn* syn;
	set<string> desiredSigs;
public:
	//A static map of signature to Expressions to avoid recomputation
	static map < string, ArithExpression *> ASigMap;
	static void clearStaticMapMemory(){
		//cout << "Deleting " << ASigMap.size() << " nodes." << endl;
		for(auto& kv: ASigMap){
			//cout << "Deleting AE node: "<< kv.first << endl;
			delete kv.second; // all expressions ever generated & not deleted immediately should be here in the map
		}
	}
	ArithExprBuilder(int numvars, set<int> &consts, vector <ArithType> &ops, int mDepth, bool repeat, vector<bool> & isBit, bool outIsBit, ArithExprSyn* _syn): consts(consts), ops(ops), repeatVars(repeat), isBit(isBit), syn(_syn) {
		//string opstr = ArithExpression::ATset2str(ops);
		this->numvars = numvars;
		//string constsStr = "";
		//for (int c: consts){
		//	constsStr  += to_string(c) + "c";
		//}
		//string accstr = to_string(numvars)+ "v" + constsStr + opstr;
		maxDepth = mDepth;
		//cout<<"ArithExprBuilder Built for: "<<accstr<<endl;
		this->outIsBit = outIsBit;
	}
	ArithExpression* addToMaps(ArithType op, ArithExpression* ae1, ArithExpression* ae2, int d, const vector< vector<int> > &inputs, const vector<int> & neededOutputs, const vector<int>& exampleIds, const vector<bool>& setOutputs);
	
	ArithExpression* getFirstExpression() {
		Assert(ASetMap.find(1) != ASetMap.end(), "depth = 1 should have been populated");
		return *(ASetMap[1].begin());
	}
	void clearSetMap() {
		ASetMap.clear();
		outputsABigMap.clear();
		outputsASmallMap.clear();
		exprOutMap.clear(); //TODO: we can clear map immediately after getExpressions method
	}
	ArithExpression* getExpressionFromSmallMap(const vector<int>& outputs, const vector<int>& exampleIds, const vector<bool>& notUnsetOutputs);
	
	ArithExpression* generateSmallMap(const vector<int>& needOutputs, const vector<int>& exampleIds, const vector<bool>& setOutputs, const vector<bool>& notUnsetOutputs);
	
	ArithExpression* addToMapsD1(ArithType op, int cival, const vector< vector<int> > &inputs, const vector<int> & neededOutputs, const vector<int>& exampleIds, const vector<bool>& setOutputs);
	
	
	
#ifdef SWAPPER
	int getExpressionsByDagSize(int depthm1, map<int, set < ArithExpression*, AE_compare > > &m) {
		int sm = 0;
		for (int d = 1; d <= depthm1; d++) {
			Assert(ASetMap.find(d) != ASetMap.end(), "This depth should have been worked out already");
			
			for (ArithExpression* ae : ASetMap[d]) {
				int dsz = ae->dagOps.size();
				if (m.find(dsz) == m.end()) {
					if (dsz > sm) sm = dsz;
				}
				m[dsz].insert(ae);
			}
		}
		return sm;
	}
	void getExpressionsUpto(int dm1, set < ArithExpression*, AE_compare > &s) {
#else
		void getExpressionsUpto(int dm1, set < ArithExpression* >  &s){
#endif
			for(int d=1;d<=dm1;d++){
				Assert(ASetMap.find(d) != ASetMap.end(),"This depth should have been worked out already");
				
				for(ArithExpression* ae: ASetMap[d]){
					s.insert(ae);
				}
			}
		}
		static string getIOString(vector<int> &input, int output){
			string s="";
			for(auto& inp:input){
				s+="|"+to_string(inp);
			}
			s+="|->"+to_string(output);
			return s;
		}
		static string getIOStrings(vector< vector<int> > &inputs, vector<int> &outputs){
			string s="";
			for (int io=0;io<inputs.size();io++){
				s+=" ## " + getIOString(inputs[io],outputs[io]);
			}
			return s;
		}
		//#define STRICTTYPING
		//Allowing bits at only leaf levels
		ArithExpression* addUnaryOps(ArithExpression* ae1, int depth, const vector< vector<int> > &inputs, const vector<int> & neededOutputs, const vector<int>& exampleIds, const vector<bool>& setOutputs) {
			if (ae1->getDepth() == depth - 1  ) {
				bool llAndOutbit = (depth == maxDepth) && outIsBit;
				ArithExpression* ae;
				
				if (!llAndOutbit && !ae1->isDead ) {
#ifdef STRICTTYPING
					bool isIntOrVar = !ae1->isBoolean || ae1->type == Variable || ae1->type == Const;
#else
					bool isIntOrVar = true;
#endif
					if (isIntOrVar) {
						ae = addToMaps(Neg, ae1, NULL, depth, inputs, neededOutputs, exampleIds, setOutputs);
						if (ae != NULL) return ae;
					}
				}
				if (ae1->isBoolean && !ae1->isDead) {
					ae = addToMaps(Not, ae1, NULL, depth, inputs, neededOutputs, exampleIds, setOutputs);
					if (ae != NULL) return ae;
				}
			}
			return NULL;
		}
		
		ArithExpression* addBinaryOps(ArithExpression* ae1, ArithExpression* ae2, int depth, const vector< vector<int> > &inputs, const vector<int> & neededOutputs, const vector<int>& exampleIds, const vector<bool>& setOutputs) {
			
			if ((ae1->getDepth() == depth - 1 || ae2->getDepth() == depth - 1)) {
				//isDead marker is used to denote that this expression has been discarded in favor of another "better" equivalent expression
				//Do not use this expression for now, and reset the marker for future iterations
				for (ArithType op : ops) {
					//if (!ArithExpression::isUnary(op))
					if (ae1->isDead || ae2->isDead) break;
#ifdef STRICTTYPING
					bool isIntOrVar1 = !ae1->isBoolean || ae1->type == Variable || ae1->type == Const;
					bool isIntOrVar2 = !ae2->isBoolean || ae2->type == Variable || ae2->type == Const;
#else
					bool isIntOrVar1 = true;
					bool isIntOrVar2 = true;
#endif
					bool skipgen =false;
					bool llAndOutbit = (depth == maxDepth) && outIsBit;
					switch (op) {
						case Neg: {
							skipgen = true; break; //Unary op
						}
						case Plus:
						case Times:
						case Minus: {
							
							skipgen = (ae1->isBoolean && ae1->isBoolean) || llAndOutbit;
#ifdef STRICTTYPING
							skipgen = skipgen || !isIntOrVar1 || !isIntOrVar2;
#endif
							break;
							//Boolean operators can cover these when both operands are bits
							//TODO: May be unsound!
						}
						case Div:
						case Mod: {
							skipgen = (ae1->isBoolean || ae2->isBoolean) || llAndOutbit;
#ifdef STRICTTYPING
							skipgen = skipgen || !isIntOrVar1 || !isIntOrVar2;
#endif
							break;
							//not interesting expressions, maybe we are losing some expressions
							//but in most cases these values can be represented with other functions
							//e.g. int i % bit b = if(b)then 0 else Undef
							//bit b % int i = if(i==1) then 0 else if (i> 1) then b else Undef
							//bit b / int i = if (==0) then undef else if(!b) then 0 else if (i==1) then 1 else 0
							//int i / bit b = if(b) then i else Undef
						}
						case Not: {
							skipgen = true; break; //Unary op
						}
						case Eq:
						case Gt:
						case Lt: {
							skipgen = ae1->isBoolean && ae2->isBoolean;
#ifdef STRICTTYPING
							skipgen = skipgen || !isIntOrVar1 || !isIntOrVar2;
#endif
							break;
							//Boolean operators can cover these when both operands are bits
						}
						case Xor:
						case And:
						case Or: {
							skipgen = !ae1->isBoolean || !ae2->isBoolean; break;
							//both operands need to be bits here!
						}
					}
					if (!skipgen) {
						ArithExpression * ae = addToMaps(op, ae1, ae2, depth, inputs, neededOutputs, exampleIds, setOutputs);
						if (ae != NULL) return ae;
						
					}
				}
			}
			return NULL;
		}
		
		ArithExpression* getExpression(int depth, const vector< vector<int> > &inputs, const vector<int> & neededOutputs, const vector<int>& exampleIds, const vector<bool>& setOutputs) {
			if (ASetMap.find(depth) != ASetMap.end()) {
				Assert(false, "This shouldn't happen since we clear the map");
			}
			else {
				ASetMap[depth] = set <ArithExpression*>();
			}
			//cout<<"ArithExprCustomSynth: "<<accstr<<" Generating all expressions of depth: "<<depth<<endl;
			
			
			Assert(depth >= 1, "depth should be GTE 1");
			if (depth == 1) {
				//variables and constants
				for (int i = 0; i < numvars; i++) {
					ArithExpression* ae = addToMapsD1(Variable, i, inputs, neededOutputs, exampleIds, setOutputs);
					if (ae != NULL) return ae;
				}
				for (int c : consts) {
					ArithExpression* ae = addToMapsD1(Const, c, inputs, neededOutputs, exampleIds, setOutputs);
					if (ae != NULL) return ae;
				}
			}
			else {
#ifdef SWAPPER
				//set < ArithExpression*,AE_compare > aeSetdm1;
				map<int, set < ArithExpression*, AE_compare > > dagMap;
				int maxSize = getExpressionsByDagSize(depth - 1, dagMap);
				vector< pair<ArithExpression*, ArithExpression*> > order;
				
				for (int sum = 0; sum <= 2*maxSize;  sum++) {
					if (dagMap.find(sum - 1) != dagMap.end()) {
						for (auto ae1 : dagMap[sum - 1]) {
							ArithExpression* ae =  addUnaryOps(ae1, depth, inputs, neededOutputs, exampleIds, setOutputs);
							if (ae != NULL) return ae;
						}
					}
					for (int diff = sum%2; diff <= maxSize; diff=diff+2) {
						int i = (sum + diff) / 2;
						int j = sum - i;
						//do (i,j) (j,i) unless (i==j)
						
						for (auto ae1 : dagMap[i]) {
							for (auto ae2 : dagMap[j]) {
								ArithExpression* ae = addBinaryOps(ae1, ae2, depth, inputs, neededOutputs, exampleIds, setOutputs);
								if (ae != NULL) return ae;
								if (i != j) {
									ArithExpression* ae = addBinaryOps(ae2, ae1, depth, inputs, neededOutputs, exampleIds, setOutputs);
									if (ae != NULL) return ae;
								}
							}
						}
					}
				}
				for (auto& kv: dagMap) {
					for (auto ae : kv.second) {
						if (ae->isDead) {
							ae->isDead = false;
						}
					}
				}
#else
				set < ArithExpression* > aeSetdm1;
				getExpressionsUpto(depth - 1, aeSetdm1);
				for (ArithExpression* ae1 : aeSetdm1) {
					ArithExpression* ae = addUnaryOps(ae1, depth, inputs, neededOutputs, exampleIds, setOutputs);
					if (ae != NULL) return ae;
					for (ArithExpression* ae2 : aeSetdm1) {
						ArithExpression* ae = addBinaryOps(ae1,ae2, depth, inputs, neededOutputs, exampleIds, setOutputs);
						if (ae != NULL) return ae;
					}
				}
				//reset the isDead marker, this node might be useful later
				for (ArithExpression* ae : aeSetdm1) {
					if (ae->isDead) {
						ae->isDead = false;
					}
				}
#endif
			}
			
			//cout<<"ArithExprCustomSynth: Under "<<inputs.size()<<" examples. Generated expressions of depth: "<<depth<< ", Total Expressions:"<< ASetMap[depth].size()<<endl;
			//cout<<getIOStrings(inputs,neededOutputs)<<endl;
			return NULL;
		}
		
  bool checkUnsetOutputs(const vector<int>& exampleIds, const vector<bool>& setOutputs, const vector<int>& outputs);
	};
	
#endif
