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

namespace SwapperPredicateNS {
enum NodeType {Bit, Negbit, True, Lt, Lte, Gt, Gte, Eq, Neq, And, Or, Ltc, Ltec, Gtc, Gtec, Eqc, Neqc};
const set<NodeType> basebinops = { Lt, Lte, Gt, Gte, Eq, Neq, Ltc, Ltec, Gtc, Gtec, Eqc, Neqc };
const set<NodeType> basebinopsC = { Ltc, Ltec, Gtc, Gtec, Eqc, Neqc };
const set<NodeType> basebinopsNC = { Lt, Lte, Gt, Gte, Eq, Neq };
const set<NodeType> recbinops = {And, Or };

class SwapperPredicate;


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

const int Undef = -457253;

class SwapperPredicate {
	NodeType type;
	SwapperPredicate* mother;
	SwapperPredicate* father;
	int i1;
	int i2;
	int depth; //For now, just 1 or 2
	string sig;
public:
	static int numvars;
	static string NT2str(NodeType nt){
		switch(nt){
			case Bit: return "B";
			case Negbit: return "!B";
			case True : return "T";
			case Ltc:
			case Lt: return "<";
			case Ltec:
			case Lte: return "<=";
			case Gtc:
			case Gt: return ">";
			case Gtec:
			case Gte: return ">=";
			case Eqc:
			case Eq: return "==";
			case Neqc:
			case Neq: return "!=";
		}
		Assert(false,"Control can't reach here")
	}

	bool isCommutative(NodeType nt){
		return (nt == And || nt == Or || nt == Eq || nt == Neq);
	}
	SwapperPredicate(NodeType nt) {
		Assert(nt == True, "Should be True");
		type = nt;
		i1 = Undef;
		mother = NULL;
		father = NULL;
		i2 = Undef;
		depth = 1;
		sig = getSig();
	}
	SwapperPredicate(NodeType nt,int bid){
		Assert(nt == Bit || nt == Negbit ,"Should be Bit");
		type = nt;
		i1 = bid;
		mother = NULL;
		father = NULL;
		i2 = Undef;
		depth=1;
		sig = getSig();
	}
	SwapperPredicate(NodeType nt, int id1,int id2) {
		Assert(nt == Lt || nt == Lte || nt == Gt || nt == Gte || nt == Eq || nt == Neq || nt == Ltc || nt == Ltec || nt == Gtc || nt == Gtec || nt == Eqc || nt == Neqc, "Should be base level comparators");
		type = nt;
		if ((type == Eq || type == Neq) && id1 > id2){
			int tmp = id1;
			id1 = id2;
			id2 = tmp;
		}
		i1 = id1;
		i2 = id2;
		depth = 1;
		mother = NULL;
		father = NULL;
		sig = getSig();
	}
	SwapperPredicate(NodeType nt, SwapperPredicate* m, SwapperPredicate* f){
		Assert(nt == And || nt == Or, "Should be And or Or");
		type = nt;
		mother = m;
		father = f;
		depth = max(m->getDepth(),f->getDepth())+1;
		i1 = Undef;
		i2 = Undef;
		sig = getSig();
	}
	int getDepth(){
		return depth;
	}
	SwapperPredicate* getMother(){
		return mother;
	}
	SwapperPredicate* getFather(){
		return father;
	}
	NodeType getOp(){
		return type;
	}
	bool_node* getDag(DagOptim* dopt, const vector<bool_node*>& params) {
		Assert(params.size() == numvars, "invalid number of params");
		bool_node* mnode = NULL;
		bool_node* fnode = NULL;
		if (mother != NULL){
			mnode = mother->getDag(dopt,params);
		}
		if (father != NULL){
			fnode = father->getDag(dopt,params);
		}

		bool_node* rnode;
		bool notneeded = false;
		bool_node* notMom = NULL;
		switch(type){
			case True: {
				return dopt->getCnode(true);
			}
			case Bit: {
				Assert(params[i1]->getOtype() == OutType::BOOL, "should be a bit node at this index");
				return params[i1];
			}
			case Negbit: {
				rnode = new NOT_node(); 
				Assert(params[i1]->getOtype() == OutType::BOOL, "should be a bit node at this index");
				rnode->mother = params[i1];
				rnode->addToParents();
				rnode = dopt->optAdd(rnode);
				return rnode;
			}
			case And: {
				rnode = new AND_node(); break;
				
			}
			case Or: {
				rnode = new OR_node(); break;
				
			}
			case Ltc:
			case Lt: {
				rnode = new LT_node();
				Assert(params[i1]->getOtype() == OutType::BOOL || params[i1]->getOtype() == OutType::INT, "should be an int or bit node at this index");
				mnode = params[i1];
				Assert((type == Ltc) || params[i2]->getOtype() == OutType::BOOL || params[i2]->getOtype() == OutType::INT, "should be an int or bit node at this index");
				fnode = (type == Lt) ? params[i2] : dopt->getCnode(i2); break;
			}
			case Gtc:
			case Gt: {
				rnode = new LT_node();
				Assert((type == Gtc) || params[i2]->getOtype() == OutType::BOOL || params[i2]->getOtype() == OutType::INT, "should be an int or bit node at this index");
				mnode = (type == Gt) ? params[i2] : dopt->getCnode(i2);
				Assert(params[i1]->getOtype() == OutType::BOOL || params[i1]->getOtype() == OutType::INT, "should be an int or bit node at this index");
				fnode = params[i1]; break;
			}
			case Eqc:
			case Eq: {
				rnode = new EQ_node();
				Assert(params[i1]->getOtype() == OutType::BOOL || params[i1]->getOtype() == OutType::INT, "should be an int or bit node at this index");
				mnode = params[i1];
				Assert((type == Eqc) || params[i2]->getOtype() == OutType::BOOL || params[i2]->getOtype() == OutType::INT, "should be an int or bit node at this index");
				fnode = (type == Eq) ? params[i2] : dopt->getCnode(i2); break;
			}
			case Neqc:
			case Neq: { // a != b  == ! (a == b)
				notneeded = true;
				notMom = new EQ_node();
				Assert(params[i1]->getOtype() == OutType::BOOL || params[i1]->getOtype() == OutType::INT, "should be an int or bit node at this index");
				mnode = params[i1];
				Assert((type == Neqc) || params[i2]->getOtype() == OutType::BOOL || params[i2]->getOtype() == OutType::INT, "should be an int or bit node at this index");
				fnode = (type == Neq) ? params[i2] : dopt->getCnode(i2);
				break;
			}
			case Ltec:
			case Lte: { // a <= b  == ! (b<a)
				notneeded = true;
				notMom = new LT_node();
				Assert((type == Ltec) || params[i2]->getOtype() == OutType::BOOL || params[i2]->getOtype() == OutType::INT, "should be an int or bit node at this index");
				mnode = (type == Lte) ? params[i2] : dopt->getCnode(i2);
				Assert(params[i1]->getOtype() == OutType::BOOL || params[i1]->getOtype() == OutType::INT, "should be an int or bit node at this index");
				fnode = params[i1];
				break;
			}
			case Gtec:
			case Gte: { // a >= b  == ! (a<b)
				notneeded = true;
				notMom = new LT_node();
				Assert(params[i1]->getOtype() == OutType::BOOL || params[i1]->getOtype() == OutType::INT, "should be an int or bit node at this index");
				mnode = params[i1];
				Assert((type == Gtec) || params[i2]->getOtype() == OutType::BOOL || params[i2]->getOtype() == OutType::INT, "should be an int or bit node at this index");
				fnode = (type == Gte) ? params[i2] : dopt->getCnode(i2);
				break;
			}
		}
		if (!notneeded) {
			rnode->mother = mnode;
			rnode->father = fnode;
		}
		else {
			notMom->mother = mnode;
			notMom->father = fnode;
			notMom->addToParents();
			notMom = dopt->optAdd(notMom);
			rnode = new NOT_node();
			rnode->mother = notMom;
		}
		rnode->addToParents();
		rnode = dopt->optAdd(rnode);
		return rnode;
	}
	string getFEOut(){
		if (type == Bit){
			return "( IN_" + to_string(i1) + " )";
		}
		else if (type == Negbit){
			return "( ! IN_" + to_string(i1) +" )";
		}
		else if (type == True) {
			return "( 1 )";
		}
		else if (type == And) {
			return "( " + mother->getFEOut() + " && " + father->getFEOut() + " )";
		}
		else if (type == Or) {
			return "( " + mother->getFEOut() + " || " + father->getFEOut() + " )";
		}
		else if (type == Lt || type == Lte || type == Gt || type == Gte || type == Eq || type == Neq) {
			return "( IN_" + to_string(i1) + " " + NT2str(type) + " IN_" +to_string(i2) + " )" ;
		}
		else if (type == Ltc || type == Ltec || type == Gtc || type == Gtec || type == Eqc || type == Neqc) {
			return "( IN_" + to_string(i1) + " " + NT2str(type) + " " + to_string(i2) + " )";
		}
		Assert(false, "Control can't come here");
	}
	
	void getOutputs(vector< vector<int> > &inputs, vector<bool> &outputs){
		outputs.clear();
		for(auto input: inputs){
			outputs.push_back(this->evaluate(input));
		}
	}
	static string buildPSig(NodeType &type,const int &i1,const int & i2, SwapperPredicate* mother, SwapperPredicate* father) {
		if (type == Bit) {
			return "b" + to_string(i1);
		}
		else if (type == Negbit) {
			return "!b" + to_string(i1);
		}
		else if (type == True) {
			return "T";
		}
		else if (type == And) {
			return "(" + mother->getSig() + "&" + father->getSig() + ")";
		}
		else if (type == Or) {
			return "(" + mother->getSig() + "|" + father->getSig() + ")";
		}
		else if (type == Lt || type == Lte || type == Gt || type == Gte || type == Eq || type == Neq) {
			return "i" + to_string(i1) + SwapperPredicate::NT2str(type) + "i" + to_string(i2);
		}
		else if (type == Ltc || type == Ltec || type == Gtc || type == Gtec || type == Eqc || type == Neqc) {
			return "i" + to_string(i1) + SwapperPredicate::NT2str(type) + "c" + to_string(i2);
		}
		Assert(false, "Control can't come here");
	}
	string getSig() {
		if (this->sig.size() > 0) return this->sig;
		sig =  buildPSig(type, i1, i2, mother, father);
		return sig;
	}
	bool evaluate(const vector<int> &values){
		bool mval = false;
		bool fval = false;
		if (type == And || type == Or){
			Assert(mother != NULL && father != NULL, "Null parents for And and Or not allowed");
			mval = mother->evaluate(values);
			fval = father->evaluate(values);
		}
		
		switch (type){
			case Bit: {
				Assert(values[i1] == 0 || values[i1] == 1, "Only bit valyes allowed");
				return (values[i1] == 1);
			}
			case Negbit: {
				Assert(values[i1] == 0 || values[i1] == 1, "Only bit valyes allowed");
				return (values[i1] == 0);
			}
			case True: return true;
			case And: return mval && fval;
			case Or: return mval || fval;
			case Lt: return values[i1] < values[i2];
			case Lte: return values[i1] <= values[i2];
			case Gt: return values[i1] > values[i2];
			case Gte: return values[i1] >= values[i2];
			case Eq: return values[i1] == values[i2];
			case Neq: return values[i1] != values[i2];
			case Ltc: return values[i1] < i2;
			case Ltec: return values[i1] <= i2;
			case Gtc: return values[i1] > i2;
			case Gtec: return values[i1] >= i2;
			case Eqc: return values[i1] == i2;
			case Neqc: return values[i1] != i2;
		}
		Assert(false, "Control cannot reach here");
	}
	//TODO: Optimize this to reuse values from lower depth instead of recursively calling here for mother and father
};

typedef unordered_map<vector< bool >, SwapperPredicate*, hash< vector< bool > > > PMapType;

class PredicateBuilder {
	//For a given level of depth - a set of expressions
	
	map < int, set < SwapperPredicate* > >  PSetMap;
	//A static map of signature to Expressions to avoid recomputation
	static unordered_map < string, SwapperPredicate *> PSigMap; //TODO: define in cpp file
	vector<int> &bits;
	vector<int> &intsOrbits;
	set<int> &consts;
	//set <NodeType> &ops;
	int maxDepth;
	unordered_map< vector< bool >, SwapperPredicate*, hashVec<bool> > outputsPMap;
	unordered_map<SwapperPredicate*, vector<bool> > evalMap;
public:
	
	static void clearStaticMapMemory(){
		for(auto& kv: PSigMap){
			delete kv.second; // all expressions ever generated & not deleted immediately should be here in the map
		}
	}
	PredicateBuilder(vector<int> & bits, vector<int> & intsOrbits, set<int> &consts, int mDepth): bits(bits), intsOrbits(intsOrbits), consts(consts) {
		maxDepth = mDepth;
		SwapperPredicate::numvars = intsOrbits.size();
	}
	SwapperPredicate* addToMaps(NodeType op, SwapperPredicate* m, SwapperPredicate* f, const int &i1, const int &i2, int d, vector< vector<int> > &inputs, vector<bool> & neededOutputs){
		
		
		string checkSig = SwapperPredicate::buildPSig(op, i1, i2, m, f);
		//cout<<"Adding expression: "<<checkSig<<endl;
		SwapperPredicate* sp;
		if (PSigMap.find(checkSig) == PSigMap.end()){
			//cannot find sig
			if (op == True)
				sp = new SwapperPredicate(op);
			else if (op == And || op == Or) {
				sp = new SwapperPredicate(op, m, f);
			}
			else if (op == Bit || op == Negbit) {
				sp = new SwapperPredicate(op, i1);
			}
			else {
				sp = new SwapperPredicate(op, i1, i2);
			}
			PSigMap[checkSig] = sp;
		}else{
			sp=PSigMap[checkSig];
		}
		vector<bool> outputs;
		sp->getOutputs(inputs, outputs);
		Assert(inputs.size() == outputs.size(),"outputs not correctly evaluated");
		if (outputs == neededOutputs){
			PSetMap[d].insert(sp);
			return sp;
		}
		auto it =outputsPMap.find(outputs);
		if(it != outputsPMap.end()){
			//found an expression with equivalent outputs
			//cout<<"Output based pruning: "<<checkSig<<" : "<<outputsAMap[outputs]->getSig()<<endl;
			return NULL;
		}else{
			outputsPMap[outputs] = sp;
			evalMap[sp] = outputs;
			PSetMap[d].insert(sp);
		}
		return NULL;
		
	}
	SwapperPredicate* getFirstExpression(){
		Assert(PSetMap.find(1) != PSetMap.end(), "depth = 1 should have been populated");
		return *(PSetMap[1].begin());
	}
	void clearSetMap(){
		PSetMap.clear();
		outputsPMap.clear();
		evalMap.clear();
	}
	SwapperPredicate* getExpressionForOutputs(vector<bool> &outputs){
		auto it =outputsPMap.find(outputs);
		if( it != outputsPMap.end()){
			return it->second;
		}
		else{
			return NULL;
		}
	}
	
	void getExpressionsUpto(int dm1, set < SwapperPredicate* >  &s){
		for(int d=1;d<=dm1;d++){
			Assert(PSetMap.find(d) != PSetMap.end(),"This depth should have been worked out already");
			
			for(SwapperPredicate* sp: PSetMap[d]){
				s.insert(sp);
			}
		}
	}
	static string getIOString(vector<int> &input, bool output){
		string s="";
		for(auto& inp:input){
			s+="|"+to_string(inp);
		}
		s+="|->"+to_string(output);
		return s;
	}
	static string getIOStrings(vector< vector<int> > &inputs, vector<bool> &outputs){
		string s="";
		for (int io=0;io<inputs.size();io++){
			s+=" ## " + getIOString(inputs[io],outputs[io]);
		}
		return s;
	}
	SwapperPredicate* getExpression(int depth, vector< vector<int> > &inputs, vector<bool> & neededOutputs){
		if (PSetMap.find(depth) != PSetMap.end()){
			Assert(false,"This shouldn't happen since we clear the map");
		}
		else {
			PSetMap[depth] = set <SwapperPredicate*>();
		}
		//cout<<"ArithExprCustomSynth: "<<accstr<<" Generating all expressions of depth: "<<depth<<endl;
		

		Assert(depth >= 1, "depth should be GTE 1");
		if (depth == 1){
			//true pred
			SwapperPredicate* st = addToMaps(True, NULL, NULL, Undef, Undef,1,inputs,neededOutputs);
			if (st != NULL) return st;
			//bits and int ops
			for (int i1 : bits) {
				//indices of bits
				SwapperPredicate* sp = addToMaps(Bit, NULL, NULL, i1, Undef, 1, inputs, neededOutputs);
				if (sp != NULL) return sp;

				sp = addToMaps(Negbit, NULL, NULL, i1, Undef, 1, inputs, neededOutputs);
				if (sp != NULL) return sp;
			}
			for (int i1 : intsOrbits) {
				for (int i2 : intsOrbits) {
					for (NodeType op : basebinopsNC){
						if (i1 != i2 && (find(bits.begin(),bits.end(),i1) == bits.end() || find(bits.begin(), bits.end(), i2) == bits.end()) ) {
							SwapperPredicate* sp = addToMaps(op, NULL, NULL, i1, i2, 1, inputs, neededOutputs);
							if (sp != NULL) return sp;
						}
					}
				}
				if (find(bits.begin(), bits.end(), i1) == bits.end()) {
					for (int i2 : consts) {
						for (NodeType op : basebinopsC) {
							SwapperPredicate* sp = addToMaps(op, NULL, NULL, i1, i2, 1, inputs, neededOutputs);
							if (sp != NULL) return sp;
						}
					}
				}
			}
		}
		else{
			
			set < SwapperPredicate* > aeSetdm1;
			getExpressionsUpto(depth-1,aeSetdm1);
			for(SwapperPredicate* ae1: aeSetdm1){
				for(SwapperPredicate* ae2: aeSetdm1){
					//"sig < sig" only valid because both And and Or are commutative
					if((ae1->getDepth() == depth-1 || ae2->getDepth() == depth-1 ) && (ae1->getSig() < ae2->getSig())){
						for (NodeType op :recbinops) {
							if (maxDepth == 2) {//this is the last level, we don't have to generate the node unless we need to return it 
								vector<bool> &om = evalMap[ae1];
								vector<bool> &of = evalMap[ae2];
								bool ignoreEx = false;
								for (int i = 0; i < neededOutputs.size(); i++) {
									if (op == And && ((om[i] && of[i]) != neededOutputs[i])) {
										ignoreEx = true;
										break;
									}
									else if (op == Or && ((om[i] || of[i]) != neededOutputs[i])) {
										ignoreEx = true;
										break;
									}
								}
								if (ignoreEx) continue;
								else {
									SwapperPredicate* ae = addToMaps(op, ae1, ae2, Undef, Undef, depth, inputs, neededOutputs);
									Assert(ae != NULL, "by construction control comes here iff all outputs are same as the ones needed");
									return ae;
								}
							}

							SwapperPredicate* ae =  addToMaps(op, ae1, ae2,Undef, Undef, depth, inputs, neededOutputs);
							if(ae != NULL) return ae;
						}
					}
				}
			}
		}
		//cout<<"ArithExprCustomSynth: Under "<<inputs.size()<<" examples. Generated expressions of depth: "<<depth<< ", Total Expressions:"<< ASetMap[depth].size()<<endl;
		//cout<<getIOStrings(inputs,neededOutputs)<<endl;
		return NULL;
	}
};
}
