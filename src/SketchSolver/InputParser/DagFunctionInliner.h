#ifndef DAGFUNCTIONINLINER_H_
#define DAGFUNCTIONINLINER_H_

#include "BooleanDAG.h"
#include "DagOptim.h"
#include <cstring>
#include <sstream>
#include "CommandLineArgs.h"
#include "BooleanToCNF.h"
#include "Tvalue.h"
#include "HoleHardcoder.h"


class Caller{
public:
	int callergid;
	int callercid;
	Caller():callergid(-1),callercid(-1){}
	Caller(int gid, int cid):callergid(gid), callercid(cid){}
};

class CallTreeTracker{

	/**
	Call tree. vector of calls by a given caller.
	Indexed by the globalID of the call node.
	*/
	map<int, vector<int> > ctree;

	/**
		
	*/
	map<int, Caller > rctree;

	/**
	Record the name of a function indexed by a call id.
	*/
	map<int, string> cname;

	string funName(const UFUN_node& fun){
		string s = fun.get_ufname().substr(0, 9);
		s += "_";
		char tmpbo[256];
		//itoa(fun.get_callsite(), tmpbo, 10);
		//s += tmpbo;
		//s += "_";
		//itoa(fun.globalId, tmpbo, 10);
                sprintf(tmpbo, "%d", fun.globalId);
		s += tmpbo;		
		return s;
	}

	void regName(const UFUN_node& fun){
		
		cname[fun.globalId] = funName(fun);
	}
	
public:
	CallTreeTracker(){}
	void clear(){

		ctree.clear();
		rctree.clear();
		cname.clear();
	}
	bool seenCall(int callgid){
		return rctree.count(callgid)>0;
	}

	Caller& getParentCallsite(int callgid){
		return rctree[callgid];
	}

	void registerCall(const UFUN_node& caller, const UFUN_node* callee){
		ctree[caller.globalId].push_back(callee->globalId);
		rctree[callee->globalId] = Caller(caller.globalId, caller.get_callsite());
		regName(caller);
		regName(*callee);
	}
	void printCtree(ostream& out, BooleanDAG& dag){
		out<<"digraph G {"<<endl;
		for(map<int, vector<int> >::iterator it = ctree.begin(); it != ctree.end(); ++it){
			for(int i=0; i<it->second.size(); ++i){
				cout<<cname[it->first]<<" -> "<<cname[it->second[i]]<<endl;
			}
		}
		for(BooleanDAG::iterator it = dag.begin(); it != dag.end(); ++it){
			if(typeid(**it) == typeid(UFUN_node)){
				if(cname.count((*it)->globalId)>0){
				cout<<cname[(*it)->globalId]<<"[shape=record]"<<endl;
				}else{
					// UFUN_node* un = dynamic_cast<UFUN_node*>(*it);
					// cout<<"couldn't find "<<un->get_ufname()<<" "<<un->globalId<<"  "<<un->get_callsite()<<endl;
				}
			}
		}
		out<<"}"<<endl;
	}
};


class InlineControl{
	
public:

	/**
	Tell the inline controller that you are about to inline the function represented by node.
	*/
	virtual void registerInline(UFUN_node& node)=0;

	/**
	Call this method to check if you want to inline a method or not.
	*/
	virtual bool checkInline(UFUN_node& node)=0;

	/**
	Call this method is called before starting to inline, just to let the inliner know what's coming.
	*/
	virtual void preCheckInline(UFUN_node& node)=0;
  
  virtual bool isRecursive(UFUN_node& node) { return false; }

	virtual void clear()=0;
  
  virtual int numRecursive(UFUN_node& node) {return 0;}
  
  virtual void registerCallWithLimit(const UFUN_node& caller, const UFUN_node* callee, int newLmt) {}

	/**
	Register the existence of a call.
	*/
	virtual void registerCall(const UFUN_node& caller, const UFUN_node* callee)=0;
  
  virtual int getLimit() {return 0;}
};


class InlinerNode{
public:
	int funid;
  int limit;
	InlinerNode* parent;	
};


class TheBestInliner: public InlineControl{
protected:
	Ostore<InlinerNode> inodestore;
	StringHTable2<int> funidmap;
	map<int, InlinerNode*> inodes;
	int nextfunid;
	int limit;

	InlinerNode* root;
	InlinerNode* current;
	int currentFun;

		/**
	When we call this function, we know we don't want to inline the call, 
	but we want to register its condition as a bad condition so that 
	other calls with the same condition don't get inlined.
	*/
	void recInsert(bool_node* n){
		badConditions.insert(n->globalId);
		if(n->type == bool_node::OR){
			if(badConditions.count(n->mother->globalId)==0){
				recInsert(n->mother);
			}
			if(badConditions.count(n->father->globalId)==0){
				recInsert(n->father);
			}
		}
	}

	/*
	When we call this function, we know the function in question is a good
	candidate for inlining, but we want to make sure it doesn't have bad conditions.
	*/
	bool recCheck(bool_node* n){
		if(badConditions.count(n->globalId)>0){
			//cout<<"Saved an inline"<<endl;
			return false;
		}else{
			if(n->type == bool_node::AND){	
				return recCheck(n->mother) && recCheck(n->father);
			}
		}
		return true;
	}

	/**
	If you decide that a call should not be inlined ant that call has 
	condition C, then any other call that has condition C should not be 
	inlined either, because C being true would surely cause an assertion, so 
	any other call that has C as a condition will not be inlined either.
	*/
	set<int> badConditions;
	bool boundByCallsite;
public:
	TheBestInliner(int& p_limit, bool p_boundByCallsite){
		boundByCallsite = p_boundByCallsite;
		limit = p_limit;
		p_limit = 1;
		nextfunid = 0;
		funidmap.condAdd("$root", 6, nextfunid, nextfunid);
		nextfunid++;
		root= inodestore.newObj();
    root->limit = -1;
		root->funid = 0;
		root->parent = NULL;
		
	}

	virtual void addChild(InlinerNode* parent, const UFUN_node& node){
		int id = nextfunid++;
		funidmap.condAdd(node.get_ufname().c_str(), node.get_ufname().size()+1, id, id);
		InlinerNode* next = inodestore.newObj();
		if(boundByCallsite){
			next->funid = node.get_callsite();
		}else{
			next->funid = id;
		}
    next->limit = -1;
		next->parent = parent;
		inodes[node.globalId] = next;
	}
  
  virtual void addChildWithLimit(InlinerNode* parent, const UFUN_node& node, int newLmt){
		int id = nextfunid++;
		funidmap.condAdd(node.get_ufname().c_str(), node.get_ufname().size()+1, id, id);
		InlinerNode* next = inodestore.newObj();
		if(boundByCallsite){
			next->funid = node.get_callsite();
		}else{
			next->funid = id;
		}
    next->limit = newLmt;
		next->parent = parent;
		inodes[node.globalId] = next;
	}

	virtual void preCheckInline(UFUN_node& node){
		if(node.dependent()){ return; }
		map<int, InlinerNode*>::iterator it = inodes.find(node.globalId);
		if(it == inodes.end()){
			//In this case, we are visiting root, so we should add these as children to root.
			addChild(root, node);
		}
		if(!node.ignoreAsserts && !localCheck(node)){
			recInsert(node.mother);
		}
	}

	virtual void registerInline(UFUN_node& node){
		current = inodes[node.globalId];
		Assert(current != NULL, "I've never seen this function before!!!");
	}

	virtual void registerCall(const UFUN_node& caller, const UFUN_node* callee){
    Assert(current == inodes[caller.globalId], "This should never happen");
		addChild(current, *callee);
	}
  
  virtual void registerCallWithLimit(const UFUN_node& caller, const UFUN_node* callee, int newLmt) {
    Assert(current == inodes[caller.globalId], "This should never happen");
    addChildWithLimit(current, *callee, newLmt);
  }

  bool isRecursive(UFUN_node& node) {
    InlinerNode* candidate = inodes[node.globalId];
		Assert(candidate != NULL, "I've never seen this function before!!!");
    InlinerNode* temp = candidate->parent;
   
    if (temp->funid == candidate->funid) {
      return true;
    } else {
      return false;
    }
  }
  
  int numRecursive(UFUN_node& node) {
    InlinerNode* candidate = inodes[node.globalId];
		Assert(candidate != NULL, "I've never seen this function before!!!");
		int cnt = 0;
		InlinerNode* temp = candidate->parent;
    while(temp != NULL){
      
			if(temp->funid == candidate->funid){
				cnt++;
			}
			temp = temp->parent;
		}
		return cnt;
  }
  
	bool localCheck(UFUN_node& node){
		InlinerNode* candidate = inodes[node.globalId];
		Assert(candidate != NULL, "I've never seen this function before!!!");
		int cnt = 0;
		InlinerNode* temp = candidate->parent;
    int recLimit = limit;
		while(temp != NULL){
      if (temp->limit != -1 && temp->limit < recLimit) {
        recLimit = temp->limit;
        if (cnt >= recLimit) {
          return false;
        }
        return true;
      }
			if(temp->funid == candidate->funid){
				cnt++;
				if(cnt >= recLimit){
					// cout<<"Prevented "<<node.get_ufname()<<endl;
					return false;
				}
			}
			temp = temp->parent;
		}
		//cout<<"Inlining with  "<<cnt<<" steps "<<node.get_ufname()<<endl;
		return true;
	}

	virtual bool checkInline(UFUN_node& node){	
		if(node.dependent()){ return true; }
		if(node.mother->type == bool_node::CONST){
			CONST_node* cn = dynamic_cast<CONST_node*>(node.mother);
			int val = cn->getVal();
			if(val == 0){
				return false;
			}else{
				//if(!node.ignoreAsserts){
					// cout<<"Inlining with true cond "<<node.get_ufname()<<endl;
					return true;
				//}
			}
		}
		if(/*!node.ignoreAsserts && */ !recCheck(node.mother)){
			return false;
		}
		return localCheck(node);
	}
  virtual int getLimit() {return limit; }
	virtual void clear(){ }
};





class BoundedCountInliner: public InlineControl{
	int inlinebound;
	map<string, int> counts;
public:
	BoundedCountInliner(int count): inlinebound(count){
		
	}

	/**
	Tell the inline controller that you are about to inline the function represented by node.
	*/
	void registerInline(UFUN_node& node){}

	virtual void preCheckInline(UFUN_node& node){
	
	}


	/**
	Call this method to check if you want to inline a method or not.
	*/
	bool checkInline(UFUN_node& node){
		if(node.dependent()){
			return true;
		}

		if(node.mother->type == bool_node::CONST){
			int val = dynamic_cast<CONST_node*>(node.mother)->getVal();
			if(val==1){
				return true;
			}else{
				return false;
			}
		}

		const string& s = node.get_ufname();
		map<string, int>::iterator it = counts.find(s);
		if(it != counts.end()){			
			if(it->second < inlinebound){				
				it->second++;
				return true;
			}else{
				return false;
			}
		}
		counts[s] = 1;
		return true;
	}

	/**
	Register the existence of a call.
	*/
	void registerCall(const UFUN_node& caller, const UFUN_node* callee){}
	void clear(){}
	void superclear(){ 
		counts.clear();
	}
};





class OneCallPerCSiteInliner: public InlineControl{

	/*Return true if I am supposed to inline the node.*/
	bool checkInlineHelper(UFUN_node& node){
		int tmp = node.globalId;
		while(ctt.seenCall(tmp)){
			Caller& c = ctt.getParentCallsite(tmp);
			if(c.callercid == node.get_callsite()){
				return false;
			}
			tmp = c.callergid;
		}
		return true;
	}
	/**
	When we call this function, we know we don't want to inline the call, 
	but we want to register its condition as a bad condition so that 
	other calls with the same condition don't get inlined.
	*/
	void recInsert(bool_node* n){
		badConditions.insert(n->globalId);
		if(n->type == bool_node::OR){
			if(badConditions.count(n->mother->globalId)==0){
				recInsert(n->mother);
			}
			if(badConditions.count(n->father->globalId)==0){
				recInsert(n->father);
			}
		}
	}

	/*
	When we call this function, we know the function in question is a good
	candidate for inlining, but we want to make sure it doesn't have bad conditions.
	*/
	bool recCheck(bool_node* n){
		if(badConditions.count(n->globalId)>0){
			//cout<<"Saved an inline"<<endl;
			return false;
		}else{
			if(n->type == bool_node::AND){	
				return recCheck(n->mother) && recCheck(n->father);
			}
		}
		return true;
	}

	/**
	If you decide that a call should not be inlined ant that call has 
	condition C, then any other call that has condition C should not be 
	inlined either, because C being true would surely cause an assertion, so 
	any other call that has C as a condition will not be inlined either.
	*/
	set<int> badConditions;
public:
	CallTreeTracker ctt;
	virtual void registerInline(UFUN_node& node){

	}

	virtual void preCheckInline(UFUN_node& node){
		checkInline(node);
	}

	virtual bool checkInline(UFUN_node& node){
		if(node.dependent()){ return true; } // dependent calls are not really calls and should be ignored.		
		
		const bool rv = checkInlineHelper(node);
		if(!rv){ 
			recInsert(node.mother);		
			return rv;
		}else{
			return recCheck(node.mother);
		}

	}
	virtual void registerCall(const UFUN_node& caller, const UFUN_node* callee){
		if(callee->dependent()){ return; } // dependent calls are not really calls and should be ignored.
		ctt.registerCall(caller, callee);
	}
	void clear(){
		ctt.clear();
		badConditions.clear();
	}
};

class ExclusiveInliner : public InlineControl{
	set<string> funsToNotInline;
	virtual void registerInline(UFUN_node& node){}
	virtual bool checkInline(UFUN_node& node){
		return funsToNotInline.count(node.get_ufname())==0;
	}
	virtual void registerCall(const UFUN_node& caller, const UFUN_node* callee){
		
	}
	void remFunToInline(const string& name){
		funsToNotInline.insert(name);
	}

	void clearRemList(){
		funsToNotInline.clear();
	}

	template<typename T>
	void remFunsToInline(T beg, T end){
		funsToNotInline.insert(beg, end);
	}
	void clear(){}
};





class HybridInliner : public InlineControl{
	OneCallPerCSiteInliner ocs;
	BoundedCountInliner bci;
public:

	HybridInliner(int count): bci(count){

	}

	/**
	Tell the inline controller that you are about to inline the function represented by node.
	*/
	void registerInline(UFUN_node& node){
		ocs.registerInline(node);
		bci.registerInline(node);
	}

	/**
	Call this method to check if you want to inline a method or not.
	*/
	virtual bool checkInline(UFUN_node& node){
		bool t1 = ocs.checkInline(node);
		bool t2 = bci.checkInline(node);
		/*if(!node.dependent()){
			cout<<" node="<<node.get_ufname()<<" t1="<<(t1?"yes":"no")<<"  t2="<<(t2?"yes":"no")<<endl;
		}*/		
		return t1 && t2;
	}

	/**
	Call this method is called before starting to inline, just to let the inliner know what's coming.
	*/
	virtual void preCheckInline(UFUN_node& node){
		ocs.preCheckInline(node);
		bci.preCheckInline(node);
	}

	virtual void clear(){
		ocs.clear();
		bci.superclear();
	}

	/**
	Register the existence of a call.
	*/
	virtual void registerCall(const UFUN_node& caller, const UFUN_node* callee){
		ocs.registerCall(caller, callee);

	}
};




class InclusiveInliner : public InlineControl{
	bool inlineAllFuns;
	set<string> funsToInline;
public:
	InclusiveInliner():inlineAllFuns(true){}
	virtual void registerInline(UFUN_node& node){}
	virtual void preCheckInline(UFUN_node& node){
		
	}
	virtual bool checkInline(UFUN_node& node){
		return (inlineAllFuns) ||  !funsToInline.count(node.get_ufname())==0;
	}
	virtual void registerCall(const UFUN_node& caller, const UFUN_node* callee){
		
	}
	virtual void addFunToInline(const string& name){
		inlineAllFuns = false;
		funsToInline.insert(name);
	}

	template<typename T>
	void addFunsToInline(T beg, T end){
		inlineAllFuns = false;
		funsToInline.insert(beg, end);
	}
	void clear(){}
};

class DagFunctionInliner : public DagOptim
{
	
	
	BooleanDAG& dag;
	map<string, BooleanDAG*>& functionMap;
  map<string, map<string, string> > replaceMap;
  int replaceDepth; // TODO: in general we may need a map for this

	timerclass ufunAll;

	vector<bool_node*> clones;

	map<string, bool_node*> seenControls;
	map<int, map<string, bool_node*> > mpcontroller;
	int uidcount;
			
	void optAndAdd(bool_node* n, vector<const bool_node*>& nmap);
	bool_node* optAndAddNoMap(bool_node* nnode);

	set<string> funsInlined;

	bool somethingChanged;
	int lnfuns;
	InlineControl* ictrl;

	
	bool randomize;
  bool onlySpRandomize;
  int spRandBias;
	HoleHardcoder* hcoder;
public:	
	int nfuns(){ return lnfuns; }
	DagFunctionInliner(BooleanDAG& p_dag, map<string, BooleanDAG*>& p_functionMap, map<string, map<string, string> > p_replaceMap, 	HoleHardcoder* p_hcoder, bool p_randomize=false, InlineControl* ict=NULL, bool p_onlySpRandomize=false, int p_spRandBias = 1);
	virtual ~DagFunctionInliner();
	virtual void process(BooleanDAG& bdag);
		
	virtual void visit( UFUN_node& node );
	virtual void visit(CTRL_node& node);
	virtual bool changed(){ return somethingChanged; }
	
  bool_node* createTupleAngelicNode(string tuple_name, string node_name, int depth);
  bool_node* createEqNode(bool_node* left, bool_node* right, int depth);


	set<string>& getFunsInlined(){
		return funsInlined;
	}
  
  void turnOffRandomization() {
    randomize = false;
  }

  void turnOnRandomization() {
    randomize = true;
  }
	
	
};


#endif /*DAGFUNCTIONINLINER_H_*/
