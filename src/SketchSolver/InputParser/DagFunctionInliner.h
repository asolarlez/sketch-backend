#ifndef DAGFUNCTIONINLINER_H_
#define DAGFUNCTIONINLINER_H_

#include "BooleanDAG.h"
#include "DagOptim.h"
#include <cstring>
#include <sstream>

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
	Register the existence of a call.
	*/
	virtual void registerCall(const UFUN_node& caller, const UFUN_node* callee)=0;
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
};


class InclusiveInliner : public InlineControl{
	bool inlineAllFuns;
	set<string> funsToInline;
public:
	InclusiveInliner():inlineAllFuns(true){}
	virtual void registerInline(UFUN_node& node){}
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
};


class DagFunctionInliner : public DagOptim
{
	
	
	BooleanDAG& dag;
	map<string, BooleanDAG*>& functionMap;	

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
public:	
	int nfuns(){ return lnfuns; }
	DagFunctionInliner(BooleanDAG& p_dag, map<string, BooleanDAG*>& p_functionMap, InlineControl* ict=NULL);
	virtual ~DagFunctionInliner();
	virtual void process(BooleanDAG& bdag);
		
	virtual void visit( UFUN_node& node );
	virtual bool changed(){ return somethingChanged; }
	



	set<string>& getFunsInlined(){
		return funsInlined;
	}

	
	
};


#endif /*DAGFUNCTIONINLINER_H_*/
