#ifndef DAGFUNCTIONINLINER_H_
#define DAGFUNCTIONINLINER_H_

#include "BooleanDAG.h"
#include "DagOptim.h"
#include <cstring>
#include <sstream>
#include <utility>
#include <utility>
#include "CommandLineArgs.h"
#include "BooleanToCNF.h"
#include "Tvalue.h"
#include "NodeHardcoder.h"
#include "HoleHardcoder.h"
#include "DagFunctionToAssertion.h"


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
			if((*it)->type == bool_node::UFUN){
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


/*
This is a node in a tree data-structure corresponding to the call tree
for the program. funid is an id for the call, will either be based on the 
callsite or on the callname depending on the inlining mode.
limit can be used to override the inlining limit.
parent is the parent in the tree.
*/
class InlinerNode{
public:
	int funid;
  int limit;
	InlinerNode* parent;	
};


class TheBestInliner: public InlineControl{
protected:
	Ostore<InlinerNode> inodestore;

	//Table maps call name to a unique id. It is only used when bounding 
	//by callname.
	StringHTable2<int> funidmap;


	//This is an index from a globalID of a function node to a tree data-structure
	//that represents the call tree.
	map<int, InlinerNode*> inodes;
	//root is the root node in the call tree.
	InlinerNode* root;

	int nextfunid;
	int limit;

	
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
			if(badConditions.count(n->mother()->globalId)==0){
				recInsert(n->mother());
			}
			if(badConditions.count(n->father()->globalId)==0){
				recInsert(n->father());
			}
		}
	}

	/*
	When we call this function, we know the function in question is a good
	candidate for inlining, but we want to make sure it doesn't have bad conditions.
	*/
	bool recCheck(bool_node* n){
		if(badConditions.count(n->globalId)>0){			
			return false;
		}else{
			if(n->type == bool_node::AND){	
				return recCheck(n->mother()) && recCheck(n->father());
			}
		}
		return true;
	}

	/**
	If you decide that a call should not be inlined ant that call has 
	condition C, then any other call that has condition C should not be 
	inlined either, because C being true would surely cause an assertion, so 
	any other call that has C as a condition will not be inlined either.
	The badConditions are the globalIds of those expressions corresponding to bad conditions.
	*/
	set<int> badConditions;

	//There are two inlining modes. Inlining by callsite means the bound
	//controls the number of times a call site appears in the stack. 
	//If this is false, it means we only care about the number of times a
	//call name appears on the stack.
	bool boundByCallsite;
protected:
	virtual void addChild(InlinerNode* parent, const UFUN_node& node) {
		int id = nextfunid++;
		funidmap.condAdd(node.get_ufname().c_str(), node.get_ufname().size() + 1, id, id);
		InlinerNode* next = inodestore.newObj();
		if (boundByCallsite) {
			next->funid = node.get_callsite();
		}
		else {
			next->funid = id;
		}
		next->limit = -1;
		next->parent = parent;
		inodes[node.globalId] = next;
	}



	virtual void addChildWithLimit(InlinerNode* parent, const UFUN_node& node, int newLmt) {
		int id = nextfunid++;
		funidmap.condAdd(node.get_ufname().c_str(), node.get_ufname().size() + 1, id, id);
		InlinerNode* next = inodestore.newObj();
		if (boundByCallsite) {
			next->funid = node.get_callsite();
		}
		else {
			next->funid = id;
		}
		next->limit = newLmt;
		next->parent = parent;
		inodes[node.globalId] = next;
	}

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

	
  
  

	virtual void preCheckInline(UFUN_node& node){
		if(node.dependent()){ return; }
		map<int, InlinerNode*>::iterator it = inodes.find(node.globalId);
		if(it == inodes.end()){
			//In this case, we are visiting root, so we should add these as children to root.
			addChild(root, node);
		}
		if(!node.ignoreAsserts && !localCheck(node)){
			recInsert(node.mother());
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
		while (temp != NULL) {
			if (temp->limit != -1 && temp->limit < recLimit) {
				recLimit = temp->limit;
				if (cnt >= recLimit) {
					return false;
				}
				return true;
			}
			if (temp->funid == candidate->funid) {
				cnt++;
				if (cnt >= recLimit) {
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
		if(node.mother()->type == bool_node::CONST){
			CONST_node* cn = dynamic_cast<CONST_node*>(node.mother());
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
		if(/*!node.ignoreAsserts && */ !recCheck(node.mother())){
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

		if(node.mother()->type == bool_node::CONST){
			int val = dynamic_cast<CONST_node*>(node.mother())->getVal();
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
			if(badConditions.count(n->mother()->globalId)==0){
				recInsert(n->mother());
			}
			if(badConditions.count(n->father()->globalId)==0){
				recInsert(n->father());
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
				return recCheck(n->mother()) && recCheck(n->father());
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
			recInsert(node.mother());		
			return rv;
		}else{
			return recCheck(node.mother());
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
		return (inlineAllFuns) ||  funsToInline.count(node.get_ufname())!=0;
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

class DagFunctionInliner : public virtual DagOptim
{
	
	bool symbolicSolve;
	
	BooleanDAG& dag;
protected:
	map<string, BooleanDAG*>& functionMap;
private:
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
	const set<string>& pureFunctions;
public:
	int nfuns(){ return lnfuns; }
	DagFunctionInliner(
	        BooleanDAG& p_dag, map<string, BooleanDAG*>& p_functionMap, map<string, map<string, string> > p_replaceMap,
	        FloatManager& fm,	HoleHardcoder* p_hcoder, const set<string>& p_pureFunctions, bool p_randomize=false, InlineControl* ict=NULL,
	        bool p_onlySpRandomize=false, int p_spRandBias = 1);
	virtual ~DagFunctionInliner();
	virtual void process(BooleanDAG& bdag);
	bool process_and_return(BooleanDAG& bdag);
		
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

class DagOneStepInlineAndConcretize : public DagFunctionInliner, public NodeHardcoder
{
public:
    DagOneStepInlineAndConcretize(
            VarStore& _ctrl_store,
            bool_node::Type tp,
            BooleanDAG& p_dag,
            map<string, BooleanDAG*>& p_functionMap,
            map<string, map<string, string> > p_replaceMap,
            FloatManager& fm,
            HoleHardcoder* p_hcoder,
            const set<string>& p_pureFunctions,
            bool p_randomize=false,
            InlineControl* ict=NULL,
            bool p_onlySpRandomize=false,
            int p_spRandBias = 1):
            DagFunctionInliner(
                    p_dag, p_functionMap, std::move(p_replaceMap),
                    fm, p_hcoder, p_pureFunctions, p_randomize,
                    ict, p_onlySpRandomize, p_spRandBias),
            NodeHardcoder(PARAMS->showInputs, p_dag, _ctrl_store, tp, fm),
            DagOptim(p_dag, fm)
            {}

    void visit(CTRL_node& node) override;
    void visit(SRC_node& node) override;
    void visit(UFUN_node &node) override;

};

/**
 * Thought Flow:
 * 1. Need to use DagOneStepInlineAndConcretize to concretize dags with a counterexample from checker on the go
 * 1.1 Benefits: if you have a conterexample, concretizing while inling should reduce memory consumption due to constant propagation and optimization.
 * 1.2 Currently the entire inlined dag is being optimized after concretization.
 *
 * Actionable:
 * Instead of inlining before doing CEGIS. Inline whenever you get a new counterexample using the DagOneStepInlineAndConcretize.
 *
 * Qs:
 * DagOneStepInlineAndConcretize accepts as input all the meta-data of a sketch (look at the spec for initializing DagOneStepInlineAndConcretize)
 * Q: should all these parameters be passed to the checker?
 * Q: should we rather package them as an inliner, and send the inliner as a parameter, and then initialize the DagOneStepInlineAndConcretize with the inliner?
 * Inline seems to be have two nested outer loops around inliner (the for and the do-while loops).
 * Q: should we run these outer loops also when we do the inlining on the go?
 *  How would that work?
 *  How should we structure the code in relation to DagConcretier?
 *  Would DagOneStepInlineAndConcretize be instead of the DagInliner in these outer loops or should the outer loops be inside the DagOneStepInlineAndConcretize and we have an one-step-concretizer-inliner as a parameter in the DagOneStepInlineAndConcretize rather than having it inherit from DagInliner.
 */



void findPureFuns(map<string, BooleanDAG*>& functionMap, set<string>& pureFuns);

class ProgramEnvironment
{
public:
    CommandLineArgs& params;
    FloatManager& floats;
    HoleHardcoder& hardcoder;
    map<string, BooleanDAG*>& functionMap;
    int num_inlining_steps;
    map<string, map<string, string> > replaceMap;

    ProgramEnvironment(CommandLineArgs& _params, FloatManager& _floats, HoleHardcoder& _hardcoder,
                       map<string, BooleanDAG*>& _functionMap, int _steps, map<string, map<string, string> >& _replaceMap):
                       params(_params), floats(_floats), hardcoder(_hardcoder), replaceMap(std::move(_replaceMap)),
                       functionMap(_functionMap), num_inlining_steps(_steps)
   {

   }

   void toggle_pcond(BooleanDAG* dag, bool val)
   {
       vector<bool_node*> ctrls = dag->getNodesByType(bool_node::CTRL);
       for(auto bn : ctrls)
       {
           if(((CTRL_node*)bn)->get_Pcond())
           {
               if (val) {
                   ((CTRL_node *) bn)->activate_pcond();
               } else {
                   ((CTRL_node *) bn)->deactivate_pcond(); }
           }
       }
   }

   void activate_pcond(BooleanDAG* dag)
   {
       toggle_pcond(dag, true);
   }
   void deactivate_pcond(BooleanDAG* dag)
   {
       toggle_pcond(dag, false);
   }

   void doInline(
           BooleanDAG& dag, VarStore& var_store, bool_node::Type var_type, bool do_deactivate_pcond = false){

        if(do_deactivate_pcond){
            deactivate_pcond(&dag);
        }

        //OneCallPerCSiteInliner fin;
        // InlineControl* fin = new OneCallPerCSiteInliner(); //new BoundedCountInliner(PARAMS->boundedCount);
        TheBestInliner fin(num_inlining_steps, params.boundmode == CommandLineArgs::CALLSITE);
        /*
        if(PARAMS->boundedCount > 0){
        fin = new BoundedCountInliner(PARAMS->boundedCount);
        }else{
        fin = new OneCallPerCSiteInliner();
        }
        */

        set<string> pureFuns;

        findPureFuns(functionMap, pureFuns);

        DagOneStepInlineAndConcretize dfi(
                var_store,
                var_type,
                dag,
                functionMap,
                std::move(replaceMap),
                floats,
                &hardcoder,
                pureFuns,
                params.randomassign,
                &fin,
                params.onlySpRandAssign,
                params.spRandBias);


        int oldSize = -1;
        bool nofuns = false;
        for (int i = 0; i<num_inlining_steps; ++i) {
            int t = 0;
            int ct = 0;
            do {
                if (params.randomassign && params.onlySpRandAssign) {
                    if (ct < 2) {
                        dfi.turnOffRandomization();
                        ct++;
                    } else {
                        dfi.turnOnRandomization();
                    }
                }

                try {
                    dfi.process(dag);
                }
                catch (BadConcretization) {
                    assert(dfi.get_failedAssert() != nullptr);
                    dag.set_failed_assert(dfi.get_failedAssert());

                    if(do_deactivate_pcond){
                        activate_pcond(&dag);
                    }
                    return ;
                }
                //
                // dag.repOK();
                set<string>& dones = dfi.getFunsInlined();
                if (params.verbosity> 6) { cout << "inlined " << dfi.nfuns() << " new size =" << dag.size() << endl; }
                //dag.lprint(cout);
                if (params.bndDAG > 0 && dag.size() > params.bndDAG) {
                    cout << "WARNING: Preemptively stopping CEGIS because the graph size exceeds the limit: " << params.bndDAG << endl;
                    exit(1);
                }
                if (oldSize > 0) {
                    if(dag.size() > 400000000 && dag.size() > oldSize * 10){
                        i = num_inlining_steps;
                        cout << "WARNING: Preemptively stopping inlining because the graph was growing too big too fast" << endl;
                        break;
                    }
                    if((dag.size() > 400000 && dag.size() > oldSize * 2)|| dag.size() > 1000000){
                        hardcoder.tryHarder();
                    }
                }
                oldSize = dag.size();
                ++t;
            } while (dfi.changed());
            if (params.verbosity> 6) { cout << "END OF STEP " << i << endl; }
            // fin.ctt.printCtree(cout, dag);

            fin.clear();
            if (t == 1 && params.verbosity> 6) { cout << "Bailing out" << endl; break; }
        }
        hardcoder.afterInline();
        {
            DagFunctionToAssertion makeAssert(dag, functionMap, floats);
            makeAssert.process(dag);
        }


       if(do_deactivate_pcond){
           activate_pcond(&dag);
       }

    }

    FloatManager &get_floats() {
        return floats;
    }

    HoleHardcoder &get_hardcoder()
    {
        return hardcoder;
    }
};

class Harness
{
    //unrolled dag is the the original unrolled dag using prepare miter at before calling assertDAG
    BooleanDAG* original_dag;
    //root dag is
    // IF NOT CONCRETIZED: the root harness dag
    // IF CONCRETIZED: a fully unrolled concretized dag using the doInline from env.
    BooleanDAG* root_dag;

    //if env == nullptr => original_dag and root_dag ARE NOT concretized
    //if env != nullptr => original_dag and root_dag ARE concretized
    ProgramEnvironment* env;

    bool new_way = true;
    bool keep_track_of_original = false;
public:
    Harness(
            BooleanDAG* _dag_root,
            BooleanDAG* _original_dag = nullptr,
            ProgramEnvironment* _evn = nullptr):
            root_dag(_dag_root), original_dag(_original_dag), env(_evn){
        if(new_way)
        {
            if(!keep_track_of_original)
            {
                original_dag = nullptr;
            }
        }
        else
        {
            if(_original_dag != nullptr)
            {
                root_dag = original_dag;
                original_dag = nullptr;
            }
        }
    }

    Harness* produce_inlined_dag(bool deactivate_pcond = false)
    {
        VarStore var_store;
        bool_node::Type var_type = bool_node::CTRL;
        return produce_concretization(var_store, var_type, deactivate_pcond, true);
    }

    Harness* do_inline(bool deactivate_pcond = false)
    {
        VarStore var_store;
        bool_node::Type var_type = bool_node::CTRL;
        return produce_concretization(var_store, var_type, deactivate_pcond, false);
    }


    Harness* concretize(VarStore& var_store, bool_node::Type var_type, bool deactivate_pcond = false)
    {
        return produce_concretization(var_store, var_type, deactivate_pcond, false);
    }

    Harness* produce_concretization(VarStore& var_store, bool_node::Type var_type, bool do_deactivate_pcond = false, bool do_clone = true)
    {
        if(new_way)
        {
            if(do_clone)
            {
                BooleanDAG* concretized_root_dag = root_dag->clone();
                env->doInline(*concretized_root_dag, var_store, var_type, do_deactivate_pcond);
                return new Harness(concretized_root_dag, nullptr, env);
            }
            else
            {
                env->doInline(*root_dag, var_store, var_type, do_deactivate_pcond);
                return this;
            }
        }
        else
        {
            assert(!do_deactivate_pcond);
            BooleanDAG* concretized_unrolled_dag;
            concretized_unrolled_dag = hardCodeINode(root_dag, var_store, var_type, env->get_floats());

            if(do_clone)
            {
                return new Harness(concretized_unrolled_dag, nullptr, env);
            }
            else
            {
                root_dag->clear();
                root_dag = concretized_unrolled_dag;
                return this;
            }
        }
    }
//
    Harness *clone() {
        if(original_dag != nullptr)
        {
            return new Harness(root_dag->clone(), original_dag->clone(), env);
        }
        else
        {
            return new Harness(root_dag->clone(), nullptr, env);
        }
    }
//WHAT ABOUT IMPLEMENTS?
    BooleanDAG *get_dag() {
        return root_dag;
    }

    void clear()
    {
        if(original_dag != nullptr)
        {
            original_dag->clear();
            delete original_dag;
            original_dag = NULL;
        }
        root_dag->clear();
        delete root_dag;
        root_dag = NULL;
    }

private:
    VarStore* ctrl_var_store__solution;
public:
    VarStore* get_ctrl_var_store()
    {
        return ctrl_var_store__solution;
    }
    void set_solution(VarStore* _ctrl_var_store)
    {
        ctrl_var_store__solution = _ctrl_var_store;
    }

private:
    string name;
public:

    void set_name(const string _name) {
        name = _name;
    }
    string get_name()
    {
        return name;
    }
};


#endif /*DAGFUNCTIONINLINER_H_*/
