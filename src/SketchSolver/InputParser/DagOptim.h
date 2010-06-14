#ifndef DAGOPTIM_H_
#define DAGOPTIM_H_

#include "BooleanDAG.h"
#include "DagCSE.h"
#include "NodeStore.h"
#include <typeinfo>
#include <set>
#include <stack>

using namespace std;

class AbstractNodeValue{		
	typedef enum { BOTTOM, LIST, RANGE, TOP } State;
	State state;
	set<int> valSet;
	int low;
	int high;	
public:
	int timestamp;
	bool isList(){ return state == LIST; }
	bool isTop(){ return state==TOP; }
	bool isRange(){ return state == RANGE; }
	bool isBottom(){ return state==BOTTOM; }
	set<int>::const_iterator vset_begin() const{
		return valSet.begin();
	}
	bool contains(int i){
		return valSet.count(i)>0;
	}
	set<int>::const_iterator vset_end()const{
		return valSet.end();
	}
	int getHigh(){
		Assert(state!= BOTTOM && state != TOP, "No bottom or top states");
		return high;
	}

	int getLow(){
		Assert(state!= BOTTOM && state != TOP, "No bottom or top states");
		return low;
	}

	AbstractNodeValue(){
		valSet.clear();
		low = 0;
		high = -1;
		state = BOTTOM;
	}
	AbstractNodeValue(int initValue){
		valSet.insert(initValue);
		low = initValue;
		high = initValue;
		state = LIST;
	}
	void init(int initValue){
		valSet.insert(initValue);
		low = initValue;
		high = initValue;
		state = LIST;
	}

	void makeTop(){
		valSet.clear();
		low = 0;
		high = -1;
		state = TOP;
	}

	void print(ostream& out){
		if(state != LIST){ 
			out<<"BOTTOM"<<endl;
		}else{
			Assert(valSet.size() > 0, "This is strange. This shouldn't happen");
			for(set<int>::iterator it = valSet.begin(); it != valSet.end(); ++it){	
				out<<*it<<", ";
			}
			out<<endl;
		}
	}

	template<typename COMP>
	int staticCompare(int C , bool reverse ){
		COMP comp;
		if(state != LIST){ return 0; }
		int rv = -2;
		for(set<int>::iterator it = valSet.begin(); it != valSet.end(); ++it){	
			bool cm = reverse? comp(C, *it) : comp(*it, C);
			int tmp =  cm ? 1 : -1;
			if((rv != -2 && tmp != rv)){
				return 0;
			}
			rv = tmp;
		}
		if(rv == -2){return 0; }		
		return rv;
	}
	void insert(int val){
		if( val > high){ high = val; }
		if(val < low){ low = val; }
		if(state==LIST || state==BOTTOM){
			valSet.insert(val);
			state = LIST;
		}
	}

	int difference(AbstractNodeValue& anv){
		if(anv.state != LIST){
			if(state != LIST){
				return 0;
			}else{
				return valSet.size();
			}
		}
		if(state != LIST){
			return anv.valSet.size();
		}
		set<int> tmp = valSet;
		tmp.insert(anv.valSet.begin(), anv.valSet.end());
		int sz1 = valSet.size();
		int sz2 = anv.valSet.size();
		int szmax = sz1 > sz2 ?  sz1 : sz2;
		int szmin = sz1 > sz2 ?  sz2 : sz1;
		int sol = tmp.size() - szmax;
		int dif = szmax - szmin;
		return sol * 10 + dif;
	}

	void insert(AbstractNodeValue& anv){
		if(anv.isTop()){
			makeTop();
			return;
		}
		if(isTop()){
			return;
		}
		
		if(this->isBottom()){
			low = anv.low;
			high = anv.high;
			valSet = anv.valSet;
			state = anv.state;			
		}else{
			low = low < anv.low ? low : anv.low;
			high = high > anv.high? high : anv.high;
			if(state == LIST){
				valSet.insert(anv.valSet.begin(), anv.valSet.end());
			}
		}				
	}

	void add(AbstractNodeValue& anv){
		if(anv.isTop()){
			makeTop();
			return;
		}
		if(isTop()){
			return;
		}
		
		if(this->isBottom()){
			low = anv.low;
			high = anv.high;
			valSet = anv.valSet;
			state = anv.state;			
		}else{
			low = low + anv.low;
			high = high + anv.high;
			if(state == LIST){
				set<int> tmp;
				for(set<int>::iterator it1 = valSet.begin(); it1 != valSet.end(); ++it1){
					for(set<int>::iterator it2 = anv.valSet.begin(); it2 != anv.valSet.end(); ++it2){
						tmp.insert(*it1 + *it2);
					}
				}
				swap(tmp, valSet);
			}
		}				
	}


};



class DagOptim : public NodeVisitor, public virtual NodeStore
{	
	bool ALTER_ARRACS;	
protected:
	DagCSE cse;	
	bool_node* stillPrivate;
	map<string, int> specialization;


	void checkAndPush(bool_node* bn, stack<bool_node*>& sd, set<bool_node*>& bnmap);
	bool checkPrecedence(bool_node* dest, bool_node* src);

public:
	map<bool_node*, AbstractNodeValue> anv;
	
	map<int, CONST_node*> cnmap;
	map<string, UFUN_node*> callMap;

	DagOptim(BooleanDAG& dag);
	virtual ~DagOptim();
	
	bool_node* quickcse(int idmom, int idpop, bool_node::Type t){
		return cse.quickcse(idmom, idpop, t);
	}

	void alterARRACS(){ ALTER_ARRACS = true; } 

	/*
		When optimizing the dag, input name will be set to value
		val. The input, however, will not be removed.
	*/
	void specializeInput(const string& name, int val){
		specialization[name] = val;
	}


	bool_node* computeOptim(bool_node* node);
	bool_node* computeCSE(bool_node* node);
	
	void initialize(BooleanDAG& dag);
	void cleanup(BooleanDAG& dag);
	void initLight(BooleanDAG& dag);
	template<typename COMP, typename NTYPE>
	bool compSymplification(NTYPE& node);
	
	template<typename COMP>
	int staticCompare(bool_node* n1, int C , bool reverse );
	
	template<typename COMP>
	void helper(bool_node* parent, int C, int reverse, int& rv, AbstractNodeValue& nv);

	virtual void visit( SRC_node& node );
	virtual void visit( CTRL_node& node );
	virtual void visit( CONST_node& node );
	
	virtual void visit( AND_node& node );
	virtual void visit( OR_node& node );
	virtual void visit( XOR_node& node );
		
	virtual void visit( NOT_node& node );
	
	virtual void visit( PLUS_node& node );
	virtual void visit( TIMES_node& node );
	virtual void visit( UFUN_node& node );
	virtual void visit( ARRACC_node& node );
	virtual void visit( DIV_node& node );
	virtual void visit( MOD_node& node );
	virtual void visit( NEG_node& node );
	
	virtual void visit( GT_node& node );
	virtual void visit( GE_node& node );
	virtual void visit( LT_node& node );
	virtual void visit( LE_node& node );
	virtual void visit( EQ_node& node );
	virtual void visit( ARRASS_node& node );
	virtual void visit( ACTRL_node& node );
	
	virtual void visit( ASSERT_node &node);	
	virtual void visit( DST_node& node );
	virtual void process(BooleanDAG& bdag);	
	virtual  CONST_node* getCnode(int c);
	virtual  CONST_node* getCnode(bool val); 
	virtual bool isNegOfEachOther(bool_node* n1, bool_node* n2);
	virtual bool isNotOfEachOther(bool_node* n1, bool_node* n2); 
	virtual bool isConst(const bool_node* n1);
	virtual bool getBval(const bool_node* n1);
	virtual int getIval(const bool_node* n1);
};



#endif /*DAGOPTIM_H_*/
