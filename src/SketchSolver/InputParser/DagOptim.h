#ifndef DAGOPTIM_H_
#define DAGOPTIM_H_

#include "BooleanDAG.h"
#include "DagCSE.h"
#include "NodeStore.h"
#include <typeinfo>
#include <set>
#include <stack>
#include <functional>

using namespace std;





class AbstractNodeValue{		
public:
	typedef enum { BOTTOM, LIST, RANGE, TOP } State;

class ANVIterator{
	set<int>::const_iterator bgit;
	set<int>::const_iterator qit;
	set<int>::const_iterator eit;
	int i;
	AbstractNodeValue::State state;
	int limit; //Never return a value greater than this.	
public:
	bool dropped;
	ANVIterator(set<int>::const_iterator beg, set<int>::const_iterator end,  int lim){
		bgit = beg;
		qit = beg;
		eit = end;
		state = AbstractNodeValue::LIST;
		limit = lim;
		dropped = false;				
	}
	ANVIterator(int start, int end, int lim){
		i = max(start, 0);
		limit = min(end+1, lim);
		if(start < 0 || end >= lim){
			dropped = true;
		}
		i = i-1;
		state = AbstractNodeValue::RANGE;
	}
	int operator*(){
		if(state==AbstractNodeValue::LIST){
			return *bgit;
		}
		return i;
	}
	bool next(){
		if(state==AbstractNodeValue::LIST){
			bgit = qit;
			while(bgit != eit && (*bgit<0 || *bgit >= limit)){
				++bgit;
				dropped = true;
			}			
			if(bgit == eit){
				return false;
			}
			qit = bgit; ++qit;
			return true;
		}
		if(state == AbstractNodeValue::RANGE){
			++i;
			if(i >= limit){
				return false;
			}			
			return true;
		}
		Assert(false, "NYI; njhytggfd");
		return false;
	}	
};

private:
	State state;
	set<int> valSet;
	int low;
	int high;	
	set<int>::const_iterator vset_begin() const{
		return valSet.begin();
	}
	set<int>::const_iterator vset_end()const{
		return valSet.end();
	}
public:
	int timestamp;
	ANVIterator getIter(int lim){
		if(state==LIST){
			return ANVIterator(valSet.begin(), valSet.end(), lim);
		}
		if(state==RANGE){
			return ANVIterator(low, high, lim);
		}
		Assert(false, "You shouldn't reach here");
		return ANVIterator(low, high, lim);
	}
	bool isList(){ return state == LIST; }
	bool isTop(){ return state==TOP; }
	bool isRange(){ return state == RANGE; }
	bool isBottom(){ return state==BOTTOM; }
	
	bool contains(int i){
		switch(state){
			case LIST: return valSet.count(i)>0;
			case RANGE: return i>=low && i <= high; 
			case TOP: return true;
			default: return false;
		}
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

	void init(int low, int high){
		valSet.clear();
		this->low = low;
		this->high = high;
		state = RANGE;
	}

	void makeTop(){
		valSet.clear();
		low = 0;
		high = -1;
		state = TOP;
	}

	void print(ostream& out){
		switch(state){
			case LIST:{
						Assert(valSet.size() > 0, "This is strange. This shouldn't happen");
						for(set<int>::iterator it = valSet.begin(); it != valSet.end(); ++it){	
							out<<*it<<", ";
						}
						out<<endl;
						break;
					  }
			case RANGE:{ out<<"["<<low<<", "<<high<<"]"<<endl; break; }
			default:out<<"BOTTOM"<<endl;
		}		
	}

	/**
	return 1 means I know for sure the comparison is true;
	return -1 means I know for sure it is false;
	return 0 means I don't know.
	*/
	template<typename COMP>
	int staticCompare(int C , bool reverse ){
		COMP comp;
		if(state == LIST){
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
		if(state == RANGE){
			return specializedComp(C, reverse, comp);
		}
		return 0; 
	}



	void insert(int val){
		if( val > high){ high = val; }
		if(val < low){ low = val; }
		if(state==LIST || state==BOTTOM){
			valSet.insert(val);
			state = LIST;
		}
	}
		
	int specializedComp(int C , bool reverse, less<int>& tt ){
		int rv = 0;
		if(reverse){			
			if(C < low) rv = 1;
			if(C >= high) rv = -1;
			return rv;
		}else{
			if(high < C) rv = 1;
			if(C <= low) rv = -1;
			return rv;
		}
	}

	
	int specializedComp(int C , bool reverse, greater<int>& tt ){
		int rv = 0;
		if(reverse){			
			if(high < C) rv = 1;
			if(C <= low) rv = -1;
			return rv;
		}else{
			if(C < low) rv = 1;
			if(C >= high) rv = -1;
			return rv;			
		}
	}

	
	int specializedComp(int C , bool reverse, less_equal<int>& tt ){
		int rv = 0;
		if(reverse){			
			if(C <= low) rv = 1;
			if(C > high) rv = -1;
			return rv;
		}else{
			if(high <= C) rv = 1;
			if(C < low) rv = -1;
			return rv;
		}
	}

	
	int specializedComp(int C , bool reverse, greater_equal<int>& tt ){
		int rv = 0;
		if(reverse){			
			if(high <= C) rv = 1;
			if(C < low) rv = -1;
			return rv;
		}else{
			if(C <= low) rv = 1;
			if(C > high) rv = -1;
			return rv;			
		}
	}


	
	int specializedComp(int C , bool reverse, equal_to<int>& tt ){
		int rv = 0;
		
		if(C == low && C == high) rv = 1;
		if(C > high || C < low) rv = -1;
		return rv;
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
				if(anv.state == LIST){
					valSet.insert(anv.valSet.begin(), anv.valSet.end());
				}else{
					if(anv.state == RANGE){
						valSet.clear();
						state = RANGE;
					}
				}
			}
		}				
	}

	void scale(int v){
		if(isTop() || isBottom()){
			return;
		}
		if(v>=0){
			low = low * v;
			high = high * v;
		}else{
			low = v*high;
			high = v*low;
		}
		if(state==LIST){
			if(v != 0){
				set<int> tmp;
				for(set<int>::iterator it1 = valSet.begin(); it1 != valSet.end(); ++it1){
					tmp.insert(*it1 * v);
				}
				swap(tmp, valSet);
			}else{
				valSet.clear();
				valSet.insert(0);
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
				if(valSet.size()*anv.valSet.size() > 10){
					state = RANGE;
					valSet.clear();
					return;
				}
				set<int> tmp;
				for(set<int>::iterator it1 = valSet.begin(); it1 != valSet.end(); ++it1){
					for(set<int>::iterator it2 = anv.valSet.begin(); it2 != anv.valSet.end(); ++it2){
						tmp.insert(*it1 + *it2);
					}
				}
				swap(tmp, valSet);
			}
			if(anv.state == RANGE){
				state = RANGE;
				valSet.clear();
				return;
			}
		}
	}


};



class TempTriple{
public:
	bool hasModified;
	ASSERT_node* main;
	bool_node* bn[3];	
	bool f[3];
	TempTriple(){
		hasModified = false;
	}
	void add(int i, bool_node* b, bool ff){
		bn[i] = b;
		f[i] = ff;
	}
};

const int NCREATORS=4;

class DagOptim : public NodeVisitor, public virtual NodeStore
{	
	bool ALTER_ARRACS;	
	bool possibleCycles;
	int nccount;
	pair<int, ARR_CREATE_node>  tempcreators[NCREATORS];
protected:
	DagCSE cse;	
	bool_node* stillPrivate;
	map<string, int> specialization;
	Ostore<TempTriple> triples;
	StringHTable2<TempTriple*> testAsserts;
	bool checkTempcreators(BooleanDAG& dag, bool_node* node, int i);
	int uidcount;
	int BOTTOM;
	int DONE;
	int INSTACK;

	bool optimizeMMsize2(ARRACC_node& node);

	void cbPerNode(bool_node* cur, stack<pair<bool_node*, childset::iterator> >& bns, map<int, UFUN_node*>& dupNodes);

	void checkAndPush(bool_node* bn, stack<bool_node*>& sd, set<bool_node*>& bnmap);
	bool checkPrecedence(bool_node* dest, bool_node* src);
	void findCycles(BooleanDAG& dag);
	void breakCycle(bool_node* bn, stack<pair<bool_node*, childset::iterator> >& s, map<int, UFUN_node*>& dupNodes);
    bool_node* process(UFUN_node* node);
public:
	bool isTopLevel;
	map<bool_node*, FastSet<bool_node> > funDependencies;
	map<bool_node*, AbstractNodeValue> anv;
	
	map<long long int, CONST_node*> cnmap;	
	map<string, UFUN_node*> callMap;
    map<int, UFUN_node*> combinedFunCallMap;

	DagOptim(BooleanDAG& dag);
	virtual ~DagOptim();
	
	bool_node* quickcse(int idmom, int idpop, bool_node::Type t){
		return cse.quickcse(idmom, idpop, t);
	}
    void combineFunCalls(BooleanDAG& dag);

	void alterARRACS(){ ALTER_ARRACS = true; } 

	/*
		When optimizing the dag, input name will be set to value
		val. The input, however, will not be removed.
	*/
	void specializeInput(const string& name, int val){
		specialization[name] = val;
	}


	bool_node* computeOptim(bool_node* node);
	bool_node* optAdd(bool_node* bn);
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
	
	
	virtual void visit( LT_node& node );
	
	virtual void visit( EQ_node& node );
	virtual void visit( ARRASS_node& node );
	virtual void visit( ACTRL_node& node );
	virtual void visit( ARR_R_node& node );
	virtual void visit( ARR_W_node& node );
   virtual void visit( TUPLE_CREATE_node& node);
    virtual void visit( TUPLE_R_node& node);

	virtual void visit( ASSERT_node &node);	
	virtual void visit( DST_node& node );
	virtual void process(BooleanDAG& bdag);	
	virtual  CONST_node* getCnode(int c);
	virtual  CONST_node* getCnode(bool val); 
	virtual  CONST_node* getCnode(double c);
	virtual bool isNegOfEachOther(bool_node* n1, bool_node* n2);
	virtual bool isNotOfEachOther(bool_node* n1, bool_node* n2); 
	bool isConst(const bool_node* n1);
	bool getBval(const bool_node* n1);
	int getIval(const bool_node* n1);
	bool_node*  addGE(bool_node* mother, bool_node* father);
	bool_node*  addLE(bool_node* mother, bool_node* father);
	bool_node*  addGT(bool_node* mother, bool_node* father);
};


inline
bool DagOptim::isConst(const bool_node* n1){
	if( n1->type == bool_node::CONST ){
		return !((CONST_node*)n1)->isFloat();
		// return true;
	}
	return false;
}

inline
bool DagOptim::getBval(const bool_node* n1){
	return getIval(n1) != 0;
}	

inline
int  DagOptim::getIval(const bool_node* n1){
	//Assert( isConst(n1), "This node is not a constant !!");
	const CONST_node * cn = (CONST_node*)(n1);
	return cn->getVal()	;
}



#endif /*DAGOPTIM_H_*/
