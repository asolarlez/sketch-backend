#ifndef DAGOPTIM_H_
#define DAGOPTIM_H_

#include "BooleanDAG.h"
#include "DagCSE.h"
#include <set>

using namespace std;

class AbstractNodeValue{	
	set<int> valSet;
	int low;
	int high;
	bool isRange(){ return valSet.size() == 0 && low <= high; }
	bool isTop(){ return low > high; }
public:
	AbstractNodeValue(){
		makeTop();
	}
	AbstractNodeValue(int initValue){
		valSet.insert(initValue);
		low = initValue;
		high = initValue;
	}
	void init(int initValue){
		valSet.insert(initValue);
		low = initValue;
		high = initValue;
	}
	void makeTop(){
		valSet.clear();
		low = 0;
		high = -1;
	}
	template<typename COMP>
	int staticCompare(int C , bool reverse ){
		COMP comp;
		if(isTop()){ return 0; }
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
		valSet.insert(val);
	}
	void insert(AbstractNodeValue& anv){
		valSet.insert(anv.valSet.begin(), anv.valSet.end());
		int oldLow = low;
		int oldHigh = high;
		low = (*valSet.begin());
		high = (*valSet.rbegin());
		Assert( low<= oldLow && high >= oldHigh || (valSet.size()==anv.valSet.size() && oldLow==0 && oldHigh==-1), "This is an error. I missunderstood the interface low="<<low<<" oldLow = "<<oldLow<<" high=" <<high<<" oldHigh="<<oldHigh);
	}

};



class DagOptim : public NodeVisitor
{
	int dagsize;
	DagCSE cse;
	bool ALTER_ARRACS;
public:
	map<bool_node*, AbstractNodeValue> anv;
	vector<bool_node*> newnodes;
	
	DagOptim(BooleanDAG& dag);
	virtual ~DagOptim();
	
	void alterARRACS(){ ALTER_ARRACS = true; } 
	
	template<typename COMP, typename NTYPE>
	bool compSymplification(NTYPE& node);
	
	template<typename COMP>
	int staticCompare(bool_node* n1, int C , bool reverse );
	
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
	map<int, CONST_node*> cnmap;
	virtual  CONST_node* getCnode(int c);
	virtual  CONST_node* getCnode(bool val); 
	virtual bool isNegOfEachOther(bool_node* n1, bool_node* n2);
	virtual bool isNotOfEachOther(bool_node* n1, bool_node* n2); 
	virtual bool isConst(bool_node* n1);
	virtual bool getBval(bool_node* n1);
	virtual int getIval(bool_node* n1);
};



#endif /*DAGOPTIM_H_*/
