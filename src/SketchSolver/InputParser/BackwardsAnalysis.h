#pragma once
#include "BooleanDAG.h"
#include "NodeVisitor.h"
#include "NodeStore.h"
#include <algorithm>
#include <set>
#include <climits>

using namespace std;

class Datum{
public:
	bool_node* node;
	int val;

	Datum(bool_node* p_node):node(p_node->type == bool_node::NOT? p_node->mother: p_node), val(p_node->type == bool_node::NOT? 0 : 1){
		
	}

	Datum(bool_node* p_node, int p_val):node(p_node->type == bool_node::NOT? p_node->mother: p_node),  val(p_node->type == bool_node::NOT? (1-p_val) : p_val){
		
	}
	Datum(const Datum& d): node(d.node), val(d.val){

	}
	Datum():node(NULL), val(-1){
	
	}
	Datum& operator=(const Datum& d){
		node = d.node;
		val = d.val;
		return *this;
	}
};


inline bool operator==(const Datum& d1, const Datum& d2){
	if(d1.node->id == d2.node->id){
		return d1.val == d2.val;
	}else{
		return false;
	}
}

inline bool operator<(const Datum& d1, const Datum& d2){
	if(d1.node->id == d2.node->id){
		return d1.val < d2.val;
	}else{
		return d1.node->id < d2.node->id;
	}
}

class Info{
	set<Datum> known;
	typedef enum{ BOTTOM, MIDDLE, TOP} State;
	State state;
	int hasD;
	Datum temp;

public:
	Info(){
		state = BOTTOM;
		hasD = false;
	}

	void makeTop(){
		state = TOP;
		known.clear();
	}

	void push(const Datum& d){
		Assert(!hasD, "Only one datum can be pushed at a time");
		temp = d;
		hasD = true;
	}

	void pop(){
		Assert(hasD, "Only one datum can be pushed at a time");
		hasD = false;
	}
 
	bool getValue(bool_node* node, int& out){
		
		set<Datum>::iterator iter = known.lower_bound(Datum(node, INT_MIN));
		if(iter == known.end()){ return false; }
		if(node->type == bool_node::NOT){
				if(iter->node == node->mother){ 
					out = iter->val; 
					out = 1-out;
					return true; 
				}
		}else{
			if(iter->node == node){
				out = iter->val; 
				return true; 
			}
		}
		return false;
	}

	Info& add(bool_node* node, int val){
		int tmp;
		Assert(!getValue(node, tmp), "Node should not already be here");
		known.insert(Datum(node, val));
	}
	
	Info& add(bool_node* node){
		int tmp;
		Assert(!getValue(node, tmp), "Node should not already be here");
		known.insert(Datum(node));
	}


	Info& operator+=(const Info& inf){
		Assert(!hasD, "This shouldn't happen");
		if(state == BOTTOM || inf.state == BOTTOM){
			if(inf.state != BOTTOM || inf.hasD){
				state = MIDDLE;
				known = inf.known;
				if(inf.hasD){
					known.insert(inf.temp);
				}
			}
		}else{
			set<Datum> tmp;
			bool contains = inf.hasD && known.count(inf.temp)>0;
			set_intersection(known.begin(), known.end(), inf.known.begin(), inf.known.end(), inserter(tmp, tmp.begin()));
			swap(tmp, known);
			if(contains){
				known.insert(inf.temp);
			}
			state = MIDDLE;
		}
		return *this;
	}
};











class BackwardsAnalysis :
	public NodeVisitor, public virtual NodeStore
{
private:
	map<bool_node*, Info> info;
	map<int, CONST_node*> cnmap;
protected:
	virtual void visitArith(arith_node& node );
	virtual void visitBool(bool_node& node );

public:
	BackwardsAnalysis(void);
	virtual ~BackwardsAnalysis(void);
	virtual void visit( AND_node& node );
	virtual void visit( OR_node& node );
	
	
	virtual void visit( ARRACC_node& node );
	
	virtual void visit( ARRASS_node& node );
	
	virtual void visit( DST_node& node );

	virtual void visit( ASSERT_node &node);	
	
	virtual void process(BooleanDAG& bdag);
	
	CONST_node* getCnode(int val);
	CONST_node* getCnode(bool c);
};
