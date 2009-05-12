#pragma once
#include "BooleanNodes.h"
#include "BooleanDAG.h"
#include "NodeVisitor.h"
#include "VarStore.h"
#include <map>

using namespace std;

class NodeEvaluator :
	public NodeVisitor
{
	map<UFUN_node*, NodeEvaluator> recursives;
	map<string, BooleanDAG*> functionMap;
	vector<int> values;
	VarStore inputs;
	
	int i(bool_node& bn){
		return values[bn.id];
	}

	bool b(bool_node& bn){
		return values[bn.id] == 1;
	}
	void setbn(bool_node& bn, int i){
		values[bn.id] = i;
	}

	void setbn(bool_node& bn, bool c){
		values[bn.id] = c ? 1 : 0;
	}
public:
	NodeEvaluator(map<string, BooleanDAG*>& functionMap_p, VarStore& inputs_p);
	NodeEvaluator();
	~NodeEvaluator(void);
	virtual void visit( AND_node& node );
	virtual void visit( OR_node& node );
	virtual void visit( XOR_node& node );
	virtual void visit( SRC_node& node );
	virtual void visit( DST_node& node );
	virtual void visit( NOT_node& node );
	virtual void visit( CTRL_node& node );
	virtual void visit( PLUS_node& node );
	virtual void visit( TIMES_node& node );
	virtual void visit( UFUN_node& node );
	virtual void visit( ARRACC_node& node );
	virtual void visit( DIV_node& node );
	virtual void visit( MOD_node& node );
	virtual void visit( NEG_node& node );
	virtual void visit( CONST_node& node );
	virtual void visit( GT_node& node );
	virtual void visit( GE_node& node );
	virtual void visit( LT_node& node );
	virtual void visit( LE_node& node );
	virtual void visit( EQ_node& node );
	virtual void visit( ARRASS_node& node );
	virtual void visit( ACTRL_node& node );
	virtual void visit( ASSERT_node &node);		
	void process(BooleanDAG& bdag);

	int getValue(bool_node& bn){
		return i(bn);
	}
	int getValue(bool_node* bn){
		return i(*bn);
	}
};
