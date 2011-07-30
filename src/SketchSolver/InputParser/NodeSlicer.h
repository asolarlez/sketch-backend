#pragma once
#include "NodeVisitor.h"
#include "NodeEvaluator.h"

class NodeSlicer :
	public NodeVisitor
{
	map<string, BooleanDAG*>& functionMap;
	map<string, bool_node*> influential;
	VarStore& inputs;
	NodeEvaluator ne;
	vector<bool_node*> slice;
	vector<bool> marked;
	bool isMarked(bool_node* n){
		return marked[n->id];
	}
	void mark(bool_node* n){
		marked[n->id]=true;
		slice.push_back(n);
	}
	bool isMarked(bool_node& n){
		return marked[n.id];
	}
	void mark(bool_node& n){
		marked[n.id]=true;
		slice.push_back(&n);
	}
public:
	NodeSlicer(map<string, BooleanDAG*>& functionMap_p, VarStore& inputs_p, BooleanDAG& bdag_p);
	~NodeSlicer(void);
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
	virtual void visit( LT_node& node );
	virtual void visit( EQ_node& node );
	virtual void visit( ARRASS_node& node );
	virtual void visit( ACTRL_node& node );
	virtual void visit( ASSERT_node &node);	

	virtual void process(BooleanDAG& bdag);

	bool isInfluential(const string& name){
		return influential.count(name)>0;
	}
};
