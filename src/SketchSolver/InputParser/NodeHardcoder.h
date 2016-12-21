#pragma once
#include "DagOptim.h"
#include "VarStore.h"
#include "FloatSupport.h"

class NodeHardcoder :
	public DagOptim
{
	VarStore& values;
	bool_node::Type type;
	map<string, vector<pair<bool_node*, vector<bool_node*> > > > ufunparams;
	bool showInputs;
	BooleanDAG* bdag;	
public:
	void nodeFromSyn(UFUN_node& node);
	bool_node* nodeForINode(INTER_node* inode);
	bool_node* nodeForFun(UFUN_node* uf);
	NodeHardcoder(bool showin, BooleanDAG& dag, VarStore& vals, bool_node::Type tp, FloatManager& _floats):
		DagOptim(dag, _floats), values(vals), type(tp), showInputs(showin), bdag(&dag){ isTopLevel = true;}
	~NodeHardcoder(void);
	virtual void visit( SRC_node& node );
	virtual void visit( CTRL_node& node );
	virtual void visit( UFUN_node& node );
};


