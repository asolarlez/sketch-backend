#ifndef DAGELIMUFUN_H_
#define DAGELIMUFUN_H_


#include "BooleanDAG.h"
#include "SATSolver.h"

class SFunInfo{
	public:
	BooleanDAG* fun;
	vector<bool_node* > actuals;
	bool_node* symval;	
	bool_node* outval;
	int step;	
	bool moreNewFuns;
	SFunInfo():
	fun(NULL),
	symval(NULL),
	outval(NULL),
	step(0),
	moreNewFuns(true)
	{
			 
	}
};



class DagElimUFUN : public NodeVisitor
{
	
	bool oneMoreFun;
	
	
	map<int, BooleanDAG> comparators;	
	map<string, SFunInfo> functions;
	BooleanDAG& getComparator(int sz);
	
	vector<bool_node*> newnodes;
	bool_node* rvalue;
	int dagsize;
	vector<BooleanDAG> bdags;
public:
	DagElimUFUN();
	virtual ~DagElimUFUN();
	bool_node* produceNextSFunInfo( UFUN_node& node  );
	
	virtual void stopProducingFuns();
	
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
};

#endif /*DAGELIMUFUN_H_*/
