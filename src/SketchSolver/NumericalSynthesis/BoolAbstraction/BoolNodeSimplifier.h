#pragma once
#include "DagOptim.h"
#include "SimpleGradEvaluator.h"

class BoolNodeSimplifier :
	public DagOptim
{
	SimpleGradEvaluator* seval;	
public:
	BoolNodeSimplifier(BooleanDAG& dag, FloatManager& _floats):
		DagOptim(dag, _floats) { }
	~BoolNodeSimplifier(void) {}
	virtual void visit( SRC_node& node );
    virtual void visit( DST_node& node );
    virtual void visit( CTRL_node& node );
    virtual void visit( PLUS_node& node );
    virtual void visit( TIMES_node& node );
    virtual void visit( ARRACC_node& node );
    virtual void visit( DIV_node& node );
    virtual void visit( MOD_node& node );
    virtual void visit( NEG_node& node );
    virtual void visit( CONST_node& node );
    virtual void visit( LT_node& node );
    virtual void visit( EQ_node& node );
    virtual void visit( AND_node& node );
    virtual void visit( OR_node& node );
    virtual void visit( NOT_node& node );
    virtual void visit( ARRASS_node& node );
    virtual void visit( UFUN_node& node );
    virtual void visit( TUPLE_R_node& node );
    virtual void visit( ASSERT_node& node );

	void process(BooleanDAG& bdag, int nodeid, SimpleGradEvaluator* seval_);
};


