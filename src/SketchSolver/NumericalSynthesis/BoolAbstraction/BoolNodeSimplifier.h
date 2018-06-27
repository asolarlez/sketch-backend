#pragma once
#include "DagOptim.h"
#include "SimpleEvaluator.h"

class BoolNodeSimplifier :
	public DagOptim
{
	SimpleEvaluator* seval;	
public:
	BoolNodeSimplifier(BooleanDAG& dag, FloatManager& _floats):
		DagOptim(dag, _floats) { }
	~BoolNodeSimplifier(void) {}
	virtual void visit( ARRACC_node& node );
	virtual void visit( AND_node& node );
	virtual void visit( OR_node& node );
	virtual void visit( ASSERT_node& node);

	void process(BooleanDAG& bdag, int nodeid, SimpleEvaluator* seval_);
};


