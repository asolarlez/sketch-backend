#ifndef DAGFUNCTIONINLINER_H_
#define DAGFUNCTIONINLINER_H_

#include "BooleanDAG.h"
#include "DagOptim.h"
#include "ExtractEvaluationCondition.h"


class DagFunctionInliner : public NodeVisitor
{
	
	bool somethingChanged;
	ExtractEvaluationCondition tnbuilder;
	BooleanDAG& dag;
	map<string, BooleanDAG*>& functionMap;	
	vector<bool_node*> newnodes;
	virtual void immInline(BooleanDAG& dag);
public:
	DagFunctionInliner(BooleanDAG& p_dag, map<string, BooleanDAG*>& p_functionMap);
	virtual ~DagFunctionInliner();
	virtual void process(BooleanDAG& bdag);
		
	virtual void visit( UFUN_node& node );
	
};


class DagFunctionToAssertion : public NodeVisitor
{	
	ExtractEvaluationCondition tnbuilder;
	BooleanDAG& dag;
	map<string, BooleanDAG*>& functionMap;	
	vector<bool_node*> newnodes;
	
public:
	DagFunctionToAssertion(BooleanDAG& p_dag, map<string, BooleanDAG*>& p_functionMap);
	virtual ~DagFunctionToAssertion();
	virtual void process(BooleanDAG& bdag);
		
	virtual void visit( UFUN_node& node );
	
};



#endif /*DAGFUNCTIONINLINER_H_*/
