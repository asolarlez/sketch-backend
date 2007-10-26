#ifndef DAGFUNCTIONTOASSERTION_H_
#define DAGFUNCTIONTOASSERTION_H_


#include "DagOptim.h"
#include "ExtractEvaluationCondition.h"

class DagFunctionToAssertion : public DagOptim
{	
	ExtractEvaluationCondition tnbuilder;
	BooleanDAG& dag;
	map<string, BooleanDAG*>& functionMap;	
	vector<bool_node*> newnodes;
	
public:
	DagFunctionToAssertion(BooleanDAG& p_dag, map<string, BooleanDAG*>& p_functionMap);
	virtual ~DagFunctionToAssertion();
	//virtual void process(BooleanDAG& bdag);
		
	virtual void visit( UFUN_node& node );
	
};


#endif
