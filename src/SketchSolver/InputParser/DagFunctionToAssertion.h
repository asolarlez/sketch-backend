#ifndef DAGFUNCTIONTOASSERTION_H_
#define DAGFUNCTIONTOASSERTION_H_


#include "DagOptim.h"

class DagFunctionToAssertion : public DagOptim
{	
	BooleanDAG& dag;
	const map<string, const BooleanDAG*>& functionMap;
	vector<bool_node*> newnodes;
	
public:
	DagFunctionToAssertion(BooleanDAG& p_dag, const map<string, const BooleanDAG *> &p_functionMap, FloatManager& fm);
	virtual ~DagFunctionToAssertion();
	//virtual void process(BooleanDAG& bdag);
		
	virtual void visit( UFUN_node& node );
	
};


#endif
