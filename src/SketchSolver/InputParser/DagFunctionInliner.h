#ifndef DAGFUNCTIONINLINER_H_
#define DAGFUNCTIONINLINER_H_

#include "BooleanDAG.h"
#include "DagOptim.h"
#include "ExtractEvaluationCondition.h"


class DagFunctionInliner : public DagOptim
{
	
	bool somethingChanged;
	ExtractEvaluationCondition tnbuilder;
	BooleanDAG& dag;
	map<string, BooleanDAG*>& functionMap;	
	int inlineAmnt;
	timerclass replTime;
	timerclass ufunAll;
	timerclass optAll;
	timerclass replTime2;
	timerclass tnbuildTime;
	timerclass optimTime;
	virtual void immInline(BooleanDAG& dag);
public:
	DagFunctionInliner(BooleanDAG& p_dag, map<string, BooleanDAG*>& p_functionMap, int inlineAmnt);
	virtual ~DagFunctionInliner();
	virtual void process(BooleanDAG& bdag);
		
	virtual void visit( UFUN_node& node );
	
};


#endif /*DAGFUNCTIONINLINER_H_*/
