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
	int divFactor;
	int oldNfun;
	bool mergeFunctions;
	timerclass replTime;
	timerclass ufunAll;
	timerclass optAll;
	timerclass replTime2;
	timerclass tnbuildTime;
	timerclass optimTime;
	timerclass cleanupTime;
	timerclass unifyTime;
	timerclass clonetime;
	map<string, timerclass> optTimers;
	vector<bool_node*> clones;
	map<int, int> specialInputs;
	int expectedNFuns;
	virtual void immInline(BooleanDAG& dag);
	int argsCompare(vector<bool_node*> arg1, vector<bool_node*> arg2);
	void mergeFuncalls(int first, int second);
public:
	DagFunctionInliner(BooleanDAG& p_dag, map<string, BooleanDAG*>& p_functionMap, int inlineAmnt, bool p_mergeFunctions);
	virtual ~DagFunctionInliner();
	virtual void process(BooleanDAG& bdag);
		
	virtual void visit( UFUN_node& node );

	virtual void computeSpecialInputs();

	virtual void unify();
	
};


#endif /*DAGFUNCTIONINLINER_H_*/
