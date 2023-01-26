#pragma once


#include "BooleanDAG.h"
#include "DagOptim.h"
#include <map>
#include <cstring>

using namespace std;

class ComplexInliner: public DagOptim
{
	BooleanDAG& dag;
	map<string, const BooleanDAG*>& functionMap;
	map<int, int> specialInputs;
	bool somethingChanged;
	int inlineAmnt;
	int divFactor;
	int oldNfun;
	bool mergeFunctions;

	timerclass optAll;

	timerclass optimTime;

	timerclass unifyTime;
	timerclass clonetime;
	int argsCompare(vector<bool_node*> arg1, bool_node::parent_iter arg2_beg, bool_node::parent_iter arg2_end);
	void mergeFuncalls(int first, int second);
	virtual void immInline(BooleanDAG& dag);
	int expectedNFuns;
public:
	ComplexInliner(BooleanDAG& p_dag, map<string, const BooleanDAG *> &p_functionMap, int p_inlineAmnt, bool p_mergeFunctions, FloatManager& fm);
	~ComplexInliner(void);
	virtual void process(BooleanDAG &bdag);
	virtual void computeSpecialInputs();
	virtual void unify();
};
