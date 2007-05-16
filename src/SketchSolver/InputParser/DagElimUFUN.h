#ifndef DAGELIMUFUN_H_
#define DAGELIMUFUN_H_


#include "BooleanDAG.h"
#include "SATSolver.h"
#include "ExtractEvaluationCondition.h"
#include <set>



using namespace std;










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
	
	ExtractEvaluationCondition tnbuilder;
	
	map<int, BooleanDAG> comparators;	
	map<string, SFunInfo> functions;
	BooleanDAG& getComparator(int sz);
	
	vector<bool_node*> newnodes;
	
	int dagsize;
	vector<BooleanDAG> bdags;
public:
	DagElimUFUN();
	virtual ~DagElimUFUN();
	bool_node* produceNextSFunInfo( UFUN_node& node  );
	
	virtual void stopProducingFuns();
	
	virtual void visit( UFUN_node& node );
	
	virtual void process(BooleanDAG& bdag);
};

#endif /*DAGELIMUFUN_H_*/
