#ifndef DAGELIMUFUN_H_
#define DAGELIMUFUN_H_


#include "BooleanDAG.h"
#include "SATSolver.h"
#include "DagOptim.h"
#include <set>



using namespace std;










class SFunInfo{
	public:
		/**

			fun is the last uninterpreted function that was produced for the function in question. 

			It has an input SVAR, corresponding to the symbolic value for the function, 
			and PARAMS, corresponding to the formal parameters of the function. 

			fun also encodes within it all the previous parameters seen, and all the previous SVARs seen.

			fun_i(PARAMS_i, SVAR_i) == SVAR_i iff PARAMS_i is different from all the previous parameters seen.
			fun_i(PARAMS_i, SVAR_i) == SVAR_j iff PARAMS_i is equal to PARAMS_j, where j is the parameter that was passed to the jth version of fun.
			

			fun_i is defined recursively as follows:

			fun_0(PARAMS_0, SVAR_0) := SVAR_0

			fun_i(PARAMS_i, SVAR_i) := let (tmp := (PARAMS_i == PARAMS_i-1) ? SVAR_i-1 : SVAR_i) in fun_i-1(PARAMS_i, tmp);


		*/
	BooleanDAG* fun;
	/**
		These are the last actual parameters passed to fun. At the beginning of the substitution, this will equal PARAMS_i-1;
	*/
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



class DagElimUFUN : public NodeVisitor, virtual NodeStore
{
	
	bool oneMoreFun;
	
	
	map<int, BooleanDAG> comparators;	
	map<string, SFunInfo> functions;
	BooleanDAG& getComparator(int sz);
	map<int, vector<bool_node*> > mothercache;
	vector<BooleanDAG> bdags;

	SRC_node* srcNode(UFUN_node& node, int i);

public:
	DagElimUFUN();
	virtual ~DagElimUFUN();
	bool_node* produceNextSFunInfo( UFUN_node& node  );

	virtual void stopProducingFuns();
	
	virtual void visit( UFUN_node& node );
	
	virtual void process(BooleanDAG& bdag);
};

#endif /*DAGELIMUFUN_H_*/
