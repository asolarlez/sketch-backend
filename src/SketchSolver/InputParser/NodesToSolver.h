#ifndef NODESTOSOLVER_H
#define NODESTOSOLVER_H

#include "BooleanDAG.h"
#include "BooleanToCNF.h"
#include "Tvalue.h"


#ifndef INTEGERBOUND
#define INTEGERBOUND 8192
#endif


class NodesToSolver : public NodeVisitor{
	SATSolver& mng;
	varDir& dir;
	const string& outname;
	map<bool_node*, int>& node_values; // -1=false, 1=true, 0=unknown
	map<bool_node*, Tvalue>& node_ids;
	
	template<typename THEOP>
	void processArith(arith_node& node);
	template<typename THEOP>
	int doArithExpr(int quant1, int quant2, int id1, int id2, THEOP comp);
	template<typename COMP>
	void processComparissons(arith_node& node);
	
	const int YES;
	const string& IN;
	const string& CTRL;
	
vector<int> scratchpad;
vector<int> tmprange;
vector<int> unirange;
	public:
	NodesToSolver(SATSolver& p_mng, varDir& p_dir, const string& p_outname, 
				 map<bool_node*,  int>& p_node_values,
				map<bool_node*,  Tvalue>& p_node_ids, const int p_YES, const string& p_IN, const string& p_CTRL):
		mng(p_mng), dir(p_dir), 
		outname(p_outname), node_values(p_node_values), 
		node_ids(p_node_ids),
		YES(p_YES), IN(p_IN), CTRL(p_CTRL), scratchpad(100),tmprange(2), unirange(1) {
				tmprange[0] = 0;
	tmprange[1] = 1;
	unirange[0] = 1;			
			 };

	virtual void visit( AND_node& node );
	virtual void visit( OR_node& node );
	virtual void visit( XOR_node& node );
	virtual void visit( SRC_node& node );
	virtual void visit( DST_node& node );
	virtual void visit( PT_node& node );
	virtual void visit( CTRL_node& node );
	virtual void visit( PLUS_node& node );
	virtual void visit( TIMES_node& node );
	virtual void visit( ARRACC_node& node );
	virtual void visit( DIV_node& node );
	virtual void visit( MOD_node& node );
	virtual void visit( GT_node& node );
	virtual void visit( GE_node& node );
	virtual void visit( LT_node& node );
	virtual void visit( LE_node& node );
	virtual void visit( EQ_node& node ){ };
	virtual void visit( ARRASS_node& node );
	virtual void visit( ACTRL_node& node );
	
	virtual void doNonBoolArrAcc(arith_node& node);
	virtual bool checkParentsChanged(bool_node& node, bool more);
};





#endif
