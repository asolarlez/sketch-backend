#ifndef __NODESTOSOLVER_H
#define __NODESTOSOLVER_H

#include "BooleanDAG.h"
#include "BooleanToCNF.h"
#include "Tvalue.h"


#ifndef INTEGERBOUND
# define INTEGERBOUND  8192*6
#endif

// #define Dout( msg ) msg

// Visitor for conversion of DAG to SAT.
class NodesToSolver : public NodeVisitor {
    const string &outname;
    map<bool_node *, int> &node_values; // -1=false, 1=true, 0=unknown    

    template<typename THEOP> void processArith (bool_node &node);
    template<typename THEOP> int doArithExpr (int quant1, int quant2,
					      int id1, int id2, THEOP comp);
    template<typename COMP> void processComparissons (bool_node &node);
	vector<int> lgv;
    Tvalue tvYES;
    Tvalue tvOne;
    Tvalue tvOneSigned;
	const int YES;
protected:
	SolverHelper &dir;
	vector<Tvalue> &node_ids;
    /* Return the value indexed by given node, or a default value (of given type). */
    inline Tvalue &tval_lookup (bool_node *node, valtype_t default_type = TVAL_BVECT,
				int quant = 1) {
	if (node){		
	    return node_ids[node->id];
	}

	switch (default_type) {
	case TVAL_BVECT:
	    return tvYES;

	case TVAL_SPARSE:
	    return tvOne;

	default:
	    assert (0);  /* Can't get here. */
	}

	return tvYES;
    }

    vector<int> scratchpad;
    vector<int> tmprange;
    vector<int> unirange;

    Tvalue intBvectComputeSum (Tvalue &, Tvalue &);
    Tvalue intBvectAdd (Tvalue &, int, Tvalue &, int);
    void intBvectPlus (arith_node &);
    void intBvectMult (arith_node &);
    void intBvectEq (arith_node &);
    void intBvectLt (arith_node &);
    void intBvectLe (arith_node &);
    void intBvectGt (arith_node &);
    void intBvectGe (arith_node &);

    void boolNodeUpdate (bool_node &, Tvalue &);

public:
   
   /*
    * p_mng is the wrapper for the sat solver.  
    * p_dir is a SAT solver wrapper that provides a few additiona services for the SAT solver.
    * p_outname is a name for this translation. It is used, for example, to name any output files produced.
    * p_node_values contains values for either input or output nodes. 
    * 
    */
   
	    
     NodesToSolver (
	     SolverHelper& p_dir, 
	     const string& p_outname, 
		 map<bool_node*,  int>& p_node_values, 
		 vector<Tvalue>& p_node_ids
	 ) :
	dir(p_dir), outname(p_outname), node_values(p_node_values), 
	node_ids(p_node_ids), YES(p_dir.YES), 
	scratchpad(100),tmprange(2), unirange(1), tvYES( p_dir.YES), tvOne (TVAL_SPARSE, p_dir.YES, 1)
    {
	tmprange[0] = 0;
	tmprange[1] = 1;
	unirange[0] = 1;

	/* Initialize the default "one" integer. */
	tvOne.num_ranges.push_back (guardedVal(p_dir.YES, 1));
    };
    
	int lastGoodVal(int id){
		if(id > lgv.size()){
			return 0;
		}else{
			return lgv[id-1];
		}
	}
    

    virtual void visit (AND_node &node);
    virtual void visit (OR_node &node);
    virtual void visit (XOR_node &node);
    virtual void visit (SRC_node &node);
    virtual void visit (DST_node &node);
    virtual void visit (NOT_node &node);
    virtual void visit (CTRL_node &node);
    virtual void visit (PLUS_node &node);
    virtual void visit (TIMES_node &node);
    virtual void visit (ARRACC_node &node);
    virtual void visit (UFUN_node &node);
    virtual void visit (DIV_node &node);
    virtual void visit (NEG_node &node);
    virtual void visit (MOD_node &node);
    virtual void visit( CONST_node& node );
    virtual void visit (GT_node &node);
    virtual void visit (GE_node &node);
    virtual void visit (LT_node &node);
    virtual void visit (LE_node &node);
    virtual void visit (EQ_node &node);
    virtual void visit (ARRASS_node &node);
    virtual void visit (ACTRL_node &node);

    virtual void visit (ASSERT_node &node);
	// void process(BooleanDAG& bdag);
	virtual void mergeTvalues(int guard, Tvalue& mid0, Tvalue& mid1, Tvalue& output, int& flag);
    virtual void doNonBoolArrAcc (ARRACC_node& node, Tvalue& output);
    virtual bool checkParentsChanged (bool_node &node, bool more);	
};

#endif /* __NODESTOSOLVER_H */

