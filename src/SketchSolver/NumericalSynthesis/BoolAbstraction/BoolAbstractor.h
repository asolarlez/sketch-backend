#pragma once

#include <functional>
#include "NodesToSolver.h"
#include "Interface.h"




// Visitor for conversion of DAG to SAT.
class BoolAbstractor : public NodesToSolver {
    Interface* interface;
    
public:
    /*
     * p_mng is the wrapper for the sat solver.
     * p_dir is a SAT solver wrapper that provides a few additiona services for the SAT solver.
     * p_outname is a name for this translation. It is used, for example, to name any output files produced.
     * p_node_values contains values for either input or output nodes.
     *
     */
    
    
    BoolAbstractor (
                    SolverHelper& p_dir,
                    const string& p_outname,
                    map<bool_node*,  int>& p_node_values,
                    vector<Tvalue>& p_node_ids,
                    FloatManager& _floats,
                    Interface* _interface
                    ) : NodesToSolver(p_dir, p_outname, p_node_values, p_node_ids, _floats), interface(_interface) {}
    
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
    virtual void visit (LT_node &node);
    virtual void visit (EQ_node &node);
    virtual void visit (ARRASS_node &node);
    virtual void visit (ACTRL_node &node);
    
    virtual void visit( ARR_R_node &node);
    virtual void visit( ARR_W_node &node);
    virtual void visit( ARR_CREATE_node &node);
    
    virtual void visit( TUPLE_R_node &node);
    virtual void visit( TUPLE_CREATE_node &node);
    
    virtual void visit (ASSERT_node &node);
    
    void addToInterface(Tvalue& tv, bool_node& node);
    
    static bool createConstraints(BooleanDAG& dag, SolverHelper& dir, map<bool_node*,  int>& node_values, vector<Tvalue>& node_ids, FloatManager& floats, Interface* interface);
    
};



