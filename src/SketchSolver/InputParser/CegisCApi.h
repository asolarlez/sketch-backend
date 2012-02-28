#ifdef __cplusplus
#pragma once

extern "C" {
#endif



    // classes from elsewhere
    //--------------------------------------------------
    // template
    //-------------------------------------------------- 
    #define STRUCT_PROTOTYPE(X) \
        struct X; \
        typedef struct X X;

    STRUCT_PROTOTYPE(bool_node);
    STRUCT_PROTOTYPE(NodeVector); // stl vector<bool_node>
    // STRUCT_PROTOTYPE(SafeString); // stl string
    STRUCT_PROTOTYPE(BooleanDAG);
    STRUCT_PROTOTYPE(CommandLineArgs);

    typedef enum {BN_AND, BN_OR, BN_XOR, BN_SRC, BN_DST,
        BN_NOT, BN_CTRL, BN_PLUS, BN_TIMES, BN_DIV,
        BN_MOD, BN_NEG, BN_CONST, BN_GT, BN_GE,
        BN_LT, BN_LE, BN_EQ, BN_ASSERT, BN_ARRACC, BN_UFUN,
        BN_ARRASS, BN_ACTRL} BNType;

    // wrappers specific to the interface

    int node_vec_size(NodeVector *v);
    bool_node *node_vec_get(NodeVector *v, int index);



    // wrapped functions
    CommandLineArgs *cmdline_args(int argc, char **argv);
    NodeVector *get_nodes_by_type(BNType t);
    void cl_set_global_params(CommandLineArgs *args);
    // bool_node *bn_clone(bool_node *n, bool copyChildren);



#ifdef __cplusplus
}
#endif
