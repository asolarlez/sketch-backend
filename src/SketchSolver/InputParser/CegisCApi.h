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
    STRUCT_PROTOTYPE(BooleanDAG);
    STRUCT_PROTOTYPE(CommandLineArgs);
    STRUCT_PROTOTYPE(InterpreterEnvironment);

    typedef enum{BN_AND, BN_OR, BN_XOR, BN_SRC, BN_DST,
        BN_NOT, BN_CTRL,BN_PLUS, BN_TIMES, BN_DIV, BN_MOD,
        BN_NEG, BN_CONST, BN_GT, BN_GE, BN_LT, BN_LE,
        BN_EQ, BN_ASSERT, BN_ARRACC, BN_UFUN, BN_ARRASS,
        BN_ACTRL, BN_ARR_R, BN_ARR_W, BN_ARR_CREATE} BNType;

    // wrappers specific to the interface

    int node_vec_size(NodeVector *v);
    bool_node *node_vec_get(NodeVector *v, int index);



    // command line argument handling / global settings
    CommandLineArgs *cmdline_args(int argc, char **argv);
    void cl_set_global_params(CommandLineArgs *args);
    char *cl_get_in_name(CommandLineArgs *args);
    void cl_set_in_name(CommandLineArgs *args, char *name);
    char *cl_get_out_name(CommandLineArgs *args);
    void cl_set_verbosity(CommandLineArgs *args, int v);

    // core synthesis functions
    void runDriver();
    InterpreterEnvironment *getEnvt();
    BooleanDAG *evt_get_copy(InterpreterEnvironment *evt, char *s);
    BooleanDAG *evt_prepare_miter(InterpreterEnvironment *evt,
        BooleanDAG *spec, BooleanDAG *sketch);
    int evt_assert_dag(InterpreterEnvironment *evt, BooleanDAG *miter);
    void evt_write_controls(InterpreterEnvironment *evt, char *fn);
    int evt_is_ready(InterpreterEnvironment *evt);
    void evt_get_controls(InterpreterEnvironment *evt, int *nkeys, char ***keys, int **values);

    // boolean dag
    NodeVector *bdag_get_nodes_by_type(BooleanDAG *dag, BNType t);
    BooleanDAG *bdag_new();
    BooleanDAG *bdag_clone(BooleanDAG *dag);
    void bdag_add_new_node(BooleanDAG *dag, bool_node *node);

    // boolean node
    int bn_is_minimize(bool_node *n);
    char *bn_get_name(bool_node *n);
    bool_node *bn_new(BooleanDAG *dag,
                      bool_node *mother,
                      bool_node *father,
                      BNType typ);
    bool_node *bn_clone(bool_node *n, int copyChildren);
    void bn_set_const(bool_node *n, int value);



#ifdef __cplusplus
}
#endif
