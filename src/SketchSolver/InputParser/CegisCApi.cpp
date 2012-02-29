
#include "CegisCApi.h"
#include "BooleanNodes.h"
#include "CommandLineArgs.h"
#include "InterpreterEnvironment.h"

struct NodeVector {
    vector<bool_node*> *vec;
    NodeVector(vector<bool_node*> *v) : vec(v) { }
};

bool_node *bn_clone(bool_node *n, bool copyChildren) {
    return n->clone(copyChildren);
}

int node_vec_size(NodeVector *v) {
    return v->vec->size();
}

bool_node *node_vec_get(NodeVector *v, int i) {
    return (*(v->vec))[i];
}

CommandLineArgs *cmdline_args(int argc, char **argv) {
    return new CommandLineArgs(argc, argv);
}

void cl_set_global_params(CommandLineArgs *args) {
    args->setPARAMS();
}

char *cl_get_in_name(CommandLineArgs *args) {
    return (char *)(args->inputFname.c_str());
}

void cl_set_in_name(CommandLineArgs *args, char *name) {
    args->inputFname = string(name);
}

char *cl_get_out_name(CommandLineArgs *args) {
    return (char *)(args->outputFname.c_str());
}

BooleanDAG *evt_get_copy(InterpreterEnvironment *evt, char *s) {
    return evt->getCopy(string(s));
}

BooleanDAG *evt_prepare_miter(InterpreterEnvironment *evt,
    BooleanDAG *spec, BooleanDAG *sketch)
{
    return evt->prepareMiter(spec, sketch);
}

int evt_assert_dag(InterpreterEnvironment *evt, BooleanDAG *miter) {
    return evt->assertDAG_wrapper(miter);
}

NodeVector *bdag_get_nodes_by_type(BooleanDAG *dag, BNType t) {
    vector<bool_node*> *vec = new vector<bool_node*>();
    *vec = dag->getNodesByType((bool_node::Type)t);
    return new NodeVector(vec);
}

void evt_print_controls(InterpreterEnvironment *evt, char *fn) {
    evt->printControls_wrapper(string(fn));
}
