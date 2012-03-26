#include "CegisCApi.h"
#include "BooleanNodes.h"
#include "CommandLineArgs.h"
#include "InterpreterEnvironment.h"
#include <iostream>

using std::cout;
using std::cerr;
using std::endl;

//--------------------------------------------------
// Helper stuff for C++ to C marshaling
//-------------------------------------------------- 
static int toCInteger(bool value) {
    if (value) { return 1; } else { return 0; }
}

static bool fromCInteger(int value) {
    return (value != 0);
}

struct NodeVector {
    vector<bool_node*> *vec;
    NodeVector(vector<bool_node*> *v) : vec(v) { }
};

int node_vec_size(NodeVector *v) {
    return v->vec->size();
}

bool_node *node_vec_get(NodeVector *v, int i) {
    Assert(i < v->vec->size(), "node_vec_get() -- index out of bounds");
    return (*(v->vec))[i];
}



//--------------------------------------------------
// Command line args
//-------------------------------------------------- 
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

void cl_set_verbosity(CommandLineArgs *args, int v) {
    args->verbosity = v;
}

char *cl_get_out_name(CommandLineArgs *args) {
    return (char *)(args->outputFname.c_str());
}



//--------------------------------------------------
// Core synthesis routines
//-------------------------------------------------- 

// runDriver, getEnvt in other C++ code

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

void evt_write_controls(InterpreterEnvironment *evt, char *fn) {
    evt->printControls_wrapper(string(fn));
}

int evt_is_ready(InterpreterEnvironment *evt) {
    return toCInteger(evt->status == InterpreterEnvironment::READY);
}

void evt_get_controls(InterpreterEnvironment *evt, int *nkeys, char ***keys, int **values) {
    int n = evt->currentControls.size();
    *nkeys = n;
    (*keys) = new char*[n];
    (*values) = new int[n];

    int i = 0;
    for (map<string, int>::iterator it = evt->currentControls.begin();
         it != evt->currentControls.end();
         it++)
    {
        Assert(i < n, "sanity check");
        (*keys)[i] = (char *)(*it).first.c_str();
        (*values)[i] = (*it).second;
        i++;
    }
}



//--------------------------------------------------
// Querying the DAG and manipulating nodes
//-------------------------------------------------- 
NodeVector *bdag_get_nodes_by_type(BooleanDAG *dag, BNType t) {
    vector<bool_node*> *vec = new vector<bool_node*>();
    *vec = dag->getNodesByType((bool_node::Type)t);
    return new NodeVector(vec);
}

BooleanDAG *bdag_new() {
    return new BooleanDAG();
}

BooleanDAG *bdag_clone(BooleanDAG *dag) {
    return dag->clone();
}

void bdag_add_new_node(BooleanDAG *dag, bool_node *node) {
    dag->addNewNode(node);
}

int bn_is_minimize(bool_node *n) {
    // cerr << "given node pointer: " << (long)((void *)n) << endl;
    Assert(n != NULL, "bn_is_minimize() -- given a null value");
    Assert(n->type == bool_node::CTRL, "bn_is_minimize() -- not given a control node!");
    return toCInteger(((CTRL_node *)n)->get_toMinimize());
}

char *bn_get_name(bool_node *n) {
    return (char *)(n->get_name().c_str());
}

bool_node *bn_new(BooleanDAG *dag,
                  bool_node *mother,
                  bool_node *father,
                  BNType typ)
{
    return dag->new_node(mother, father, (bool_node::Type)typ);
}

bool_node *bn_clone(bool_node *n, int copyChildren) {
    return n->clone(fromCInteger(copyChildren));
}

void bn_set_const(bool_node *n, int value) {
    ((CONST_node*)n)->setVal(value);
}

  // bool_node* new_node(bool_node* mother, bool_node* father, bool_node::Type t);
