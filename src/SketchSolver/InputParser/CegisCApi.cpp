
#include "CegisCApi.h"
#include "BooleanNodes.h"

struct NodeVector {
    vector<bool_node*> *vec;
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

NodeVector *get_nodes_by_type(BNType t) {
    return NULL;
}
