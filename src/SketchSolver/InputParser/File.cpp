//
// Created by kliment on 12/20/21.
//

#include "File.h"
#include "BooleanDAG.h"

void File::growInputs(VarStore & inputStore, BooleanDAG* dag){
    dag->growInputIntSizes();
    redeclareInputs(inputStore, dag);
}

File::File(BooleanDAG *dag, const string &file, FloatManager &floats, int seed) {
    generator = std::mt19937(seed);
    BooleanDAG* problem = dag;
    VarStore input_store;
    redeclareInputsAndAngelics(input_store, problem);
    vector<bool_node*>& inputs = problem->getNodesByType(bool_node::SRC);

    File::Result res = parseFile(file, floats, inputs, input_store);
    while (res == File::MOREBITS) {
        growInputs(input_store, problem);
        res = parseFile(file, floats, inputs, input_store);
    }
    assert(res == File::DONE);
    used = vector<int>(size(), 0);
}

int File::get_used(int i) {
    return used[i];
}

void File::set_used(int i) {
    used[i]++;
    counterexample_ids_over_time.emplace_back(i);
}
