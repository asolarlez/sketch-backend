//
// Created by kliment on 12/20/21.
//

#include "File.h"
#include "SketchFunction.h"

void File::growInputs(VarStore & inputStore, BooleanDAG* dag){
    dag->growInputIntSizes();
    redeclareInputs(inputStore, dag);
}

void File::relabel(SketchFunction *harness) {
    SketchFunction* cloned_inlined_harness = harness->produce_inlined_dag();
    BooleanDAG* problem = cloned_inlined_harness->get_dag();
    VarStore input_store;
    redeclareInputsAndAngelics(input_store, problem);
    vector<bool_node*>& inputs = problem->getNodesByType(bool_node::SRC);

    for(int i = 0;i<size();i++)
    {
        at(i)->relabel(inputs);
    }

    cloned_inlined_harness->clear();
}

File::File(SketchFunction *harness, const string &file, FloatManager &floats, int seed) {
    generator = std::mt19937(seed);
    SketchFunction* cloned_inlined_harness = harness->produce_inlined_dag();
    BooleanDAG* problem = cloned_inlined_harness->get_dag();
    VarStore input_store;
    redeclareInputsAndAngelics(input_store, problem);
    vector<bool_node*>& inputs = problem->getNodesByType(bool_node::SRC);

    File::Result res = parseFile(file, floats, inputs, input_store);
    while (res == File::MOREBITS) {
        int at_int_size = problem->getIntSize();
        growInputs(input_store, problem);

        {
            for(auto it: harness->get_env()->functionMap) {
                assert(it.second->getIntSize() == at_int_size);
                it.second->growInputIntSizes();
            }
            assert(harness->get_dag()->getIntSize() == at_int_size);
            harness->get_dag()->growInputIntSizes();
        }

        res = parseFile(file, floats, inputs, input_store);
    }
    assert(res == File::DONE);
    used = vector<int>(size(), 0);
    cloned_inlined_harness->clear();
}

int File::get_used(int i) {
    return used[i];
}

void File::set_used(int i) {
    used[i]++;
    counterexample_ids_over_time.emplace_back(i);
}

File *File::produce_filter(std::function< bool(VarStore*) >& lambda_condition) {
    File* ret = new File(generator);
    for(int i = 0;i<size();i++)
    {
        if(lambda_condition(at(i)))
        {
            ret->push_back(at(i)->copy());
        }
    }
    ret->used = vector<int>(ret->size(), 0);
    return ret;
}

