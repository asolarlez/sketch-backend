//
// Created by kliment on 12/20/21.
//

#include "BooleanDagLightUtility.h"
#include "File.h"
#include "BooleanDAG.h"

void File::growInputs(VarStore & inputStore, BooleanDAG* dag){
    dag->growInputIntSizes();
    redeclareInputs(inputStore, dag);
}

void File::relabel(BooleanDagLightUtility *harness) {
    BooleanDagLightUtility* cloned_inlined_harness = harness->produce_inlined_dag();
    cloned_inlined_harness->increment_shared_ptr();
    BooleanDAG* problem = cloned_inlined_harness->get_dag();
    VarStore input_store;
    redeclareInputsAndAngelics(input_store, problem);
    auto inputs = problem->getNodesByType(bool_node::SRC);

    for(int i = 0;i<size();i++)
    {
        at(i)->relabel(inputs);
    }

    cloned_inlined_harness->clear();
}

File::File(BooleanDagLightUtility *harness, const string &file, FloatManager &floats, int seed) {
    generator = std::mt19937(seed);
//    SketchFunction* cloned_inlined_harness = harness->produce_concretization();
    BooleanDAG* problem = harness->get_dag()->clone();
    harness->get_env()->doInline(*problem);
//    cloned_inlined_harness->get_dag();
    VarStore input_store;
    redeclareInputsAndAngelics(input_store, problem);
    auto inputs = problem->getNodesByType(bool_node::SRC);

    File::Result res = parseFile(file, floats, inputs, input_store);
    const int max_num_bits = 64;

    const map<string, BooleanDAG *> * bool_dag_map = harness->get_env()->function_map.to_boolean_dag_map();
    while (res == File::MOREBITS) {
        int at_int_size = problem->getIntSize();
        AssertDebug(at_int_size < max_num_bits, "TOO MANY BITS, CHECK THE INPUT TYPES OF YOUR HARNESS. OTHERWISE PROBABLY WRONG CODE/INPUT/OUTPUT.");
        growInputs(input_store, problem);
        if(true){
            assert(harness->get_dag()->getIntSize() == at_int_size);
            bool harness_in_function_map = false;
            for(auto it: *bool_dag_map) {
                assert(it.second->getIntSize() == at_int_size);
                it.second->growInputIntSizes();
                if(it.second->get_name() == harness->get_dag()->get_name()) {
                    assert(!harness_in_function_map);
                    assert(it.second == harness->get_dag());
                    harness_in_function_map = true;
                }
            }
            if(!harness_in_function_map)
            {
                assert(harness->get_dag()->getIntSize() == at_int_size);
                harness->get_dag()->growInputIntSizes();
            }
            else
            {
                assert(harness->get_dag()->getIntSize() == at_int_size+1);
            }
        }

        res = parseFile(file, floats, inputs, input_store);
    }
    assert(res == File::DONE);
#ifdef CHECK_FILE_INVARIANT
    used = vector<int>(size(), 0);
#endif
    delete bool_dag_map;
    bool_dag_map = nullptr;
    problem->clear();
}

#ifdef CHECK_FILE_INVARIANT
int File::get_used(int i) {
    return used[i];
}

void File::set_used(int i) {
    used[i]++;
    counterexample_ids_over_time.emplace_back(i);
}

#endif

File *File::produce_filter(std::function< bool(VarStore*) >& lambda_condition) {
    File* ret = new File(generator);
    for(int i = 0;i<size();i++)
    {
        if(lambda_condition(at(i)))
        {
            ret->push_back(at(i)->clone());
        }
    }
#ifdef CHECK_FILE_INVARIANT
    ret->used = vector<int>(ret->size(), 0);
#endif
    return ret;
}

File::File() {

}

