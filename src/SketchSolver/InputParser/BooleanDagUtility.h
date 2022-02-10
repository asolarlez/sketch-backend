//
// Created by kliment on 2/9/22.
//

#ifndef SKETCH_SOURCE_BOOLEANDAGUTILITY_H
#define SKETCH_SOURCE_BOOLEANDAGUTILITY_H

#include "ProgramEnvironment.h"
#include "File.h"

static bool new_way = true;


static SkValType bool_node_out_type_to_sk_val_type(OutType* out_type)
{
    assert(out_type == OutType::INT || out_type == OutType::BOOL || OutType::FLOAT);
    if(out_type == OutType::INT)
    {
        return sk_type_int;
    }
    else if(out_type == OutType::BOOL)
    {
        return sk_type_bool;
    }
    else if(out_type == OutType::FLOAT)
    {
        return sk_type_float;
    }
    else
    {
        assert(false);
    }
}

class BooleanDagUtility {
    BooleanDAG* const root_dag = nullptr;
    ProgramEnvironment* env = nullptr;
    int shared_ptr = 0;

    ProgramEnvironment* original_program_env = nullptr;

    bool at_least_inlined = false;

protected:
    const string& dag_name;
public:

    BooleanDagUtility(BooleanDAG* _root_dag):
            root_dag(_root_dag), dag_name(_root_dag->get_name()) {
        assert(root_dag != nullptr);
        AssertDebug(env != nullptr, "env needs to be defined.");
    }

    BooleanDagUtility(BooleanDAG* _root_dag, ProgramEnvironment* _env, ProgramEnvironment* _original_env = nullptr, bool _at_least_inlined = false):
        root_dag(_root_dag), env(_env), dag_name(_root_dag->get_name()), original_program_env(_original_env), at_least_inlined(_at_least_inlined) {
        assert(root_dag != nullptr);
    }

    BooleanDagUtility(BooleanDagUtility* to_copy): root_dag(to_copy->root_dag->clone()), env(to_copy->env), dag_name(to_copy->dag_name), at_least_inlined(to_copy->at_least_inlined) {
        assert(root_dag != nullptr);
    }

    void print_hole_names()
    {
        for(auto it:get_dag()->getNodesByType(bool_node::CTRL))
        {
            cout << it->get_name() << endl;
        }
    }

    vector<SkHoleSpec>* get_holes()
    {
        BooleanDagUtility* inlined_harness = produce_inlined_dag();
        auto ctrl_nodes = inlined_harness->get_dag()->getNodesByType(bool_node::CTRL);
        auto* ret = new vector<SkHoleSpec>();
        for(auto & ctrl_node : ctrl_nodes)
        {
            ret->push_back(
                    SkHoleSpec(
                            ctrl_node->get_name(),
                            bool_node_out_type_to_sk_val_type(ctrl_node->getOtype())));
        }
        inlined_harness->clear();
        return ret;
    }

    BooleanDAG* get_dag() {
        return root_dag;
    }

    int get_num_holes()
    {
        return get_dag()->getNodesByType(bool_node::CTRL).size();
    }

    const string& get_dag_name()
    {
        return dag_name;
    }

    ProgramEnvironment* get_env() {
        return env;
    }

    BooleanDagUtility* produce_inlined_dag(bool use_same_name = false)
    {
        VarStore var_store;
        BooleanDagUtility* ret = clone(use_same_name);
        ret->concretize_this_dag(var_store, bool_node::CTRL);
        return ret;
    }

    BooleanDagUtility* clone(bool use_same_name = false) {
        BooleanDAG* new_dag = nullptr;
        if(use_same_name) {
            new_dag = get_dag()->clone(dag_name);
        }
        else {
            new_dag = get_dag()->clone();
        }
        return new BooleanDagUtility(new_dag, env, original_program_env, at_least_inlined);
    }

    BooleanDagUtility* produce_concretization(VarStore& var_store, bool_node::Type var_type)
    {
        BooleanDagUtility* ret = clone();
        ret->concretize_this_dag(var_store, var_type);
        return ret;
    }


    void inline_this_dag()
    {
        assert(!at_least_inlined);
        vector<string>* tmp = nullptr;
        VarStore var_store;
        concretize_this_dag(var_store, bool_node::CTRL, tmp);
        if(tmp != nullptr){
            tmp->clear();
            delete tmp;
        }
    }

    void concretize_this_dag(VarStore& var_store, bool_node::Type var_type)
    {
        vector<string>* tmp = nullptr;
        concretize_this_dag(var_store, var_type, tmp);
        if(tmp != nullptr){
            tmp->clear();
            delete tmp;
        }
    }

    void concretize_this_dag(VarStore& var_store, bool_node::Type var_type, vector<string>*& inlined_functions) {
        assert(!get_dag()->get_failed_assert());
        at_least_inlined = true;
        if (new_way) {
            env->doInline(*root_dag, var_store, var_type, inlined_functions);
        } else {
            hardCodeINodeNoClone(root_dag, var_store, var_type, env->get_floats());
            inlined_functions = nullptr;
        }
    }


    int count_passing_inputs(File* file) {
        int ret = 0;
        int num_0s = 0;
        int num_1s = 0;
        for(int i = 0;i<file->size();i++)
        {
            BooleanDagUtility* _dag = produce_concretization(*file->at(i), bool_node::SRC);
            _dag->increment_shared_ptr();
            auto dag = _dag->get_dag();
            assert(dag->getNodesByType(bool_node::CTRL).size() == 0);
            assert((dag->size() == 0) == (dag->get_failed_assert() == nullptr));
            if(dag->get_failed_assert() == nullptr) {
                ret += 1;
            }
            _dag->clear();
        }
        return ret;
    }

    virtual void clear() {
        if(soft_clear()){
            assert(shared_ptr == 0);
            delete this;
        }
        else {
            assert(shared_ptr >= 1);
        }
    }

    bool soft_clear()
    {
        shared_ptr--;
        assert(shared_ptr>=0);
        if(shared_ptr == 0) {
            bool ret = soft_clear_assert_num_shared_ptr_is_0();
            assert(ret);
            return ret;
        }
        else {
            return false;
        }
    }


    bool soft_clear_assert_num_shared_ptr_is_0();

    void increment_shared_ptr() {
        assert(shared_ptr >= 0);
        shared_ptr++;
    }


    void decrement_shared_ptr_wo_clear() {
        assert(shared_ptr >= 1);
        shared_ptr--;
    }

    int get_num_shared_ptr() const
    {
        assert(shared_ptr >= 0);
        return shared_ptr;
    }

    void swap_env(ProgramEnvironment *new_env);

    void reset_env_to_original();

    bool env_was_swapped() {
        return original_program_env != nullptr;
    }

    void hard_swap_env(ProgramEnvironment* new_env)
    {
        assert(original_program_env == nullptr);
        env = new_env;
    }

    bool has_it_been_at_least_inlined();
};

#include "BooleanNodes.h"
#include "VarStore.h"
#include "SkVal.h"
#include "File.h"
#include "ProgramEnvironment.h"
#include "FunctionMapTransformerLanguage.h"

#endif //SKETCH_SOURCE_BOOLEANDAGUTILITY_H
