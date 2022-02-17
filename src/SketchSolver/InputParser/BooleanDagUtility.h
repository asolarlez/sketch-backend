//
// Created by kliment on 2/9/22.
//

#ifndef SKETCH_SOURCE_BOOLEANDAGUTILITY_H
#define SKETCH_SOURCE_BOOLEANDAGUTILITY_H

#include "File.h"
#include "SkVal.h"
#include "BooleanDagLightUtility.h"


class BooleanDagUtility: public BooleanDagLightUtility {

    ProgramEnvironment* original_program_env = nullptr;

    InliningTree* inlining_tree = nullptr;

    bool has_been_concretized = false;

public:

    BooleanDagUtility(BooleanDAG* _root_dag): BooleanDagLightUtility(_root_dag) {}

    BooleanDagUtility(BooleanDAG* _root_dag, ProgramEnvironment* _env, ProgramEnvironment* _original_env, InliningTree* _inlining_tree, bool _has_been_concretized):
            BooleanDagLightUtility(_root_dag, _env),
            original_program_env(_original_env), inlining_tree(_inlining_tree), has_been_concretized(_has_been_concretized) {
        if(inlining_tree != nullptr) {
            inlining_tree = new InliningTree(this, inlining_tree);
            assert(inlining_tree->get_skfunc() == this);
        }
        if(has_been_concretized)
        {
//            assert(get_dag()->get_dag_id() != 31);
        }
    }

    BooleanDagUtility(BooleanDAG* _root_dag, ProgramEnvironment* _env, InliningTree* _inlining_tree, bool _has_been_concretized):
            BooleanDagLightUtility(_root_dag, _env), inlining_tree(_inlining_tree), has_been_concretized(_has_been_concretized) {
        if(inlining_tree != nullptr) {
            inlining_tree = new InliningTree(this, inlining_tree);
            assert(inlining_tree->get_skfunc() == this);
        }
    }

    BooleanDagUtility(BooleanDagUtility* to_copy): BooleanDagLightUtility(to_copy), inlining_tree(to_copy->inlining_tree), has_been_concretized(to_copy->has_been_concretized) {
        if(inlining_tree != nullptr) {
            inlining_tree = new InliningTree(this, inlining_tree);
            assert(inlining_tree->get_skfunc() == this);
        }

        if(has_been_concretized)
        {
//            assert(get_dag()->get_dag_id() != 31);
        }
    }

    void print_hole_names()
    {
        print_hole_names(cout);
    }

    void print_hole_names(ostream& out)
    {
        for(auto it:get_dag()->getNodesByType(bool_node::CTRL))
        {
            out << ((CTRL_node*)it)->get_name() << endl;
        }
    }

    void calc_inlining_tree()
    {
        assert(inlining_tree == nullptr);
        inlining_tree = new InliningTree(this);
        if(inlining_tree != nullptr) {
            assert(inlining_tree->get_skfunc() == this);
        }
    }

    void clear(const SolverLanguagePrimitives::HoleAssignment*& solution) {
        return BooleanDagLightUtility::clear(inlining_tree, solution);
    }

    virtual void clear() override {
        const SolverLanguagePrimitives::HoleAssignment* tmp_solution = nullptr;
        return BooleanDagLightUtility::clear(inlining_tree, tmp_solution);
    }

    bool soft_clear(const SolverLanguagePrimitives::HoleAssignment*& solution)
    {
        return BooleanDagLightUtility::soft_clear(inlining_tree, solution);
    }

    virtual bool soft_clear_assert_num_shared_ptr_is_0() override
    {
        assert(inlining_tree == nullptr);
        return BooleanDagLightUtility::soft_clear_assert_num_shared_ptr_is_0();
    }

    BooleanDagUtility* produce_inlined_dag(bool use_same_name = false)
    {
        BooleanDagUtility* ret = clone(use_same_name);
        ret->concretize_this_dag(nullptr, bool_node::CTRL);
        return ret;
    }

    BooleanDagUtility* clone(bool use_same_name = false) {
        BooleanDAG* new_dag = nullptr;
        if(use_same_name) {
            new_dag = get_dag()->clone(get_dag_name());
        }
        else {
            new_dag = get_dag()->clone();
        }
        return new BooleanDagUtility(new_dag, get_env(), original_program_env, inlining_tree, has_been_concretized);
    }

    BooleanDagUtility* produce_concretization(const VarStore* var_store, bool_node::Type var_type)
    {
        BooleanDagUtility* ret = clone();
        ret->concretize_this_dag(var_store, var_type);
        return ret;
    }

//    void inline_this_dag()
//    {
//        assert(inlining_tree == nullptr);
//        vector<string>* tmp = nullptr;
//        VarStore var_store;
//        concretize_this_dag(var_store, bool_node::CTRL, tmp);
//        if(tmp != nullptr){
//            tmp->clear();
//            delete tmp;
//        }
//    }

    void concretize_this_dag(const VarStore* var_store, bool_node::Type var_type)
    {
        vector<string>* tmp = nullptr;
        concretize_this_dag(var_store, var_type, tmp);
        if(tmp != nullptr){
            tmp->clear();
            delete tmp;
        }
    }

    void concretize_this_dag(const VarStore* var_store, bool_node::Type var_type, vector<string>*& inlined_functions) {
        assert(get_dag()->get_failed_assert() == nullptr);

        if(inlining_tree != nullptr) {
            assert(get_dag()->getNodesByType(bool_node::UFUN).empty());
        }
        else {
            assert(inlining_tree == nullptr);
            inlining_tree = new InliningTree(this);
        }
        if(inlining_tree != nullptr) {
            assert(inlining_tree->get_skfunc() == this);
        }

        bool is_being_concretized = false;
        if(var_type == bool_node::CTRL) {
            if(has_been_concretized)
            {
                assert(get_dag()->getNodesByType(bool_node::CTRL).empty());
                assert(get_dag()->getNodesByType(bool_node::UFUN).empty());
            }
            else
            {
                if(var_store != nullptr) {
                    is_being_concretized = true;
                    if(var_store->size() == 0) {
                        assert(inlining_tree->has_no_holes());
                    }
                }
                else {
                    if (inlining_tree->has_no_holes()) {
                        is_being_concretized = true;
                    }
                    assert(inlining_tree!= nullptr);
                }
            }
            if(is_being_concretized) {
                assert(!has_been_concretized);
            }
        }

        if(var_store == nullptr)
        {
            var_store = new VarStore();
        }

        BooleanDagLightUtility::concretize_this_dag(var_store, var_type, inlined_functions);

        if(is_being_concretized) {
            has_been_concretized = true;
        }
    }

    void swap_env(ProgramEnvironment *new_env);

    void reset_env_to_original();

    bool env_was_swapped() {
        return original_program_env != nullptr;
    }

    void hard_swap_env(ProgramEnvironment* new_env) {
        assert(original_program_env == nullptr);
        get_env_ref() = new_env;
    }

    bool is_inlining_tree_nonnull();

    InliningTree *& get_inlining_tree(bool assert_nonnull = true);

    bool get_has_been_concretized();

    void clear_inlining_tree();
};

#endif //SKETCH_SOURCE_BOOLEANDAGUTILITY_H
