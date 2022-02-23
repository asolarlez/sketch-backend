//
// Created by kliment on 2/9/22.
//

#ifndef SKETCH_SOURCE_BOOLEANDAGUTILITY_H
#define SKETCH_SOURCE_BOOLEANDAGUTILITY_H

#include "File.h"
#include "BooleanDagLightUtility.h"


class BooleanDagUtility: public BooleanDagLightUtility {

    ProgramEnvironment* original_program_env = nullptr;

    LightInliningTree* inlining_tree = nullptr;

    bool has_been_concretized = false;
    VarStore* var_store_used_for_concretization = nullptr;

public:

    vector<string> get_deep_holes() const;

    const VarStore* get_var_store() const {
        return var_store_used_for_concretization;
    }

    BooleanDagUtility(BooleanDAG* _root_dag): BooleanDagLightUtility(_root_dag) {}

    BooleanDagUtility(BooleanDAG* _root_dag, ProgramEnvironment* _env, ProgramEnvironment* _original_env, const LightInliningTree* _inlining_tree, bool _has_been_concretized):
            BooleanDagLightUtility(_root_dag, _env),
            original_program_env(_original_env), has_been_concretized(_has_been_concretized) {
        if(_inlining_tree != nullptr) {
            inlining_tree = new LightInliningTree(this, _inlining_tree);
            inlining_tree->get_solution();
            assert(inlining_tree->get_dag_id()  == get_dag_id());
        }
        if(has_been_concretized)
        {
//            assert(get_dag()->get_dag_id() != 31);
        }
    }

    BooleanDagUtility(BooleanDAG* _root_dag, ProgramEnvironment* _env, const LightInliningTree* _inlining_tree, bool _has_been_concretized):
            BooleanDagLightUtility(_root_dag, _env), has_been_concretized(_has_been_concretized) {
        if(_inlining_tree != nullptr) {
            inlining_tree = new LightInliningTree(this, _inlining_tree);
            assert(inlining_tree->get_dag_id()  == get_dag_id());
        }
    }

    BooleanDagUtility(BooleanDagUtility* to_copy): BooleanDagLightUtility(to_copy), inlining_tree(to_copy->inlining_tree), has_been_concretized(to_copy->has_been_concretized) {
        if(inlining_tree != nullptr) {
            inlining_tree = new LightInliningTree(this, inlining_tree);
            assert(inlining_tree->get_dag_id()  == get_dag_id());
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

//    void calc_inlining_tree()
//    {
//        assert(inlining_tree == nullptr);
//        inlining_tree = new LightInliningTree(this);
//        if(inlining_tree != nullptr) {
//            assert(inlining_tree->get_dag_id()  == get_dag_id());
//        }
//    }

    virtual void clear() override {
        return BooleanDagLightUtility::clear(inlining_tree);
    }

    bool soft_clear() {
        return BooleanDagLightUtility::soft_clear(inlining_tree);
    }

    virtual bool soft_clear_assert_num_shared_ptr_is_0() override
    {
        assert(inlining_tree == nullptr);
        return BooleanDagLightUtility::soft_clear_assert_num_shared_ptr_is_0();
    }

    BooleanDagUtility* produce_inlined_dag(bool use_same_name = false)
    {
        BooleanDagUtility* ret = unit_clone(use_same_name);
        ret->concretize_this_dag(nullptr, bool_node::CTRL);
        return ret;
    }

    BooleanDagUtility* unit_clone(bool use_same_name = false) {
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
        BooleanDagUtility* ret = unit_clone();
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

    void concretize_this_dag(const VarStore* const _var_store, bool_node::Type var_type, vector<string>*& inlined_functions) {
        assert(get_dag()->get_failed_assert() == nullptr);

        if(_var_store != nullptr)
        {
            assert(_var_store->get_inlining_tree() != nullptr);
            _var_store->check_rep();
        }

        if(inlining_tree != nullptr) {
            assert(get_dag()->getNodesByType(bool_node::UFUN).empty());
        }
        else {
            assert(inlining_tree == nullptr);
            inlining_tree = new LightInliningTree(this, _var_store);
            inlining_tree->get_solution();
        }
        if(inlining_tree != nullptr) {
            assert(inlining_tree->get_dag_id()  == get_dag_id());
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
                if(_var_store != nullptr) {
                    is_being_concretized = true;
                    if(_var_store->size() == 0) {
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

        vector<string> local_hole_names;
        for(auto it: get_dag()->getNodesByType(bool_node::CTRL)) {
            if(it->get_name() != "#PC") {
                assert(_var_store->contains(it->get_name()));
                local_hole_names.push_back(it->get_name());
            }
        }

        BooleanDagLightUtility::concretize_this_dag(_var_store, var_type, inlined_functions);

        if(is_being_concretized) {
            assert(!has_been_concretized);
//            if(get_dag_id() == 33)
//            {
//                cout << "here" << endl;
//            }
            has_been_concretized = true;
            assert(var_store_used_for_concretization == nullptr);
            if(_var_store != nullptr) {
                var_store_used_for_concretization = new VarStore();
                for(int i = 0; i< local_hole_names.size();i++)
                {
                    string hole_name = local_hole_names[i];
                    if(hole_name != "#PC") {
                        var_store_used_for_concretization->insertObj(hole_name, var_store_used_for_concretization->size(), VarStore::objP(
                                _var_store->getObjConst(hole_name)));
                    }
                }
            }
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

    const LightInliningTree * get_inlining_tree(bool assert_nonnull = true) const;
    LightInliningTree * get_inlining_tree_non_const(bool assert_nonnull = true) const;

    bool get_has_been_concretized() const ;
    bool get_has_been_inlined() const ;

    void clear_inlining_tree();
};

#endif //SKETCH_SOURCE_BOOLEANDAGUTILITY_H
