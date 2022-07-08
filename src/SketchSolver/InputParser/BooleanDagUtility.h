//
// Created by kliment on 2/9/22.
//

#ifndef SKETCH_SOURCE_BOOLEANDAGUTILITY_H
#define SKETCH_SOURCE_BOOLEANDAGUTILITY_H

#include "BooleanDagLightUtility.h"


class BooleanDagUtility: public BooleanDagLightUtility {

    ProgramEnvironment* original_program_env = nullptr;

    LightInliningTree* inlining_tree = nullptr;

    VarStore* var_store_used_for_concretization = nullptr;

public:

    vector<string> get_deep_holes() const;

    const VarStore* get_var_store() const {
        return var_store_used_for_concretization;
    }

    BooleanDagUtility(BooleanDAG* _root_dag): BooleanDagLightUtility(_root_dag) {}

    BooleanDagUtility(BooleanDAG* _root_dag, ProgramEnvironment* _env, ProgramEnvironment* _original_env, const LightInliningTree* _inlining_tree, bool _has_been_concretized):
            BooleanDagLightUtility(_root_dag, _env, _has_been_concretized),
            original_program_env(_original_env) {
        if(_inlining_tree != nullptr) {
            inlining_tree = new LightInliningTree(this, _inlining_tree);
            inlining_tree->get_solution();
            assert(inlining_tree->get_dag_id()  == get_dag_id());
        }
    }

    BooleanDagUtility(BooleanDAG* _root_dag, ProgramEnvironment* _env, const LightInliningTree* _inlining_tree, bool _has_been_concretized):
            BooleanDagLightUtility(_root_dag, _env, _has_been_concretized) {
        if(_inlining_tree != nullptr) {
            inlining_tree = new LightInliningTree(this, _inlining_tree);
            assert(inlining_tree->get_dag_id()  == get_dag_id());
        }
    }

    BooleanDagUtility(BooleanDagUtility* to_copy): BooleanDagLightUtility(to_copy), inlining_tree(to_copy->inlining_tree) {
        if(inlining_tree != nullptr) {
            inlining_tree = new LightInliningTree(this, inlining_tree);
            assert(inlining_tree->get_dag_id()  == get_dag_id());
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



//    BooleanDagUtility* produce_inlined_dag(bool use_same_name = false)
//    {
//        BooleanDagUtility* ret = unit_clone(use_same_name);
//        ret->concretize_this_dag(nullptr, bool_node::CTRL);
//        return ret;
//    }

//    BooleanDagUtility* unit_clone(bool use_same_name = false) {
//        BooleanDAG* new_dag = nullptr;
//        if(use_same_name) {
//            new_dag = get_dag()->clone(get_dag_name());
//        }
//        else {
//            new_dag = get_dag()->clone();
//        }
//        return new BooleanDagUtility(new_dag, get_env(), original_program_env, inlining_tree, has_been_concretized);
//    }

//    BooleanDagUtility* produce_concretization(const VarStore* var_store, bool_node::Type var_type)
//    {
//        BooleanDagUtility* ret = unit_clone();
//        ret->concretize_this_dag(var_store, var_type);
//        return ret;
//    }


//    BooleanDagUtility* deep_clone(bool only_tail)
//    {
//
//        //first get all inlined functions
//        //clone all inlined functions;
//        //replace the name inside the inlined function with eachother's
//
//        //assert that all the ufuns are represented in the function map
//        for (const auto& _it: get_env()->function_map) {
//            BooleanDagUtility* it_second = (BooleanDagUtility*)_it.second;
//            for (auto ufun_it: it_second->get_dag()->getNodesByType(bool_node::UFUN)) {
//                auto f = get_env()->function_map.find(((UFUN_node *) ufun_it)->get_ufun_name());
//                assert(f != get_env()->function_map.end());
//            }
//        }
//
//        set<string>* inlined_functions = get_inlined_functions();
//
//        assert(inlined_functions != nullptr);
//
//        //assert that all that the previous operation hasn't corrupted the function map ufuns invariant
//        for (const auto& _it: get_env()->function_map) {
//            BooleanDagUtility* it_second = (BooleanDagUtility*)_it.second;
//            for (auto ufun_it: it_second->get_dag()->getNodesByType(bool_node::UFUN)) {
//                auto f = get_env()->function_map.find(((UFUN_node *) ufun_it)->get_ufun_name());
//                assert(f != get_env()->function_map.end());
//            }
//        }
//
//        // assert that all the dags in the inlined functions have all the ufuns also in the inlined functions
//        for (const auto &inlined_f_name: *inlined_functions) {
//            auto it_f = get_env()->function_map.find(inlined_f_name);
//            assert(it_f != get_env()->function_map.end());
//            BooleanDagUtility* it_f_second = (BooleanDagUtility*)it_f->second;
//            for (auto ufun_it: it_f_second->get_dag()->getNodesByType(bool_node::UFUN)) {
//                auto f = get_env()->function_map.find(((UFUN_node *) ufun_it)->get_ufun_name());
//                assert(f != get_env()->function_map.end());
//                assert(inlined_functions->find(((UFUN_node *) ufun_it)->get_ufun_name()) != inlined_functions->end());
//            }
//        }
//
//        BooleanDagUtility* clone_of_this = nullptr;
//
//        bool entered_recursive_case = false;
//        map<string, BooleanDagUtility *> to_inline_skfuncs;
//        for (const string &_inlined_function_name: *inlined_functions) {
//            assert(get_env()->function_map.find(_inlined_function_name) != get_env()->function_map.end());
//            assert(to_inline_skfuncs.find(_inlined_function_name) == to_inline_skfuncs.end());
//            if(only_tail) {
//                if (_inlined_function_name != get_dag_name()) {
//                    const string &to_clone_fname = _inlined_function_name;
//                    to_inline_skfuncs[to_clone_fname] = get_env()->function_map[to_clone_fname]->unit_clone_and_insert_in_function_map();
//                } else {
//                    assert(_inlined_function_name == get_dag_name());
//                    entered_recursive_case = true;
//                    to_inline_skfuncs[_inlined_function_name] = get_env()->function_map[_inlined_function_name];
//                }
//            }
//            else {
//                const string &to_clone_fname = _inlined_function_name;
//                if(get_dag_name() == _inlined_function_name)
//                    assert(get_env()->function_map[to_clone_fname] == this);
//                to_inline_skfuncs[to_clone_fname] = get_env()->function_map[to_clone_fname]->unit_clone_and_insert_in_function_map();
//                if (_inlined_function_name == get_dag_name()) {
//                    entered_recursive_case = true;
//                    clone_of_this = to_inline_skfuncs[to_clone_fname];
//                }
//            }
//        }
//
//        if(entered_recursive_case) {
//            assert(to_inline_skfuncs.find(get_dag_name()) != to_inline_skfuncs.end());
//        }
//        else {
//            assert(to_inline_skfuncs.find(get_dag_name()) == to_inline_skfuncs.end());
//        }
//
//        assert(to_inline_skfuncs.size() == inlined_functions->size());
//
//        for (const auto &it: *inlined_functions) {
//            assert(to_inline_skfuncs.find(it) != to_inline_skfuncs.end());
//        }
//
//        delete inlined_functions;
//        inlined_functions = nullptr;
//
//        if (to_inline_skfuncs.find(get_dag_name()) != to_inline_skfuncs.end()) {
//            assert(entered_recursive_case);
//            if(only_tail) {
//                assert(to_inline_skfuncs[get_dag_name()] == this);
//            }
//        } else {
//            assert(!entered_recursive_case);
//            assert(clone_of_this == nullptr);
//            if(only_tail)
//            {
//                to_inline_skfuncs[get_dag_name()] = this;
//            }
//            else {
//                const string &to_clone_fname = get_dag_name();
//                assert(to_inline_skfuncs.find(to_clone_fname) == to_inline_skfuncs.end());
//                to_inline_skfuncs[to_clone_fname] = unit_clone_and_insert_in_function_map();
//                clone_of_this = to_inline_skfuncs[to_clone_fname];
//            }
//        }
//
//        assert(to_inline_skfuncs.find(get_dag_name()) != to_inline_skfuncs.end());
//
//        if(only_tail) {
//            assert(clone_of_this == nullptr);
//        }
//
//        //rename all the ufuns
//        for (const auto& skfunc_it: to_inline_skfuncs) {
//            set<pair<string, string> > ufnames;
//
//            for (auto ufun_it: skfunc_it.second->get_dag()->getNodesByType(bool_node::UFUN)) {
//                string ufname = ((UFUN_node *) ufun_it)->get_ufun_name();
//                string original = ((UFUN_node *) ufun_it)->get_original_ufname();
//                ufnames.insert(make_pair(ufname, original));
//            }
//
//            for (const pair<string, string> &ufname_now_original: ufnames) {
//                string ufname = ufname_now_original.first;
//                string original = ufname_now_original.second;
//
//                assert(to_inline_skfuncs[ufname]->get_env()->function_map.find(to_inline_skfuncs[ufname]->get_dag_name()) !=
//                       to_inline_skfuncs[ufname]->get_env()->function_map.end());
//
//                if (to_inline_skfuncs.find(ufname) == to_inline_skfuncs.end()) {
//                    AssertDebug(false, "this should fail here, it was checked before");
//                } else {
//                    SketchFunction* skfunc = skfunc_it.second;
//                    skfunc->replace(original, to_inline_skfuncs[ufname]->get_dag_name());
//                }
//            }
//        }
//
//        if(only_tail) {
//            assert(clone_of_this == nullptr);
//        }
//
//        return clone_of_this;
//    }


    void inline_this_dag(bool assert_not_inlined = true)
    {
        if(assert_not_inlined) {
            assert(inlining_tree == nullptr);
        }
        if(inlining_tree == nullptr) {
            concretize_this_dag(nullptr, bool_node::CTRL);
        }
    }

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
        }

        if(inlining_tree != nullptr) {
            assert(get_dag()->getNodesByType(bool_node::UFUN).empty());
        }
        else {
            assert(inlining_tree == nullptr);
//            if(get_dag_name() == "sketch_main__Wrapper__id360__id376")
//            {
//                cout << "HERE" << endl;
//            }
            inlining_tree = new LightInliningTree(this, _var_store);
            inlining_tree->get_solution();
        }
        if(inlining_tree != nullptr) {
            assert(inlining_tree->get_dag_id()  == get_dag_id());
        }

        bool is_being_concretized = false;
        if(var_type == bool_node::CTRL) {
            if(get_has_been_concretized())
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
                assert(!get_has_been_concretized());
            }
        }

        if(is_being_concretized) {
            vector<string> local_hole_names;
            for (auto it: get_dag()->getNodesByType(bool_node::CTRL)) {
                if (it->get_name() != "#PC") {
                    assert(_var_store != nullptr);
                    assert(_var_store->contains(it->get_name()));
                    local_hole_names.push_back(it->get_name());
                }
            }

            assert(!get_has_been_concretized());
            BooleanDagLightUtility::concretize_this_dag(_var_store, var_type, inlined_functions);

            assert(var_store_used_for_concretization == nullptr);
            if(_var_store != nullptr) {
                var_store_used_for_concretization = new VarStore();
                for(int i = 0; i< local_hole_names.size();i++)
                {
                    string hole_name = local_hole_names[i];
                    if(hole_name != "#PC") {
                        var_store_used_for_concretization->insertObj(hole_name, var_store_used_for_concretization->size(), objP(
                                _var_store->getObjConst(hole_name)));
                    }
                }
            }
        }
        else {
            //only inlining; not concretizing;
            assert(_var_store == nullptr);
            BooleanDagLightUtility::concretize_this_dag(_var_store, var_type, inlined_functions);
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

    bool get_has_been_inlined() const ;

    void clear_inlining_tree();
};

#endif //SKETCH_SOURCE_BOOLEANDAGUTILITY_H
