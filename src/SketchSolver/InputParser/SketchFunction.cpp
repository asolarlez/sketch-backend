//
// Created by kliment on 12/20/21.
//

#include "SketchFunction.h"
#include "BooleanDagUtility.h"

#include <utility>

const bool rename_holes = true;

SketchFunction *SketchFunction::produce_concretization(
        const VarStore* _var_store, const bool_node::Type var_type, const bool do_clone, const bool do_deep_clone_tail, const bool do_recursive_concretize) {

    assert(do_clone || do_deep_clone_tail);
    assert(do_recursive_concretize);

    VarStore* var_store = nullptr;
    if(_var_store != nullptr) {
        var_store = new VarStore(*_var_store, true);
    }

    SketchFunction* ret = nullptr;

    if(do_clone) {
        assert(do_deep_clone_tail);
        ret = deep_clone();
//        SketchFunction* the_clone = unit_clone();
    }
    else {
        ret = this;
        if (do_deep_clone_tail) {
            deep_clone_tail();
        }
    }

    ret->increment_shared_ptr();

    ret->_inplace_recursive_concretize(var_store, var_type, do_recursive_concretize);

    ret->decrement_shared_ptr_wo_clear();

    if(var_store != nullptr) {
        var_store->clear();
    }

    return ret;

}

void SketchFunction::_inplace_recursive_concretize(
        VarStore* var_store, const bool_node::Type var_type, bool do_recursive_concretize) {
    if (do_recursive_concretize) {
        if (var_type == bool_node::CTRL) {
            LightInliningTree *tmp_inlining_tree = new LightInliningTree(this);

            set<string> *subf_names = get_inlined_functions();

            //for every subfunction
            for (const auto &f_name: *subf_names) {
                auto target = tmp_inlining_tree->get_target(f_name);
                assert(get_env()->function_map.find(f_name) != get_env()->function_map.end());
                SketchFunction *subf = get_env()->function_map.find(f_name)->second;

                //for every non-concretized hole in the subfunction
                for (auto it: subf->get_dag()->getNodesByType(bool_node::CTRL)) {
                    if (it->get_name() != "#PC") {
                        if (target->get_var_store() != nullptr) {
                            //a concratization must not exists
                            assert(false);
                            assert(!target->get_var_store()->contains(it->get_name()));
                        } else {
                            //if a concretization doesn't exist
                            //make sure that the rep knows that the hole is unconcretized
                            string org_name = ((CTRL_node *) it)->get_original_name();
                            assert(target->get_unconc_map().find(org_name) != target->get_unconc_map().end());
                            assert(target->get_unconc_map().at(org_name).find(f_name) !=
                                   target->get_unconc_map().at(org_name).end());
                            assert(target->get_unconc_map().at(org_name).at(f_name) == it->get_name());
                        }
                    }
                }
            }

            if (var_store != nullptr) {
                assert(tmp_inlining_tree->match_topology(var_store->get_inlining_tree()));
                tmp_inlining_tree->rename_var_store(*var_store);
                for (const auto &it: get_deep_holes()) {
                    AssertDebug(var_store->contains(it), "MISSING VALUE FOR HOLE: " + it + ".");
                }
            }

            tmp_inlining_tree->concretize(this, var_store);
            tmp_inlining_tree->clear();
        }
    }
    else {
        //assert all holes are represented in var_store.
        for (const auto &it: get_deep_holes()) {
            AssertDebug(var_store->contains(it), "MISSING VALUE FOR HOLE: " + it + ".");
        }
    }

    _inplace_concretize__assert_subfuncts_are_concretized(var_store, var_type);

}

SketchFunction *SketchFunction::_inplace_concretize__assert_subfuncts_are_concretized(
        const VarStore* var_store, const bool_node::Type var_type)
{
    assert(!get_has_been_concretized());

    for(const auto& assignment: get_unit_ufuns_map()) {
        auto it = assignment.second;
        assert(get_env()->function_map.find(it) != get_env()->function_map.end());
        if(it != get_dag_name()) {
            AssertDebug(get_env()->function_map[it]->get_has_been_concretized(),
                        "INVARIANT NOT SATISFIED: NEED ALL SUBFUNCTIONS TO BE CONCRETIZED. "
                        "USED TO RESTRICT VAR STORE TO ONLY THE THIS SUBFUNCTION FOR PRINTING PURPOSES.");
        }
    }

    for(const auto& it:dependencies) {
        bool found = false;
        for(auto ufun_node : get_dag()->getNodesByType(bool_node::UFUN)) {
            if(it.first == ((UFUN_node *) ufun_node)->get_ufun_name()) {
                found = true;
                break;
            }
        }
        if(!found) {
            if(it.first == get_dag_name()) {
                AssertDebug(found, "THE SELF IS IT'S OWN DEPENDENCY, WHICH IS FINE. THE REASON FOR THIS ASSERT IS BECAUSE AS OF THIS MOMENT, THE SELF IS ONLY A DEPENDENCY IF IT HAS A UFUN POINTING TO ITSELF.")
            }
        }
        AssertDebug(found, "THERE ARE DEPENDENCIES THAT ARE UNNECESSARY.");
    }

    vector<string> unit_holes = get_unit_holes();

    vector<string> *inlined_functions = nullptr;
    concretize_this_dag(var_store, var_type, inlined_functions);
    assert(inlined_functions != nullptr);

    rep = get_env()->function_map.concretize(
            get_dag_name(), var_store->produce_restrict(unit_holes), var_type, inlined_functions);
    if(!get_dag()->getNodesByType(bool_node::UFUN).empty() || !get_dag()->getNodesByType(bool_node::CTRL).empty()) {
        assert(get_dag()->get_failed_assert() != nullptr);
    }
    delete inlined_functions;

    bool remove_unnecessary_dependencies = false;
    if(remove_unnecessary_dependencies)
    {
        // remove unnecessary dependencies. (these are the inlined function that were previously dependencies, but now theyare not.
        //BUT! these dependencies are necessary if you want to access the subdags after make_executable.
        vector<string> to_erase_from_dependencies;
        for (const auto &it: dependencies) {
            assert(!it.first.empty());
            bool found = false;
//            if (it.first == get_dag_name()) {
//                found = true;
//            } else
            {
                for (auto ufun_node: get_dag()->getNodesByType(bool_node::UFUN)) {
                    if (it.first == ((UFUN_node *) ufun_node)->get_ufun_name()) {
                        found = true;
                        break;
                    }
                }
            }
            if (!found) {
                to_erase_from_dependencies.push_back(it.first);
            }
        }
        for(const auto& it: to_erase_from_dependencies) {
            if(it == get_dag_name()) {
                increment_shared_ptr();
            }
            dependencies.erase(it);
        }
        for(const auto& it:dependencies) {
            bool found = false;
            for(auto ufun_node : get_dag()->getNodesByType(bool_node::UFUN)) {
                if(it.first == ((UFUN_node *) ufun_node)->get_ufun_name()) {
                    found = true;
                    break;
                }
            }
            AssertDebug(found, "THERE ARE DEPENDENCIES THAT ARE UNNECESSARY.");
        }
    }


    return this;
}

SketchFunction *SketchFunction::unit_clone_and_insert_in_function_map() {
    SketchFunction *ret_clone = unit_clone();
    assert(get_env()->function_map.find(ret_clone->get_dag_name()) == get_env()->function_map.end());
    get_env()->function_map.insert(ret_clone->get_dag_name(), ret_clone);
    return ret_clone;
}

SketchFunction *SketchFunction::unit_clone(const string& explicit_name, const map<string, string>* hole_rename_map) {

    assert(rename_holes);
    BooleanDAG* cloned_dag = get_dag()->clone(explicit_name, rename_holes, hole_rename_map);

    if(rename_holes)
    for(auto it : cloned_dag->getNodesByType(bool_node::CTRL)) {
        string actual_name = ((CTRL_node*)it)->get_name();
        if(actual_name != "#PC") {
            string var_name = ((CTRL_node *) it)->get_original_name();
            string sub_dag_name = ((CTRL_node *) it)->get_source_dag_name();

            assert(sub_dag_name == cloned_dag->get_name());
        }
    }

    const FMTL::TransformPrimitive * new_primitive = get_env()->function_map.clone(get_dag_name(),
                                                                                   cloned_dag->get_name(),
                                                                                   cloned_dag->get_hole_assignment_map());

    SketchFunction* ret = new SketchFunction(
            cloned_dag, get_env(),
            replaced_labels, original_labels, new_primitive, dependencies, get_inlining_tree(false), get_has_been_concretized());

    for(auto it: ret->get_dag()->getNodesByType(bool_node::UFUN)) {
        string ufun_name = ((UFUN_node*)it)->get_ufun_name();
        assert(ret->dependencies.find(ufun_name) != ret->dependencies.end());
    }

    return ret;
}

void SketchFunction::core_clear(const string& dag_name)
{
    assert(local_clear_id == global_clear_id);

    get_env()->function_map.erase(dag_name);

    for(auto it: dependencies) {
        if(it.second->local_clear_id != global_clear_id) {
            assert(get_env()->function_map.find(it.second->get_dag_name()) != get_env()->function_map.end());
        }
        it.second->_clear();
    }

    delete this;
}

long long SketchFunction::global_clear_id = 0;

bool SketchFunction::_clear()
{
    if(local_clear_id != global_clear_id) {
        local_clear_id = global_clear_id;
    }
    else {
        return false;
    }

    string dag_name = get_dag_name();

    long long prev_global = global_clear_id;
    if(BooleanDagUtility::soft_clear()) {
        assert(prev_global == global_clear_id);
        core_clear(dag_name);
        return true;
    }
    else {
        local_clear_id = -1;
        if(dependencies.has(dag_name) && get_num_shared_ptr() == 1) {
            assert(dependencies.at(dag_name) == this);
            bool ret = _clear();
            assert(ret);
            return ret;
        }
        return false;
    }
}

void SketchFunction::clear(){

    //assert invariant
    for(const auto& sk_it : get_env()->function_map)
    {
        auto ufuns = sk_it.second->get_dag()->getNodesByType(bool_node::UFUN);
        for(auto it_ufun : ufuns)
        {
            string ufname = ((UFUN_node *) it_ufun)->get_ufun_name();
            assert(get_env()->function_map.find(ufname) != get_env()->function_map.end());
        }
    }

    global_clear_id++;

    ProgramEnvironment* prev_env = get_env();

    _clear();

    for(const auto& sk_it : prev_env->function_map)
    {
        auto ufuns = sk_it.second->get_dag()->getNodesByType(bool_node::UFUN);
        for(auto it_ufun : ufuns)
        {
            string ufname = ((UFUN_node *) it_ufun)->get_ufun_name();
            assert(prev_env->function_map.find(ufname) != prev_env->function_map.end());
        }
    }
}

void SketchFunction::clear_assert_num_shared_ptr_is_0() {
    global_clear_id++;

    string dag_name = get_dag_name();

    if(BooleanDagUtility::soft_clear_assert_num_shared_ptr_is_0()) {
        core_clear(dag_name);
    }
}

void SketchFunction::replace(const string replace_this, const string with_this) {
    assert(new_way);

    AssertDebug(!is_inlining_tree_nonnull(), "TODO: when renaming, need to update inlining tree.");
    AssertDebug(!get_has_been_concretized(), "TODO: Implement ability to replace after concretizing.")

    assert(get_env()->function_map.find(with_this) != get_env()->function_map.end());

    {
        assert(replaced_labels.find(replace_this) != replaced_labels.end());
        assert(original_labels.find(replace_this) != original_labels.end());

        if(replaced_labels[replace_this] == with_this){
            assert(get_dag_name() == with_this);

            assert(dependencies.find(with_this) != dependencies.end());

            //nothing to do.
            //replacing a label with itself in a self-recursive function.
            return;
        }

        rep = get_env()->function_map.replace_label_with_another(get_dag_name(), replace_this, with_this);
        get_dag()->replace_label_with_another(replaced_labels[replace_this], with_this);

        assert(replaced_labels.find(replace_this) != replaced_labels.end());

        auto original_it = original_labels.find(replace_this);
        assert(original_it != original_labels.end());
        assert(original_it->second == replace_this);
        string prev_dep_name = replaced_labels[replace_this];

        assert(get_env()->function_map.find(prev_dep_name) != get_env()->function_map.end());
        assert(dependencies.find(prev_dep_name) != dependencies.end());

        assert(get_env()->function_map.find(with_this) != get_env()->function_map.end());

        increment_shared_ptr(); // need this bc the instruction bellow might erase it otherwise.
        dependencies.erase(prev_dep_name);
        decrement_shared_ptr_wo_clear(); // need this bc the instruction bellow might erase it otherwise.

        assert(get_env()->function_map.find(with_this) != get_env()->function_map.end());

        if(get_inlining_tree(false) != nullptr){
            AssertDebug(false, "before you couldn't rename after inlining. attend to this. need to edit the inlining tree.");
            assert(get_inlining_tree()->find(prev_dep_name) != nullptr);
        }
    }

    auto dependency_it = get_env()->function_map.find(with_this);
    assert(dependency_it != get_env()->function_map.end());
    assert(dependencies.find(with_this) == dependencies.end());

    if (dependency_it->second != this){
        assert(dependency_it->second->get_dag_name() != get_dag_name());
        dependencies.insert(with_this, dependency_it->second);
    }
    else {
        assert(dependency_it->second->get_dag_name() == get_dag_name());
        dependencies.insert(with_this, dependency_it->second);
    }

    replaced_labels[replace_this] = with_this;
}

SketchFunction * SketchFunction::produce_get(const string& get_the_dag_under_this_varname) {
    return get_env()->function_map.produce_get(get_dag_name(), get_the_dag_under_this_varname);
}

string SketchFunction::get_assignment(const string& key) {
    auto it = replaced_labels.find(key);
    assert(it != replaced_labels.end());
    return it->second;
}

void SketchFunction::reset(const string& key) {
    auto it = replaced_labels.find(key);
    assert(it != replaced_labels.end());

    auto original_it = original_labels.find(key);
    assert(original_it != original_labels.end());
    assert(original_it->second == key);

    replace(key, key);
}

const map<string, string> &SketchFunction::get_replace_map() const {
    return replaced_labels;
}

void SketchFunction::set_rep(const FMTL::TransformPrimitive *new_or_existing_primitive) {
    if(rep == nullptr) {
        rep = new_or_existing_primitive;
    }
    else {
        assert(rep == new_or_existing_primitive);
    }
}

const FMTL::TransformPrimitive * SketchFunction::get_rep() {
    return rep;
}

const FMTL::TransformPrimitive * SketchFunction::get_mirror_rep() const {
    return mirror_rep;
}

void SketchFunction::set_mirror_rep(const FMTL::TransformPrimitive *_mirror_rep) {
    mirror_rep = _mirror_rep;
}

SketchFunction * SketchFunction::unit_exact_clone_in_fresh_env(
        Dependencies &new_dependencies, ProgramEnvironment *fresh_env)
{
    BooleanDAG *cloned_dag = get_dag()->clone(get_dag_name(), false);

    SketchFunction *clone = new SketchFunction(
            cloned_dag, fresh_env,
            replaced_labels, original_labels, nullptr, new_dependencies, get_inlining_tree(false),
            get_has_been_concretized());

    clone->set_rep(fresh_env->function_map.insert(get_dag_name(), clone));

    return clone;
}

SketchFunction* SketchFunction::deep_exact_clone_and_fresh_function_map(
        ProgramEnvironment *new_environment, map<SketchFunction*, SketchFunction*>* dp)
{
    assert(dp->find(this) == dp->end());
    (*dp)[this] = nullptr;

    if(new_environment == nullptr) {
        new_environment = get_env()->shallow_copy_w_new_blank_function_map();
    }

    FunctionMap &fresh_function_map = new_environment->function_map;

    Dependencies new_dependencies = Dependencies();

    SketchFunction *ret = nullptr;

    set<string> unit_visited_ufun_names;

    for(auto node: get_dag()->getNodesByType(bool_node::UFUN))
    {
        UFUN_node* ufun_node = (UFUN_node*)node;
        string ufun_name = ufun_node->get_ufun_name();

        if(unit_visited_ufun_names.find(ufun_name) != unit_visited_ufun_names.end()) {
            continue;
        }
        AssertDebug(unit_visited_ufun_names.find(ufun_name) == unit_visited_ufun_names.end(),
                    "TODO: REFACTOR THIS SLIGHTLY TO ONLY PROCESS UFUN_NAMEs ONCE. [arises in case: f(){g(); g()}]<enough to only process g once>");

        unit_visited_ufun_names.insert(ufun_name);

        if (ufun_name == get_dag_name()) {
            assert(ret == nullptr);
            assert(this == get_env()->function_map[ufun_name]);
            assert(dp->find(this) != dp->end());
            assert((*dp)[this] == nullptr);
            continue;
        }

        assert(get_env()->function_map.find(ufun_name) != get_env()->function_map.end());

        SketchFunction* sub_skfunc = get_env()->function_map[ufun_name];
        assert(sub_skfunc != this);

        assert(ufun_name == sub_skfunc->get_dag_name());

//        assert(fresh_function_map.find(ufun_name) == fresh_function_map.end());

        SketchFunction *fresh_deep_exact_copy = nullptr;

        if(dp->find(sub_skfunc) == dp->end()) {
             fresh_deep_exact_copy = sub_skfunc->deep_exact_clone_and_fresh_function_map(new_environment, dp);
        }
        else
        {
            assert((*dp)[sub_skfunc] != nullptr);
            fresh_deep_exact_copy = (*dp)[sub_skfunc];
        }

        assert(fresh_deep_exact_copy->get_dag_name() == ufun_name);
        assert(fresh_function_map.find(ufun_name) != fresh_function_map.end());
        assert(fresh_function_map[ufun_name]->get_dag_name() == ufun_name);

        new_dependencies.insert(ufun_name, fresh_deep_exact_copy);
    }

    assert(ret == nullptr);
    if (ret == nullptr) {
        //exact copy of root skfunc.

        string _ufun_name = this->get_dag_name();
//        assert(fresh_function_map.find(_ufun_name) == fresh_function_map.end());

        SketchFunction* _sub_skfunc = this;

        SketchFunction* fresh_unit_clone = nullptr;

        fresh_unit_clone = (*dp)[_sub_skfunc];
        assert(fresh_unit_clone == nullptr);
        fresh_unit_clone = _sub_skfunc->unit_exact_clone_in_fresh_env(new_dependencies, new_environment);

        assert(fresh_function_map.find(_ufun_name) != fresh_function_map.end());
        assert(fresh_function_map[_ufun_name]->get_dag_name() == _ufun_name);

        assert(fresh_unit_clone == fresh_function_map[_ufun_name]);
        ret = fresh_function_map[_ufun_name];
    }

    new_dependencies.clear();

    (*dp)[this] = ret;

    return ret;


    bool flat_implementation = false;
    if(flat_implementation)
    //--------
    //flattening, but it doesn't work because you need to reassign the dependencies;
    //which you can only do recursively
    {

        set<string> *inlined_functions = get_inlined_functions();

        ProgramEnvironment *_new_environment = get_env()->shallow_copy_w_new_blank_function_map();
        FunctionMap &fresh_function_map = _new_environment->function_map;

        SketchFunction *ret = nullptr;

        for (const auto &ufun_name: *inlined_functions) {
            assert(fresh_function_map.find(ufun_name) == fresh_function_map.end());
            assert(get_env()->function_map.find(ufun_name) != get_env()->function_map.end());

            assert(ufun_name == get_env()->function_map[ufun_name]->get_dag_name());
            get_env()->function_map[ufun_name]->unit_exact_clone_in_fresh_env(
                    get_env()->function_map[ufun_name]->dependencies, _new_environment);

            assert(fresh_function_map.find(ufun_name) != fresh_function_map.end());
            assert(fresh_function_map[ufun_name]->get_dag_name() == ufun_name);

            if (ufun_name == get_dag_name()) {
                assert(ret == nullptr);
                assert(this == get_env()->function_map[ufun_name]);
                ret = fresh_function_map[ufun_name];
            }
        }

        if (ret == nullptr) {
            //exact copy root.
            string _ufun_name = this->get_dag_name();
            assert(fresh_function_map.find(_ufun_name) == fresh_function_map.end());

            this->unit_exact_clone_in_fresh_env(dependencies, _new_environment);

            assert(fresh_function_map.find(_ufun_name) != fresh_function_map.end());
            assert(fresh_function_map[_ufun_name]->get_dag_name() == _ufun_name);
            ret = fresh_function_map[_ufun_name];
        }

        assert(ret != nullptr);

        for (const auto &it: fresh_function_map) {
            for (auto node: it.second->get_dag()->getNodesByType(bool_node::UFUN)) {
                UFUN_node *ufun_node = (UFUN_node *) node;
                assert(fresh_function_map.find(ufun_node->get_ufun_name()) != fresh_function_map.end());
            }
        }

        return ret;
    }
}

SketchFunction* SketchFunction::deep_clone(bool only_tail)
{

    //first get all inlined functions
    //clone all inlined functions;
    //replace the name inside the inlined function with eachother's

    //assert that all the ufuns are represented in the function map
    for (const auto& it: get_env()->function_map) {
        for (auto ufun_it: it.second->get_dag()->getNodesByType(bool_node::UFUN)) {
            auto f = get_env()->function_map.find(((UFUN_node *) ufun_it)->get_ufun_name());
            assert(f != get_env()->function_map.end());
        }
    }

    set<string>* inlined_functions = get_inlined_functions();

    assert(inlined_functions != nullptr);

    //assert that all that the previous operation hasn't corrupted the function map ufuns invariant
    for (const auto& it: get_env()->function_map) {
        for (auto ufun_it: it.second->get_dag()->getNodesByType(bool_node::UFUN)) {
            auto f = get_env()->function_map.find(((UFUN_node *) ufun_it)->get_ufun_name());
            assert(f != get_env()->function_map.end());
        }
    }

    // assert that all the dags in the inlined functions have all the ufuns also in the inlined functions
    for (const auto &inlined_f_name: *inlined_functions) {
        auto it_f = get_env()->function_map.find(inlined_f_name);
        assert(it_f != get_env()->function_map.end());
        for (auto ufun_it: it_f->second->get_dag()->getNodesByType(bool_node::UFUN)) {
            auto f = get_env()->function_map.find(((UFUN_node *) ufun_it)->get_ufun_name());
            assert(f != get_env()->function_map.end());
            assert(inlined_functions->find(((UFUN_node *) ufun_it)->get_ufun_name()) != inlined_functions->end());
        }
    }

    SketchFunction* clone_of_this = nullptr;

    bool entered_recursive_case = false;
    map<string, SketchFunction *> to_inline_skfuncs;
    for (const string &_inlined_function_name: *inlined_functions) {
        assert(get_env()->function_map.find(_inlined_function_name) != get_env()->function_map.end());
        assert(to_inline_skfuncs.find(_inlined_function_name) == to_inline_skfuncs.end());
        if(only_tail) {
            if (_inlined_function_name != get_dag_name()) {
                const string &to_clone_fname = _inlined_function_name;
                to_inline_skfuncs[to_clone_fname] = get_env()->function_map[to_clone_fname]->unit_clone_and_insert_in_function_map();
            } else {
                assert(_inlined_function_name == get_dag_name());
                entered_recursive_case = true;
                to_inline_skfuncs[_inlined_function_name] = get_env()->function_map[_inlined_function_name];
            }
        }
        else {
            const string &to_clone_fname = _inlined_function_name;
            if(get_dag_name() == _inlined_function_name)
            assert(get_env()->function_map[to_clone_fname] == this);
            to_inline_skfuncs[to_clone_fname] = get_env()->function_map[to_clone_fname]->unit_clone_and_insert_in_function_map();
            if (_inlined_function_name == get_dag_name()) {
                entered_recursive_case = true;
                clone_of_this = to_inline_skfuncs[to_clone_fname];
            }
        }
    }

    if(entered_recursive_case) {
        assert(to_inline_skfuncs.find(get_dag_name()) != to_inline_skfuncs.end());
    }
    else {
        assert(to_inline_skfuncs.find(get_dag_name()) == to_inline_skfuncs.end());
    }

    assert(to_inline_skfuncs.size() == inlined_functions->size());

    for (const auto &it: *inlined_functions) {
        assert(to_inline_skfuncs.find(it) != to_inline_skfuncs.end());
    }

    delete inlined_functions;
    inlined_functions = nullptr;

    if (to_inline_skfuncs.find(get_dag_name()) != to_inline_skfuncs.end()) {
        assert(entered_recursive_case);
        if(only_tail) {
            assert(to_inline_skfuncs[get_dag_name()] == this);
        }
    } else {
        assert(!entered_recursive_case);
        assert(clone_of_this == nullptr);
        if(only_tail)
        {
            to_inline_skfuncs[get_dag_name()] = this;
        }
        else {
            const string &to_clone_fname = get_dag_name();
            assert(to_inline_skfuncs.find(to_clone_fname) == to_inline_skfuncs.end());
            to_inline_skfuncs[to_clone_fname] = unit_clone_and_insert_in_function_map();
            clone_of_this = to_inline_skfuncs[to_clone_fname];
        }
    }

    assert(to_inline_skfuncs.find(get_dag_name()) != to_inline_skfuncs.end());

    if(only_tail) {
        assert(clone_of_this == nullptr);
    }

    //rename all the ufuns
    for (const auto& skfunc_it: to_inline_skfuncs) {
        set<pair<string, string> > ufnames;

        for (auto ufun_it: skfunc_it.second->get_dag()->getNodesByType(bool_node::UFUN)) {
            string ufname = ((UFUN_node *) ufun_it)->get_ufun_name();
            string original = ((UFUN_node *) ufun_it)->get_original_ufname();
            ufnames.insert(make_pair(ufname, original));
        }

        for (const pair<string, string> &ufname_now_original: ufnames) {
            string ufname = ufname_now_original.first;
            string original = ufname_now_original.second;

            assert(to_inline_skfuncs[ufname]->get_env()->function_map.find(to_inline_skfuncs[ufname]->get_dag_name()) !=
            to_inline_skfuncs[ufname]->get_env()->function_map.end());

            if (to_inline_skfuncs.find(ufname) == to_inline_skfuncs.end()) {
                AssertDebug(false, "this should fail here, it was checked before");
            } else {
                SketchFunction* skfunc = skfunc_it.second;
                skfunc->replace(original, to_inline_skfuncs[ufname]->get_dag_name());
            }
        }
    }

    if(only_tail) {
        assert(clone_of_this == nullptr);
    }

    return clone_of_this;
}

void SketchFunction::deep_clone_tail() {
    assert(deep_clone(true) == nullptr);
}

set<string> SketchFunction::ufun_names() {
    set<string> ret;
    for(auto it: get_dag()->getNodesByType(bool_node::UFUN))
    {
        ret.insert(((UFUN_node *) it)->get_ufun_name());
    }
    return ret;
}

VarStore *SketchFunction::get_solution() {
    LightInliningTree* local_inlining_tree = nullptr;
    bool new_inlining_tree = false;
    if(get_has_been_concretized()) {
        assert(get_inlining_tree() != nullptr);
        local_inlining_tree = new LightInliningTree(get_inlining_tree());
    }
    else {
        new_inlining_tree = true;
        local_inlining_tree = new LightInliningTree(this);
    }

    assert(local_inlining_tree != nullptr);

    VarStore* ret = local_inlining_tree->get_solution();
    assert(ret->get_inlining_tree() == nullptr);
    ret->set_inlining_tree(local_inlining_tree);

    return ret;
}

void SketchFunction::set_dependencies(const FunctionMap* fmap) {
    if(!new_way)
    {
        return;
    }
    assert(dependencies.empty());
    assert(replaced_labels.empty());
    assert(original_labels.empty());
    for(auto it: get_dag()->getNodesByType(bool_node::UFUN))
    {
        string ufun_name = ((UFUN_node *) it)->get_ufun_name();
        if(dependencies.find(ufun_name) == dependencies.end()) {
            assert(fmap->find(ufun_name) != fmap->end());
            dependencies.insert(ufun_name, (*fmap)[ufun_name]);

            assert(replaced_labels.find(ufun_name) == replaced_labels.end());
            replaced_labels[ufun_name] = ufun_name;
            assert(original_labels.find(ufun_name) == original_labels.end());
            original_labels[ufun_name] = ufun_name;

        }

        else
        {
            assert(replaced_labels.find(ufun_name) != replaced_labels.end());
            assert(replaced_labels[ufun_name] == ufun_name);
            assert(original_labels.find(ufun_name) != original_labels.end());
            assert(original_labels[ufun_name] == ufun_name);
        }
    }
}

vector<string> SketchFunction::get_unit_holes() {
    vector<string> ret = vector<string>();
    for (auto it: get_dag()->getNodesByType(bool_node::CTRL)) {
        if (it->get_name() != "#PC") {
            ret.push_back(it->get_name());
        }
    }
    return ret;
}

const map<string, string> &SketchFunction::get_unit_ufuns_map() {
    return replaced_labels;
}

//bool SketchFunction::has_unit_self_loop() const {
//    bool ret = false;
//    for(auto it: get_dag()->getNodesByType(bool_node::UFUN)) {
//        if(((UFUN_node*)it)->get_ufun_name() == get_dag_name()) {
//            ret = true;
//        }
//    }
//
//    bool second_ret = false;
//    for(const auto& it: replaced_labels)
//    {
//        if(it.second == get_dag_name()) {
//            assert(ret);
//            second_ret = true;
//        }
//    }
//    assert(ret == second_ret);
//
//    return ret;
//}



#include "SolverLanguageLexAndYaccHeader.h"

#include "GenericFile.h"
#include "File.h"


#include <chrono>

int SketchFunction::count_passing_inputs(File *file, bool do_assert) {
    if(do_assert) {
        assert(false);
    }

    SL::PolyVec* passing_inputs_bitvector = evaluate_inputs(file);
    vector<bool> bitvector = passing_inputs_bitvector->to_vector_bool();
    passing_inputs_bitvector->clear();
    int predicted_ret = 0;
    for(int i = 0;i<bitvector.size();i++) {
        predicted_ret+=bitvector[i];
    }
    return predicted_ret;
}

#include <cilk/cilk.h>
#include <cilk/cilk_api.h>

//#define cilk_spawn
//#define cilk_sync
//#define cilk_for for



SL::PolyVec* SketchFunction::evaluate_inputs(File *file) {

    cout << "START MEASURING TIME (evaluate_inputs)" << endl;
    auto start = chrono::steady_clock::now();

    SketchFunction* concretized_clone = deep_exact_clone_and_fresh_function_map();
    concretized_clone->increment_shared_ptr();
    concretized_clone->inline_this_dag(false);
    SL::PolyVec* ret = new SL::PolyVec(new SL::PolyType("any"), file->size());

    cilk_for(int i = 0;i<file->size();i++) {
        bool passes = SketchFunctionEvaluator::new_passes(
                concretized_clone, file->at(i), false)->get_bool(true, false);
        SL::VarVal* passes_var_val = new SL::VarVal(passes);
        ret->set(i, passes_var_val);
//        ret->push_back(passes_var_val); //sequential
/// FOR EXECUTION (i.e. getting the output, rather whether or not it passes).
//        if(passes) {
//            SL::VarVal *output = SketchFunctionEvaluator::eval(concretized_clone, file->at(i));
//            ret->push_back(output);
//        }
//        else {
//            ret->push_back(nullptr);
//        }
    }
    concretized_clone->clear();


    auto end = chrono::steady_clock::now();

    auto elapsed = chrono::duration_cast<chrono::microseconds>(end - start).count();

    cout << "END MEASURING TIME (evaluate_inputs)" << endl;
    cout << "ELAPSED: " << elapsed << " (microseconds)" << endl;
    cout << "__cilkrts_get_nworkers " << __cilkrts_get_nworkers() << endl;
    cout << "__cilkrts_get_worker_number " << __cilkrts_get_worker_number() << endl;
    cout << "__cilkrts_get_total_workers " << __cilkrts_get_total_workers() << endl;

    return ret;
}


SL::VarVal *SketchFunctionEvaluator::eval(SketchFunction *skfunc, const string& _line)
{
    VarStore* var_store = string_to_var_store(_line, skfunc);
    auto ret = new_passes(skfunc, var_store);
    var_store->clear();
    return ret;
}


SL::VarVal* SketchFunctionEvaluator::eval(SketchFunction *skfunc, const VarStore* _the_var_store) {
    BooleanDAG *the_dag = nullptr;
    bool new_clone = false;

    assert(skfunc->get_has_been_concretized());
    AssertDebug(skfunc->get_dag()->get_failed_assert() == nullptr, "YOU CAN'T EVALUATE A SKFUNC THAT HAS A FAILING ASSERT BEFORE EVEN PASSING THE INPUT.");
    assert(skfunc->get_dag()->getNodesByType(bool_node::CTRL).empty());
    assert(skfunc->get_dag()->getNodesByType(bool_node::UFUN).empty());

    if (skfunc->get_has_been_concretized()) {
        the_dag = skfunc->get_dag();
    } else {
        AssertDebug(false, "You could support this. If the dag wasn't concretized, then inline it and see if it's evaluable.")
        the_dag = skfunc->get_dag()->clone();
        new_clone = true;
        skfunc->get_env()->doInline(*the_dag);
    }

    VarStore *the_var_store = new VarStore(*_the_var_store);

    const bool assert_num_remaining_holes_is_0 = true;
    if (assert_num_remaining_holes_is_0) {
        size_t remaining_holes = the_dag->getNodesByType(bool_node::CTRL).size();
        AssertDebug(remaining_holes == 0,
                    "This dag should not havey any holes remaining, but it has " +
                    std::to_string(remaining_holes) + " remaining_holes.");
    }

    NodeEvaluator node_evaluator(*the_dag, skfunc->get_env()->floats);
    bool fails = node_evaluator.run(*the_var_store);

    the_var_store->clear();
    the_var_store = nullptr;

    AssertDebug(!fails, "the dag " + the_dag->get_name() + " asserts false on this input.");

    auto after_run_dests = the_dag->getNodesByType(bool_node::DST);
    assert(after_run_dests.size() == 1);
    //SHOULD BE THIS BUT ISN'T
//                      ret = new VarVal(node_evaluator.getValue(after_run_dests[0]));
    int tuple_node_id = node_evaluator.getValue(after_run_dests[0]);
    bool_node *tuple_node = (*the_dag)[tuple_node_id];
    assert(tuple_node->getOtype()->isTuple && tuple_node->type == bool_node::TUPLE_CREATE);
    AssertDebug(tuple_node->nparents() == 1, "NEET TO GENERALIZE THIS.");
    OutType *out_type = tuple_node->get_parent(0)->getOtype();

    int val = node_evaluator.getValue(tuple_node->get_parent(0));
    //THIS IS A HACK BUT WORKS FOR NOW
    SL::VarVal* ret = nullptr;
    if (out_type == OutType::BOOL) {
        ret = new SL::VarVal((bool) val);
    } else if (out_type == OutType::INT) {
        ret = new SL::VarVal((int) val);
    } else {
        AssertDebug(false,
                    "NEED TO GENERALIZE THIS (^). IN GENERAL out_type can be anything. This was put like this because ::BOOL was the only time that was being used for testing.");
    }

    if (new_clone) {
        the_dag->clear();
    }

    assert(ret != nullptr);
    return ret;
}

SL::VarVal *SketchFunctionEvaluator::new_passes(BooleanDagLightUtility *skfunc, const string& _line)
{
    VarStore* var_store = string_to_var_store(_line, skfunc);
    auto ret = new_passes(skfunc, var_store);
    var_store->clear();
    return ret;
}

SL::VarVal *SketchFunctionEvaluator::new_passes(BooleanDagLightUtility *skfunc, const VarStore* _the_var_store, bool do_assert)
{
    if(do_assert) {
        assert(false);
    }
    if(skfunc->get_dag()->get_failed_assert() != nullptr) {
        return new SL::VarVal(false);
    }

    assert(skfunc->get_has_been_concretized());
    assert(skfunc->get_dag()->get_failed_assert() == nullptr);
    assert(skfunc->get_dag()->getNodesByType(bool_node::CTRL).empty());
    assert(skfunc->get_dag()->getNodesByType(bool_node::UFUN).empty());

    BooleanDAG *the_dag = nullptr;
    bool new_clone = false;

    if (skfunc->get_has_been_concretized()) {
        the_dag = skfunc->get_dag();
    } else {
        the_dag = skfunc->get_dag()->clone();
        new_clone = true;
        skfunc->get_env()->doInline(*the_dag);
    }

    VarStore *the_var_store = new VarStore(*_the_var_store);

    const bool assert_num_remaining_holes_is_0 = true;
    if (assert_num_remaining_holes_is_0) {
        size_t remaining_holes = the_dag->getNodesByType(bool_node::CTRL).size();
        AssertDebug(remaining_holes == 0,
                    "This dag should not havey any holes remaining, but it has " +
                    std::to_string(remaining_holes) + " remaining_holes.");
    }

    NodeEvaluator node_evaluator(*the_dag, skfunc->get_env()->floats);
    bool fails = node_evaluator.run(*the_var_store);

    if(new_clone) {
        the_dag->clear();
    }
    the_var_store->clear();

    return new SL::VarVal(!fails);
}

