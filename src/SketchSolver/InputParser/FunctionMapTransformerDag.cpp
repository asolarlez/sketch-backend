//
// Created by kliment on 1/29/22.
//

#include "FunctionMapTransformerDag.h"
#include "SketchFunction.h"

using namespace FMTL;

const TransformPrimitive * FunctionMapTransformer::concretize(const string &function_name, const VarStore *store, const bool_node::Type type,
                                                              const vector<string> *sub_functions) {

    assert(root_dag_reps.find(function_name) != root_dag_reps.end());
    assert(root_dag_reps[function_name]->get_function_name() == function_name);
    TransformPrimitive* new_primitive =
            new ConcretizePrimitive(function_name, store, type);
    add_parent(new_primitive, function_name, true);
    if(sub_functions != nullptr) {
        for (const auto &it: *sub_functions) {
            if(root_dag_reps[it]->get_meta_type() != _make_executable && it != function_name)
            {
                bool ok = false;
                auto at = root_dag_reps[it];
                while(at != nullptr)
                {
                    at = at->get_main_parent();
                    if(at->get_meta_type() == _make_executable)
                    {
                        ok = true;
                        break;
                    }
                    else if(at->get_meta_type() != _clone)
                    {
                        assert(false);
                    }
                    assert(at->get_meta_type() == _clone);
                }
            }
            add_parent(new_primitive, it);
        }
    }
    program.insert(new_primitive);
    root_dag_reps[function_name] = new_primitive;
    assert(root_dag_reps[function_name]->get_function_name() == function_name);
    return new_primitive;
}

const TransformPrimitive * FunctionMapTransformer::replace_label_with_another(
        const string& function_name, const string &replace_this_str, const string &with_this_str) {

    auto main_parent_it = root_dag_reps.find(function_name);
    assert( main_parent_it != root_dag_reps.end());
    assert(root_dag_reps.find(with_this_str) != root_dag_reps.end());
    auto main_parent = main_parent_it->second;
    assert(main_parent->get_function_name() == function_name);

    auto new_primitive = new ReplacePrimitive(function_name, replace_this_str, with_this_str);
    add_parent(new_primitive, function_name, true);
//    add_parent(root_dag_reps[with_this_str], function_name, false);
    program.insert(new_primitive);
    root_dag_reps[function_name] = new_primitive;
    assert(root_dag_reps[function_name]->get_function_name() == function_name);
    return new_primitive;
}

const TransformPrimitive * FunctionMapTransformer::clone(const string &original_function_name,
                                                         const string &clone_function_name,
                                                         const map<string, string> hole_rename_map) {

    assert(original_function_name != clone_function_name);
    assert(root_dag_reps.find(clone_function_name) == root_dag_reps.end());
    auto new_primitive = new ClonePrimitive(original_function_name, clone_function_name, hole_rename_map);
    add_parent(new_primitive, original_function_name, true);
    program.insert(new_primitive);
    root_dag_reps[clone_function_name] = new_primitive;
    assert(root_dag_reps[clone_function_name]->get_function_name() == clone_function_name);

    if(false) {
        cout << "new clone! " << original_function_name << " -> " << clone_function_name << endl;
        cout << "program.size() " << program.size() << endl;
        cout << "root_dag_reps.size() " << root_dag_reps.size() << endl;
        int erased_kids = 0;
        for (auto it: root_dag_reps) {
            erased_kids += it.second->get_is_erased();
        }
        cout << "non_erased_kids " << root_dag_reps.size() - erased_kids << endl;
        int dag_size = calc_dag_size();
        cout << "dag_size() " << calc_dag_size() << endl;
        assert(dag_size == program.size());
        cout << endl;
    }

    return new_primitive;
}

const TransformPrimitive * FunctionMapTransformer::insert(const string &new_function_name, const vector<string>& subfunction_names, const vector<string>& hole_names) {

    if(root_dag_reps.find(new_function_name) == root_dag_reps.end()) {
        auto new_primitive = new InitPrimitive(new_function_name, subfunction_names, hole_names);
        program.insert(new_primitive);
        root_dag_reps[new_function_name] = new_primitive;
        assert(root_dag_reps[new_function_name]->get_function_name() == new_function_name);
        return new_primitive;
    }
    else
    {
        auto existing_primitive = root_dag_reps[new_function_name];
        assert(existing_primitive->get_function_name() == new_function_name);
        assert(!existing_primitive->get_is_erased());
        return existing_primitive;
    }
}

void FunctionMapTransformer::erase(const string &to_erase_name, bool assert_not_in_function_map) {
    if(assert_not_in_function_map) {
        assert(function_map->find(to_erase_name) == function_map->end());
    }
//    if(to_erase_name == "condition")
//    {
//        cout << "HERE" << endl;
//    }
    assert(root_dag_reps.find(to_erase_name) != root_dag_reps.end());
    assert(root_dag_reps[to_erase_name]->get_function_name() == to_erase_name);
    auto to_erase = root_dag_reps[to_erase_name];
//    check_consistency();
    auto ret = to_erase->erase(this);
    if(ret == nullptr) {
        assert(root_dag_reps.find(to_erase_name) == root_dag_reps.end());
    }
    else {
        assert(root_dag_reps.find(to_erase_name)->second == ret);
    }
    erased_root_dag_reps.insert(to_erase_name);
//    check_consistency();
}

set<TransformPrimitive *> &FunctionMapTransformer::get_program() {

    return program;
}

const map<string, TransformPrimitive *> & FunctionMapTransformer::get_root_dag_reps() const {

    return root_dag_reps;
}

map<string, TransformPrimitive *> & FunctionMapTransformer::get_root_dag_reps_nonconst() {
    return root_dag_reps;
}

void FunctionMapTransformer::check_consistency() {

    for(auto it: program) {
        assert(root_dag_reps.find(it->get_function_name()) != root_dag_reps.end());
        it->check_consistency(this);
        if(it->get_is_erased()) {
            assert(function_map->find(it->get_function_name()) == function_map->end());
            assert(erased_root_dag_reps.find(it->get_function_name()) != erased_root_dag_reps.end());
        }
        else {
            assert(erased_root_dag_reps.find(it->get_function_name()) == erased_root_dag_reps.end());
        }
    }
    for(auto it: root_dag_reps) {
        assert(it.first == it.second->get_function_name());
        assert(!it.second->get_is_superseded() || it.second->get_is_erased());
    }
}

SketchFunction *
FunctionMapTransformer::extract_sketch_function(const string &from_dag, const string &under_this_var,
                                                const string &to_this_dag) const  {
    auto it = root_dag_reps.find(from_dag);
    assert(it != root_dag_reps.end());
    SketchFunction* ret = it->second->extract_sketch_function(to_this_dag, under_this_var, this);
    bool found = ret != nullptr;
    AssertDebug(found, "this indicates that " + to_this_dag + " wasn't found starting from " + from_dag);
    return ret;
}

string FunctionMapTransformer::find_subdag_name(const string &from_dag, const string &find_what_dag_this_varname_maps_to) const
{
    auto it = root_dag_reps.find(from_dag);
    assert(it != root_dag_reps.end());

    bool print = false;
    if(print) {
        cout << endl << "IN find_subdag_name " << from_dag << " " << find_what_dag_this_varname_maps_to << endl;
    }
    TransformPrimitive* subdag = it->second->find_underlying_function(find_what_dag_this_varname_maps_to, this, print);

    AssertDebug(subdag != nullptr, "port_name: '" +find_what_dag_this_varname_maps_to +"' not found.");
    string subdag_name = subdag->get_function_name();
    assert(root_dag_reps.find(subdag_name) != root_dag_reps.end());
    assert(root_dag_reps.at(subdag_name) == subdag);

    if(print)
        cout << "find_subdag_name(" + from_dag + ", " + find_what_dag_this_varname_maps_to + ") returns " << subdag << endl << endl;

    return subdag_name;
}

SketchFunction * FunctionMapTransformer::reconstruct_sketch_function(const string &from_dag, const string &under_this_var, const string &to_this_dag) {
    auto it = root_dag_reps.find(from_dag);
    assert(it != root_dag_reps.end());
    auto ret = it->second->reconstruct_sketch_function(to_this_dag, under_this_var, this);
    bool found = ret != nullptr;
    AssertDebug(found, "this indicates that " + to_this_dag + " wasn't found starting from " + from_dag);
    return ret;
}

bool FunctionMapTransformer::contains_only_necessary() {
    bool ret = true;
    for(auto it: program) {
        if (!it->get_is_erased()) {
            if (function_map->find(it->get_function_name()) == function_map->end()) {
                ret = false;
            }
        }
    }
    return ret;
}

void FunctionMapTransformer::reinsert(const string &to_reinsert_name) {
    auto rep = root_dag_reps.find(to_reinsert_name);
    assert(rep != root_dag_reps.end());
    assert(rep->second->get_is_erased());
    rep->second->unerase();
}

void TransformPrimitive::set_is_erased(bool is_original) {
    if (!is_erased) {
        is_erased = true;
        if (main_parent != nullptr) {
            if (main_parent->function_name == function_name) {
                main_parent->set_is_erased(false);
            }
        }
    }
}

TransformPrimitive *TransformPrimitive::erase(FunctionMapTransformer *transformer)
{
    set_is_erased(true);
    assert(is_erased);

    if(!children.empty()) {
        return this;
    }
    assert(children.empty());
    for(const auto& it_parent : parents)
    {
        auto parent = it_parent.second;
        auto this_child = parent->children.find(this);
        assert(this_child != parent->children.end());
        int prev_num_children = parent->children.size();
        parent->children.erase(this_child);
        assert(prev_num_children-1 == parent->children.size());
    }

    map<string, TransformPrimitive *> & root_dag_reps = transformer->get_root_dag_reps_nonconst();

    set<TransformPrimitive*>& program = transformer->get_program();

    TransformPrimitive* ret = nullptr;
    for(const auto& it_parent : parents)
    {
        auto parent = it_parent.second;
        if(parent->is_erased) {
            string parent_name = parent->function_name;
            if(program.find(parent) == program.end()) {
                assert(root_dag_reps.find(parent_name) == root_dag_reps.end());
                assert(parent_name != function_name);
                continue;
            }
            else
            {
                assert(root_dag_reps.find(parent_name) != root_dag_reps.end());
            }

            TransformPrimitive* remaining_rep = parent->erase(transformer);

            if(remaining_rep != nullptr && parent_name == function_name)
            {
                assert(remaining_rep->function_name == function_name);
                assert(ret == nullptr);
                ret = remaining_rep;
            }
            else if(parent_name != function_name)
            {
                if(remaining_rep == nullptr)
                {
                    assert(root_dag_reps.find(parent_name) == root_dag_reps.end());
                }
                else
                {
//                    assert(!root_dag_reps.find(parent_name)->second->superseded || root_dag_reps.find(parent_name)->second->is_erased);
//                    assert(root_dag_reps.find(parent_name)->second == remaining_rep);
                }
            }
        }
    }

    parents.clear();

    {
        auto it = root_dag_reps.find(function_name);
        if(it != root_dag_reps.end()) {
            if (it->second == this) {
                if (ret == nullptr) {
                    root_dag_reps.erase(it);
                } else {
                    it->second = ret;
                }
            } else {
                assert(it->second->function_name == function_name);
                if (it->second == ret) {

                } else {
                    assert(ret == nullptr);
                    root_dag_reps.erase(it);
                }
            }
        }
        else {
            assert(ret == nullptr);
        }
    }

    program.erase(this);

    auto it = root_dag_reps.find(function_name);
    if(it != root_dag_reps.end()) {
        if (it->second == this) {
            assert(ret == nullptr);
            root_dag_reps.erase(it);
        }
        else
        {
            assert(it->second == ret);
        }
    }

    for(const auto& local_it: root_dag_reps)
    {
        assert(local_it.second != this);
    }

    clear();

    return ret;
}

TransformPrimitiveMetaType TransformPrimitive::get_primitive_type() const {
    return meta_type;
}

vector<string> TransformPrimitive::get_inlined_fs() {
    vector<string> ret;
    for(const auto& it_parent:parents)
    {
        auto parent = it_parent.second;
        ret.push_back(parent->function_name);
    }
    return ret;
}

void TransformPrimitive::check_consistency(FunctionMapTransformer* transformer) {
    set<TransformPrimitive*>& program = transformer->get_program();
    auto root_dag_reps = transformer->get_root_dag_reps();
    if(is_erased && children.empty()) {
        assert(false);
    }
    for(const auto& it_parent:parents)
    {
        auto it = it_parent.second;
        assert(program.find(it) != program.end());
        assert(root_dag_reps.find(it->function_name) != root_dag_reps.end());
    }
    for(auto it:children)
    {
        assert(program.find(it) != program.end());
        assert(root_dag_reps.find(it->function_name) != root_dag_reps.end());
    }
}

int TransformPrimitive::num_reachable_nodes() {
    assert(!visited);
    visited = true;
    int ret = 1;
    for(const auto& it_parent: parents)
    {
        auto it = it_parent.second;
        if(!it->visited) {
            ret += it->num_reachable_nodes();
        }
    }
    return ret;
}
TransformPrimitive * TransformPrimitive::find_underlying_function(const string &var_name, const FunctionMapTransformer *root,
                                                                  const bool print,
                                                                  int t) const  {
    auto assign_map = get_assign_map();
    TransformPrimitive * ret_so_far = nullptr;
    if(print) {
        if((assign_map == nullptr || assign_map->empty()) && main_parent == nullptr) {
            return ret_so_far;
        }
        cout << tab(t) << "entering " << function_name << " (" << transform_primitive_meta_type_name[meta_type] <<") " << endl;
    }
    if(assign_map != nullptr) {
        if(print) {
            assert(!assign_map->empty() || main_parent == nullptr);
            cout << tab(t+1) << function_name << " has ";
            for (const auto& it: *assign_map) {
                cout << it.first << " " << it.second << "; ";
            }
            cout << endl;
        }
        if(assign_map->find(var_name) != assign_map->end())
        {
            string target_dag_name = (*assign_map).at(var_name);
            assert(ret_so_far == nullptr);
            auto parent = parents.find(target_dag_name);
//            if(parent != parents.end()) {
//                ret_so_far = parent->second;
//                return ret_so_far;
//            } else {
//                assert(meta_type == _init);
//                auto reps = root->get_root_dag_reps();
//                auto it = reps.find(target_dag_name);
//                assert(it != reps.end());
//                return it->second;
//            }
            assert(root->get_root_dag_reps().find(target_dag_name) != root->get_root_dag_reps().end());
            return root->get_root_dag_reps().at(target_dag_name);
        }
    }
    else {
        assert(meta_type != _replace && meta_type != _init);
    }
    if(ret_so_far == nullptr) {
        if(main_parent != nullptr)
        {
            ret_so_far = main_parent->find_underlying_function(var_name, root, print, t + 1);
        }
//                for (const auto &it: parents) {
//                    string tmp_ret = it->find_underlying_function(var_name, print, t + 1);
//                    if (!tmp_ret.empty()) {
//                        if (!ret_so_far.empty()) {
//                            assert(ret_so_far == tmp_ret);
//                        } else {
//                            ret_so_far = tmp_ret;
//                        }
//                    }
//                }
    }
    if(print) {
        cout << tab(t+1) << "RETURNS '" << ret_so_far << "'" << endl;
        cout << tab(t) << "exiting " << function_name << endl;
    }
    return ret_so_far;
}


SketchFunction * TransformPrimitive::extract_sketch_function(const string &to_this_dag, const string &under_this_var,
                                                             const FunctionMapTransformer *root) const  {
    switch (meta_type) {
        case _make_executable: {
            for (const auto &parent_it: parents) {
                if (parent_it.first == to_this_dag) {
                    auto parent = parent_it.second;
                    if (parent != main_parent) {
                        //HELPFUL PRINT MESSAGE
                    }
                    assert(parent->function_name == to_this_dag);
                    assert(!parent->is_erased);
                    auto maybe_ret = parent->reconstruct_sketch_function(to_this_dag, under_this_var, root);
                    assert(maybe_ret != nullptr);

                    AssertDebug(maybe_ret->get_has_been_concretized(), "YOU NEED TO RETHINK RECONSTRUCTION ONCE THIS FAILS. NEED MORE INVOLVED STRATEGY.");
                    return maybe_ret;
                }
            }

            AssertDebug(false,
                        "DIDN'T FIND to_this_dag. Can't look further than a _concretization step. You could potentially look further, in the case that to_this_dag was concretized before");
            break;
        }
        case _init:
        case _replace: {
            if(main_parent != nullptr) {
                assert(meta_type == _replace);
            } else {
                assert(meta_type == _init);
            }
            auto it = get_assign_map()->find(under_this_var);
            if (it != get_assign_map()->end()) {
                string name = it->second;
                if(name != to_this_dag) {
                    return nullptr;
                }
                auto it_parent = parents.find(to_this_dag);
                assert(it_parent == parents.end());

                assert(root->get_root_dag_reps().find(to_this_dag) != root->get_root_dag_reps().end());
                return root->get_root_dag_reps().at(to_this_dag)->reconstruct_sketch_function(to_this_dag, under_this_var, root);
            }
            if(meta_type == _init)
            {
                assert(main_parent == nullptr);
                return nullptr;
            }
            else {
                assert(meta_type == _replace);
                assert(main_parent != nullptr);
                return main_parent->extract_sketch_function(to_this_dag, under_this_var, root);
            }
            break;
        }
        case _clone:
            return main_parent->extract_sketch_function(to_this_dag, under_this_var, root);
            break;
        default:
            AssertDebug(false, "IMPLEMENT UNKNOWN CASE");
    }
    AssertDebug(false, "NOT FOUND");
}

//const VarStore *TransformPrimitive::WRONG_find_last_var_store_on_the_way_to(
//        const string &to_this_dag, const string &under_this_var, bool &found) const  {
//    cout << "entering to_this_dag " << to_this_dag <<" under_this_val " << under_this_var << endl;
//    assert(!found);
//    const VarStore* ret_so_far = nullptr;
//    auto var_store = get_var_store();
////    if(var_store != nullptr)
//    {
//        for(const auto& it: parents)
//        {
//            if(it.second->function_name == to_this_dag)
//            {
//                found = true;
//                return var_store;
//            }
//        }
//        if(function_name == to_this_dag) {
//            found = true;
//            return var_store;
//        }
//    }
//
//    if(function_name == to_this_dag)
//    {
//        Assert(false, "AT TIME OF WRITING, THIS DOESN'T SEEM REACHABLE. MAYBE IF FUNCTION IS CALLING ITS SELF?")
//        found = true;
//        return nullptr;
//    }
//
//    auto assign_map = get_assign_map();
//    if(assign_map != nullptr) {
//        auto it = assign_map->find(under_this_var);
//        if(it != assign_map->end()){
//            Assert(false, "AT TIME OF WRITING, THIS DOESN'T SEEM REACHABLE. IF YOU GET THIS FAR YOU HAVE PROBABLY MISSED IT.")
//            assert(it->second == to_this_dag);
//            found = true;
//            return nullptr;
//        }
//    }
//
//    if(main_parent != nullptr) {
//        ret_so_far = main_parent->find_last_var_store_on_the_way_to(to_this_dag, under_this_var, found);
//        if(found) {
//            if(ret_so_far == nullptr)
//            {
//                if(var_store != nullptr)
//                {
//                    ret_so_far = var_store;
//                }
//            }
//        }
//    }
//
////            for(const auto& it: parents) {
////                bool local_found = false;
////                const VarStore* tmp_ret = it->find_last_var_store_on_the_way_to(dag_name, local_found);
////                if(tmp_ret != nullptr) {
////                    assert(local_found);
////                    if (ret_so_far != nullptr) {
////                        assert(ret_so_far == tmp_ret);
////                    }
////                    else {
////                        ret_so_far = tmp_ret;
////                    }
////                }
////                if(local_found) {
////                    if(var_store != nullptr) {
////                        if(ret_so_far == nullptr) {
////                            ret_so_far = var_store;
////                        }
////                        else
////                        {
////                            AssertDebug(false, "PUT SOME ASSERTS HERE. BASICALLY IT'S CONFUSING IF THERE ARE MULTIPLE VAR STORES.")
////                        }
////                    }
////                    found = true;
////                }
////            }
//    return ret_so_far;
//}

void check_that_all_dependencies_are_there(SketchFunction* almost_ret, ProgramEnvironment* original_env, bool is_root = true) {
    string name = almost_ret->get_dag()->get_name();
    if(!is_root) {
        assert(original_env->function_map.find(name) != original_env->function_map.end());
    }
    auto it = original_env->function_map.get_root_dag_reps().find(name);
    assert(it != original_env->function_map.get_root_dag_reps().end());
    for(const auto& replace_it : almost_ret->get_replace_map()) {
        assert(it->second->get_parents().find(replace_it.second) != it->second->get_parents().end());
        assert(original_env->function_map.find(replace_it.second) != original_env->function_map.end());
        assert(almost_ret->get_dependencies().find(replace_it.second) != almost_ret->get_dependencies().end());
        check_that_all_dependencies_are_there(almost_ret->get_dependencies().find(replace_it.second)->second, original_env, false);
    }
}

void add_dependencies_to_original_env(SketchFunction* almost_ret, ProgramEnvironment* new_env, ProgramEnvironment* original_env, bool is_root = true) {
    cout << "NOW ADDING DEPENDENCIES FOR: " << almost_ret->get_dag()->get_name() <<" : ";
    for(const auto& it: almost_ret->get_replace_map())
    {
        cout << it.second << endl;
    }
    cout << endl;

    //apply base case to every node.
    if(!almost_ret->env_was_swapped()) {
        assert(almost_ret->get_env() == new_env);
        almost_ret->hard_swap_env(original_env);
        assert(almost_ret->get_env() == original_env);
        string name = almost_ret->get_dag()->get_name();

        auto mirror_rep = almost_ret->get_mirror_rep();
        assert(mirror_rep->get_is_erased() || mirror_rep->get_is_superseded());

        new_env->function_map.erase(name);

        AssertDebug(mirror_rep->get_is_erased(), "THIS NEEDS TO BE ERASED, OTHERWISE YOU WILL HAVE TWO COPIES OF THE SAME NAME IN MEMORY. TODO: WHEN PERFORMIN PRODUCE_CONCRETIZE, ACTUALLY MAKE NON-CONCRETIZED CLONES OF ALL THE FUNCTIONS, AND APROPRIATELLY RENAME THEM. ");
        mirror_rep->unerase();

        original_env->function_map.reinsert(name);
        if(!is_root) {
            assert(original_env->function_map.find(name) == original_env->function_map.end());
            original_env->function_map.insert(name, almost_ret);
        }
    }
    else {
        assert(almost_ret->get_env() == new_env);
        almost_ret->reset_env_to_original();
        assert(almost_ret->get_env() == original_env);
        string name = almost_ret->get_dag()->get_name();
        new_env->function_map.erase(name);
        assert(original_env->function_map.get_root_dag_reps().find(name) != original_env->function_map.get_root_dag_reps().end());
        assert(original_env->function_map.get_root_dag_reps().find(name)->second->get_is_erased() == false);
//        original_env->function_map.reinsert(name);
        assert(!is_root);
        if(!is_root) {
            assert(original_env->function_map.find(name) != original_env->function_map.end());
//            original_env->function_map.insert(name, almost_ret);
        }
    }

    if(!almost_ret->get_replace_map().empty()) {
        assert(almost_ret->get_dependencies().empty());
        for(const auto& it: almost_ret->get_replace_map()) {
            AssertDebug(false, "THIS IS PROBABLY OK, BUT I ADDED 'to_add->increment_shared_ptr();' INSIDE add_dependency. TODO: TEST THAT THIS IS FINE.")
            almost_ret->add_dependency(new_env->function_map[it.second]);
        }
        for(const auto& it: almost_ret->get_dependencies()) {
            add_dependencies_to_original_env(it.second, new_env, original_env, false);
        }
    }
}

SketchFunction *
TransformPrimitive::reconstruct_sketch_function(const string &to_this_dag, const string &under_this_var,
                                                const FunctionMapTransformer *root) const {

//    cout << "trying to find " << to_this_dag << endl;

    const TransformPrimitive* the_dag = nullptr;

    if(function_name == to_this_dag) {
        if(!is_erased && !superseded)
        {
            auto ret = (*root->get_function_map())[to_this_dag];
//            ret->get_hole_var_store();  //debug code, it's memory-leaky
            return ret;
        }
        else
        {
            AssertDebug(false, "CURRENT THOUGH: YOU SHOULDN'T REACH HERE BC ALL THE DAGS THAT YOU MIGHT WANT TO RECONSTRUCT HAVE BEEN CONCRETIZED AND CLONED AND THEY EXIST IN THE VAR STORE.")
        }
        assert(is_erased || superseded);
//        cout << "found " << function_name << endl;
        the_dag = this;
    }
    else {
        AssertDebug(false, "DOUBLE CHECK IF THIS IS WHAT YOU REALLY WANT;");
        auto assign_map = get_assign_map();
        if (assign_map != nullptr) {
            auto it_name = assign_map->find(under_this_var);
            if (it_name != assign_map->end()) {
                assert(it_name->second == to_this_dag);
                assert(is_erased || superseded);
//                cout << "found " << function_name << endl;
                if (meta_type == _replace) {
                    assert(parents.find(to_this_dag) != parents.end());
                    the_dag = parents.at(to_this_dag);
                } else {
                    AssertDebug(false, "meta_type is " + transform_primitive_meta_type_name[meta_type] +
                                       ". Needs to be _replace, bc otherwise it must be Init, but Inits can't be erased. Or there is another replacing mechanism which needs to be added.");
                }
            }
        }
    }

    if(the_dag != nullptr) {
        assert(the_dag != nullptr);
        assert(the_dag->is_erased || the_dag->superseded);

        ProgramEnvironment* new_env = root->get_function_map()->get_env()->shallow_copy_w_new_blank_function_map();
        auto almost_ret =  the_dag->reconstruct_sketch_function(root, new_env);
//        almost_ret->get_hole_var_store(); // debug code, memory leaky
        new_env->function_map.check_consistency();

//        AssertDebug(false, "DEPENDENCIES SHOULD ALREADY BE IN THE FUNCTION MAP!!! ADD A FIELD TO SKETCH FUNCTION THAT REPRESENTS WHICH TRANSFORMER PRIMITIVE REPRESENTS IT!!!")
        add_dependencies_to_original_env(almost_ret, new_env, root->get_function_map()->get_env());
        check_that_all_dependencies_are_there(almost_ret, root->get_function_map()->get_env());

//            assert(almost_ret->get_num_shared_ptr() == 0);
//
//            assert(!almost_ret->env_was_swapped());
//            almost_ret->hard_swap_env(root->get_function_map()->get_env());
//            new_env->function_map.erase(almost_ret->get_dag()->get_name());
//            root->get_function_map()->get_env()->function_map.reinsert(almost_ret->get_dag()->get_name());

        vector<string> to_erase_and_swap_back;
        for(const auto& it: new_env->function_map) {
            if(it.second->env_was_swapped()) {
                to_erase_and_swap_back.push_back(it.first);
            }
            else {
                assert(it.second->get_num_shared_ptr() == 0);
            }
        }

        for(const auto& it : to_erase_and_swap_back) {
            (new_env->function_map)[it]->reset_env_to_original();
            new_env->function_map.erase(it);
        }

        assert(almost_ret->get_num_shared_ptr() == 0);
        new_env->function_map.clear_assert_num_shared_ptr_is_0();

        //THINK ABOUT HERE HOW/WHERE TO UTILIZE THE FUNCTION MAP THAT WAS CREATED BY reconstruct_sketch_function. (ret started off with a blank function map.)
//        almost_ret->swap_env(root->get_function_map()->get_env());
        assert(almost_ret->get_env() != new_env);
        assert(almost_ret->get_env() == root->get_function_map()->get_env());
        assert(almost_ret != nullptr);
        delete new_env;
        return almost_ret;
    }
    else {
        assert(the_dag == nullptr);
    }

    if(main_parent != nullptr) {
        SketchFunction* ret = main_parent->reconstruct_sketch_function(to_this_dag, under_this_var, root);
        if(ret != nullptr) {
            return ret;
        }
    }

    return nullptr;
}

SketchFunction *TransformPrimitive::reconstruct_sketch_function(const FunctionMapTransformer  * root,
                                                                ProgramEnvironment *new_env) const {

//    cout << "HERE: " << function_name << endl;
    if(current_new_env == new_env) {
        auto ret_it = new_env->function_map.find(function_name);
        assert(ret_it != new_env->function_map.end());
        auto ret = ret_it->second;
        return ret;
    }

    assert(current_new_env != new_env);

    current_new_env = new_env;

    if(!is_erased) {
        auto it = root->get_root_dag_reps().find(function_name);
        if(it->second == this) {
            assert(!superseded);
            cout << "BASE CASE: " << it->first << endl;
            auto ret = (*root->get_function_map())[function_name];
            assert(ret->get_env() != new_env);
            ret->swap_env(new_env);
            new_env->function_map.insert(ret->get_dag()->get_name(), ret);
            return ret;
        }
        else {
            assert(superseded);
//            AssertDebug(false, "IF THIS FAILS, IT MEANS THAT YOU HAVE FOUND A NON-ERASED FUNCTION WHICH IS NOT A REPRESENTATIVE OF IT'S NAME (IT'S BEEN OUTDATED).");
        }
    }

    switch (meta_type) {

        case _make_executable: {
            auto ret = main_parent->reconstruct_sketch_function(root, new_env);
            assert(ret->get_env() == new_env);
            for (const auto &it_parent: parents) {
                string parent_name = it_parent.first;
                SketchFunction *parent_skfunc = it_parent.second->reconstruct_sketch_function(root, new_env);
                assert(new_env->function_map.find(parent_skfunc->get_dag_name()) != new_env->function_map.end());
            }
            VarStore *var_store = get_var_store();
            assert(var_store != nullptr);
            const bool_node::Type *bool_node_type = get_concretization_type();
            assert((*bool_node_type) == bool_node::CTRL);
            ret->produce_concretization(var_store, *bool_node_type, false);
            ret->set_mirror_rep(this);
            assert(new_env->function_map.find(ret->get_dag()->get_name()) != new_env->function_map.end());

//            ret->get_hole_var_store(); // debug code, memory leaky

            return ret;
            break;
        }
        case _replace: {

            auto assign_map = get_assign_map();
            assert(assign_map != nullptr);
            assert(assign_map->size() == 1);

            string replace_with = assign_map->begin()->second;

            //first calc main
            auto ret = main_parent->reconstruct_sketch_function(root, new_env);
            assert(ret->get_env() == new_env);

            //then calc other parent
            {
                auto rep_it = root->get_root_dag_reps().find(replace_with);
                auto replace_with_dag = rep_it->second->reconstruct_sketch_function(root, new_env);

                if (new_env->function_map.find(replace_with) == new_env->function_map.end()) {
                    AssertDebug(false,
                                "Ideally inserting in the fmap will be handled at the end of the previous recursive call.");
                    new_env->function_map.insert(replace_with, replace_with_dag);
                }
            }

            ret->replace(assign_map->begin()->first, replace_with);
            assert(new_env->function_map.find(ret->get_dag()->get_name()) != new_env->function_map.end());

            ret->set_mirror_rep(this);
            return ret;
            break;
        }
        case _clone: {
            assert(parents.size() == 1);
            assert(parents.begin()->second == main_parent);

            if(!is_erased) {
                assert(superseded);
            }
            SketchFunction* almost_ret = main_parent->reconstruct_sketch_function(root, new_env);
            assert(almost_ret->get_env() == new_env);
            SketchFunction* ret = almost_ret->unit_clone(function_name);
            assert(ret->get_env() == new_env);
            assert(new_env->function_map.find(ret->get_dag()->get_name()) == new_env->function_map.end());
            new_env->function_map.insert(ret->get_dag()->get_name(), ret);
            assert(new_env->function_map.find(ret->get_dag()->get_name()) != new_env->function_map.end());

            ret->set_mirror_rep(this);
            return ret;
            break;
        }
        case _init:
            assert(superseded);
            assert(!is_erased);
            AssertDebug(false, "_inits should never be erased or superseded.")
            break;
        default:
            AssertDebug(false, "MISSING CASE.");
    }

    return nullptr;
}

string TransformPrimitive::parents_to_str() const {
    string ret;
    for(const auto& it:parents)
    {
        ret+=it.second->function_name+", ";
    }
    return ret;
}

string TransformPrimitive::children_to_str() const {
    string ret;
    for(const auto& it:children)
    {
        ret+=it->function_name+", ";
    }
    return ret;
}

TransformPrimitiveMetaType TransformPrimitive::get_meta_type() {
    return meta_type;
}

void TransformPrimitive::unerase(const TransformPrimitive *parent) const{
    assert(is_erased);
    is_erased = false;
    for(auto it: children)
    {
        if(it->function_name == function_name) {
            assert(it == parent);
        }
    }
    if(main_parent->function_name == function_name) {
        main_parent->unerase(this);
    }

}

const map<string, TransformPrimitive *> &TransformPrimitive::get_parents() const {
    return parents;
}

bool TransformPrimitive::get_is_superseded() const {
    return superseded;
}

TransformPrimitive *TransformPrimitive::get_main_parent() {
    return main_parent;
}

void TransformPrimitive::pretty_print(string& ret, map<string, string>* holes_to_values, const FunctionMapTransformer& fmt, map<string, map<string, string> >* running_assignment_map,
                                        set<TransformPrimitive*>* _visited) const {
    assert(false);
}

string TransformPrimitive::to_string() {
    return function_name + " | " + transform_primitive_meta_type_name[meta_type] + " | parents: " + parents_to_str() + " | children:" + children_to_str();
}

void ReplacePrimitive::pretty_print(string& ret, map<string, string>* holes_to_values,
        const FunctionMapTransformer &fmt,
        map<string, map<string, string> >* running_assignment_map,
        set<TransformPrimitive*>* visited) const {

    if(visited->find((TransformPrimitive *) this) != visited->end()) {
        return;
    }

    visited->insert((TransformPrimitive*)this);

    assert(assign_map.size() == 1);
    auto root_dag_reps = fmt.get_root_dag_reps();

    string var_name = assign_map.begin()->first;
    string val_name = assign_map.begin()->second;

    if(running_assignment_map->find(function_name) != running_assignment_map->end()) {
        if(running_assignment_map->at(function_name).find(var_name) != running_assignment_map->at(function_name).end()) {
            //var_name has been updated on the path to here with another val_name in the function with function name.
            //meaning that the current val_name is superseded.
            return main_parent->pretty_print(ret, holes_to_values,  fmt, running_assignment_map, visited);
        }
        else {
            running_assignment_map->at(function_name)[var_name] = val_name;
        }
    }
    else {
        (*running_assignment_map)[function_name] = assign_map;
    }

    assert(root_dag_reps.find(assign_map.begin()->second) != root_dag_reps.end());

    main_parent->pretty_print(ret, holes_to_values,  fmt, running_assignment_map, visited);
    root_dag_reps.at(assign_map.begin()->second)->pretty_print(ret, holes_to_values,  fmt, running_assignment_map, visited);
    ret += function_name + ".replace(" + "\"" + assign_map.begin()->first + "\"" + ", " + assign_map.begin()->second + ");" + "\n";

    assert(running_assignment_map->find(function_name) != running_assignment_map->end());
    assert(assign_map.size() == 1);
    assert(running_assignment_map->at(function_name).find(assign_map.begin()->first) != running_assignment_map->at(function_name).end());
    running_assignment_map->at(function_name).erase(assign_map.begin()->first);
    assert(running_assignment_map->at(function_name).find(assign_map.begin()->first) == running_assignment_map->at(function_name).end());
    if(running_assignment_map->at(function_name).empty()) {
        running_assignment_map->erase(function_name);
    }
}

void ConcretizePrimitive::pretty_print(string& ret, map<string, string>* holes_to_values, const FunctionMapTransformer &fmt,
                                         map<string, map<string, string>> *running_assignment_map,
                                         set<TransformPrimitive *> *visited) const  {

    if(visited->find((TransformPrimitive *) this) != visited->end()) {
        return;
    }

    visited->insert((TransformPrimitive *) this);


    bool has_main_parent = false;
    for (const auto &it: parents) {
        if (it.second != main_parent) {
            it.second->pretty_print(ret, holes_to_values,  fmt, running_assignment_map, visited);
        } else {
            assert(!has_main_parent);
            has_main_parent = true;
        }
    }
    assert(has_main_parent);
    main_parent->pretty_print(ret, holes_to_values,  fmt, running_assignment_map, visited);

    auto floats = fmt.get_function_map()->get_env()->get_floats();

    string var_store_str;
    if(var_store != nullptr) {
        map<string, string> ctrls_str_str = var_store->to_map_str_str(floats);

        const string init_var_store_string = "{";
        var_store_str = init_var_store_string;
        for (const auto& it: ctrls_str_str) {
            if (var_store_str != init_var_store_string) {
                var_store_str += ", ";
            }
            var_store_str += "\"" + it.first + "\"" + " : \"" + it.second + "\"";
            assert(holes_to_values->find(it.first) == holes_to_values->end());
            (*holes_to_values)[it.first] = it.second;

        }
        var_store_str += "}";
    }
    else
    {
        var_store_str = "{}";
    }
    ret += function_name + ".inplace_unit_concretize(" + var_store_str + ");" + "\n";
}

void
InitPrimitive::pretty_print(string& ret, map<string, string>* holes_to_values, const FunctionMapTransformer &fmt, map<string, map<string, string>> *running_assignment_map,
                            set<TransformPrimitive *> *visited) const {

    if(visited->find((TransformPrimitive *) this) != visited->end()) {
        return;
    }

    visited->insert((TransformPrimitive *) this);
    assert(main_parent == nullptr);

    auto root_dag_reps = fmt.get_root_dag_reps();

    for(const auto& it: assign_map) {
        string var_name = it.first;
        string val_name = it.second;

        bool print_parent = true;

        if (running_assignment_map->find(function_name) != running_assignment_map->end()) {
            if (running_assignment_map->at(function_name).find(var_name) !=
                running_assignment_map->at(function_name).end()) {
                //var_name has been updated on the path to here with another val_name in the function with function name.
                //meaning that the current val_name is superseded.
                //continue
                print_parent = false;
            }
        }

        if(print_parent) {
            auto next = root_dag_reps.at(val_name);
            next->pretty_print(ret, holes_to_values,  fmt, running_assignment_map, visited);
        }
        else {
            ret += "//" + var_name + " -> " + val_name + " superseded in " + function_name + "\n";
        }
    }

    const string init_hole_names_str = "[";
    string hole_names_str = init_hole_names_str;
    for (const auto &it: hole_names) {
        if (hole_names_str != init_hole_names_str) {
            hole_names_str += ", ";
        }
        hole_names_str += "\"" + it + "\"";
    }
    hole_names_str += "]";

    const string init_assign_map_str = "{";
    string assign_map_str = init_assign_map_str;
    for (const auto &it: assign_map) {
        if (assign_map_str != init_assign_map_str) {
            assign_map_str += ", ";
        }
        string key_col_val_str;
        key_col_val_str = "\"" + it.first + "\"" + " : " + "\"" + it.second + "\"";

        assign_map_str += key_col_val_str;
    }
    assign_map_str += "}";

    ret += function_name + " = declare(" + "\"" + function_name + "\"" + ", " + hole_names_str + ", " + assign_map_str + ");" + "\n";
}

void ClonePrimitive::pretty_print(string& ret, map<string, string>* holes_to_values, const FunctionMapTransformer &fmt,
                                    map<string, map<string, string>> *running_assignment_map,
                                    set<TransformPrimitive *> *visited) const {

    if(visited->find((TransformPrimitive *) this) != visited->end()) {
        return;
    }

    visited->insert((TransformPrimitive *) this);

    const string init_hole_renaming_map_str = "{";
    string hole_renaming_map_str = init_hole_renaming_map_str;
    for (const auto &it: hole_renaming_map) {
        if (hole_renaming_map_str != init_hole_renaming_map_str) {
            hole_renaming_map_str += ", ";
        }
        string key_col_val_str;
        key_col_val_str = "\"" + it.first + "\"" + " : " + "\"" + it.second + "\"";

        hole_renaming_map_str += key_col_val_str;
    }
    hole_renaming_map_str += "}";

    main_parent->pretty_print(ret, holes_to_values,  fmt, running_assignment_map, visited);
    ret += function_name + " = " + main_parent->get_function_name() + ".unit_clone(" + "\"" + function_name + "\"" + ", " + hole_renaming_map_str + ");" + "\n";
}