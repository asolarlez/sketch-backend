//
// Created by kliment on 1/29/22.
//

#include "FunctionMapTransformerLanguage.h"
#include "SketchFunction.h"

using namespace FMTL;

void FunctionMapTransformer::concretize(const string &function_name, VarStore &store, bool_node::Type type,
                                        const vector<string> *sub_functions) {
    
    assert(root_dag_reps.find(function_name) != root_dag_reps.end());
    assert(root_dag_reps[function_name]->get_function_name() == function_name);
    TransformPrimitive* new_primitive =
            new ConcretizePrimitive(function_name, store, type);
    add_parent(new_primitive, function_name, true);
    if(sub_functions != nullptr) {
        for (const auto &it: *sub_functions) {
            add_parent(new_primitive, it);
        }
    }
    program.insert(new_primitive);
    root_dag_reps[function_name] = new_primitive;
    assert(root_dag_reps[function_name]->get_function_name() == function_name);
}

void FunctionMapTransformer::replace_label_with_another(
        const string& function_name, const string &replace_this_str, const string &with_this_str) {
    
    auto main_parent_it = root_dag_reps.find(function_name);
    assert( main_parent_it != root_dag_reps.end());
    auto main_parent = main_parent_it->second;
    assert(main_parent->get_function_name() == function_name);

    auto new_primitive = new ReplacePrimitive(function_name, replace_this_str, with_this_str);
    add_parent(new_primitive, function_name, true);
    add_parent(new_primitive, with_this_str);
    program.insert(new_primitive);
    root_dag_reps[function_name] = new_primitive;
    assert(root_dag_reps[function_name]->get_function_name() == function_name);
}

void FunctionMapTransformer::clone(const string &original_function_name, const string &clone_function_name) {
    
    assert(root_dag_reps.find(clone_function_name) == root_dag_reps.end());
    auto new_primitive = new ClonePrimitive(original_function_name, clone_function_name);
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
}

void FunctionMapTransformer::insert(const string &new_function_name, const vector<string>& subfunction_names) {
    
    if(root_dag_reps.find(new_function_name) == root_dag_reps.end()) {
        auto new_primitive = new InitPrimitive(new_function_name, subfunction_names);
        program.insert(new_primitive);
        root_dag_reps[new_function_name] = new_primitive;
        assert(root_dag_reps[new_function_name]->get_function_name() == new_function_name);
    }
    else
    {
        assert(root_dag_reps[new_function_name]->get_function_name() == new_function_name);
    }
}

void FunctionMapTransformer::erase(const string &to_erase_name, bool assert_not_in_function_map) {
    if(assert_not_in_function_map) {
        assert(function_map->find(to_erase_name) == function_map->end());
    }
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
    }
    for(const auto& it: erased_root_dag_reps) {
        assert(function_map->find(it) == function_map->end());
    }
}

const VarStore *
FunctionMapTransformer::find_last_var_store_on_the_way_to(const string &from_dag, const string &under_this_var,
                                                          const string &to_this_dag) const  {
    auto it = root_dag_reps.find(from_dag);
    assert(it != root_dag_reps.end());
    bool found = false;
    const VarStore* ret = it->second->find_last_var_store_on_the_way_to(to_this_dag, under_this_var, found);
    AssertDebug(found, "this indicates that " + to_this_dag + " wasn't found starting from " + from_dag);
    return ret;
}

string FunctionMapTransformer::find_subdag_name(const string &from_dag,
                                                const string &find_what_dag_this_varname_maps_to) const
{
    auto it = root_dag_reps.find(from_dag);
    assert(it != root_dag_reps.end());
    bool print = false;
    if(print)
        cout << endl << "IN find_subdag_name " << from_dag << " " << find_what_dag_this_varname_maps_to << endl;
    TransformPrimitive* subdag = it->second->find_underlying_function(find_what_dag_this_varname_maps_to, this, print);
    assert(subdag != nullptr);
    string subdag_name = subdag->get_function_name();
    assert(root_dag_reps.find(subdag_name) != root_dag_reps.end());
    assert(root_dag_reps.at(subdag_name) == subdag);
    if(print)
        cout << "find_subdag_name(" + from_dag + ", " + find_what_dag_this_varname_maps_to + ") returns " << subdag << endl << endl;
    return subdag_name;
}

SketchFunction *
FunctionMapTransformer::reconstruct_sketch_function(
        const string &from_dag, const string &under_this_var, const string &to_this_dag) {
    auto it = root_dag_reps.find(from_dag);
    assert(it != root_dag_reps.end());
    bool found = false;
    auto ret = it->second->reconstruct_sketch_function(to_this_dag, under_this_var, found, this);
    AssertDebug(found, "this indicates that " + to_this_dag + " wasn't found starting from " + from_dag);
    return ret;
}

bool FunctionMapTransformer::contains_only_necessary()
{
    bool ret = true;
    for(auto it: program)
    {
        if(!it->get_is_erased())
        {
            if(function_map->find(it->get_function_name()) == function_map->end())
            {
                cout << "contains unnecessary: " << it->get_function_name() << endl;
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

void TransformPrimitive::set_is_erased(bool is_original)
{
    if(!is_erased) {
        is_erased = true;
        if(main_parent != nullptr)
        {
            if(main_parent->function_name == function_name) {
                main_parent->set_is_erased(false);
            }
        }
        for (auto it: children) {
            if (it->function_name == function_name) {
                if(is_original) {
                    assert(false);
                }
                assert(it->is_erased);
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
                    assert(root_dag_reps.find(parent_name)->second == remaining_rep);
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
            if(parent != parents.end()) {
                ret_so_far = parent->second;
            } else {
                assert(meta_type == _init);
                auto reps = root->get_root_dag_reps();
                auto it = reps.find(target_dag_name);
                assert(it != reps.end());
                return it->second;
            }
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

const VarStore *TransformPrimitive::find_last_var_store_on_the_way_to(
        const string &to_this_dag, const string &under_this_var, bool &found) const  {
    assert(!found);
    const VarStore* ret_so_far = nullptr;
    auto var_store = get_var_store();
    if(var_store != nullptr) {
        if(function_name == to_this_dag) {
            ret_so_far = var_store;
            found = true;
            return ret_so_far;
        }
    }

    if(function_name == to_this_dag)
    {
        found = true;
        return nullptr;
    }

    auto assign_map = get_assign_map();
    if(assign_map != nullptr) {
        auto it = assign_map->find(under_this_var);
        if(it != assign_map->end()){
            assert(it->second == to_this_dag);
            found = true;
            return nullptr;
        }
    }

    if(main_parent != nullptr) {
        ret_so_far = main_parent->find_last_var_store_on_the_way_to(to_this_dag, under_this_var, found);
        if(found) {
            if(ret_so_far == nullptr)
            {
                if(var_store != nullptr)
                {
                    ret_so_far = var_store;
                }
            }
        }
    }

//            for(const auto& it: parents) {
//                bool local_found = false;
//                const VarStore* tmp_ret = it->find_last_var_store_on_the_way_to(dag_name, local_found);
//                if(tmp_ret != nullptr) {
//                    assert(local_found);
//                    if (ret_so_far != nullptr) {
//                        assert(ret_so_far == tmp_ret);
//                    }
//                    else {
//                        ret_so_far = tmp_ret;
//                    }
//                }
//                if(local_found) {
//                    if(var_store != nullptr) {
//                        if(ret_so_far == nullptr) {
//                            ret_so_far = var_store;
//                        }
//                        else
//                        {
//                            AssertDebug(false, "PUT SOME ASSERTS HERE. BASICALLY IT'S CONFUSING IF THERE ARE MULTIPLE VAR STORES.")
//                        }
//                    }
//                    found = true;
//                }
//            }
    return ret_so_far;
}

SketchFunction *
TransformPrimitive::reconstruct_sketch_function(const string &to_this_dag, const string &under_this_var, bool& found, FunctionMapTransformer* root) {
    assert(!found);

//    cout << "trying to find " << to_this_dag << endl;

    TransformPrimitive* the_dag = nullptr;

    if(function_name == to_this_dag) {
        assert(is_erased);
//        cout << "found " << function_name << endl;
        the_dag = this;
        found = true;
    }
    else {
        auto assign_map = get_assign_map();
        if (assign_map != nullptr) {
            auto it_name = assign_map->find(under_this_var);
            if (it_name != assign_map->end()) {
                assert(it_name->second == to_this_dag);
                assert(is_erased);
//                cout << "found " << function_name << endl;
                if (meta_type == _replace) {
                    assert(parents.find(to_this_dag) != parents.end());
                    the_dag = parents[to_this_dag];
                    found = true;
                } else {
                    AssertDebug(false, "meta_type is " + transform_primitive_meta_type_name[meta_type] +
                                       ". Needs to be _replace, bc otherwise it must be Init, but Inits can't be erased. Or there is another replacing mechanism which needs to be added.");
                }
            }
        }
    }

    if(found) {
        assert(the_dag != nullptr);
        assert(the_dag->is_erased);

        ProgramEnvironment* new_env = root->get_function_map()->get_env()->shallow_copy_w_new_blank_function_map();
        auto almost_ret =  the_dag->reconstruct_sketch_function(root, new_env);
        assert(almost_ret->get_num_shared_ptr() == 0);

        assert(!almost_ret->env_was_swapped());
        almost_ret->hard_swap_env(root->get_function_map()->get_env());
        new_env->function_map.erase(almost_ret->get_dag()->get_name());

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
        root->get_function_map()->get_env()->function_map.reinsert(almost_ret->get_dag()->get_name());
        assert(almost_ret->get_env() != new_env);
        assert(almost_ret->get_env() == root->get_function_map()->get_env());
        assert(found);
        delete new_env;
        return almost_ret;
    }
    else {
        assert(the_dag == nullptr);
    }

    if(main_parent != nullptr) {
        SketchFunction* ret = main_parent->reconstruct_sketch_function(to_this_dag, under_this_var, found, root);
        if(found) {
            assert(ret != nullptr);
            assert(found);
            return ret;
        }
        else {
            assert(ret == nullptr);
        }
    }

    assert(!found);
    return nullptr;
}

SketchFunction *TransformPrimitive::reconstruct_sketch_function(FunctionMapTransformer *root,
                                                                ProgramEnvironment *new_env) {

    if(current_new_env == new_env) {
        auto ret_it = new_env->function_map.find(function_name);
        assert(ret_it != new_env->function_map.end());
        auto ret = ret_it->second;
        return ret;
    }
    current_new_env = new_env;

    if(!is_erased) {
        auto it = root->get_root_dag_reps().find(function_name);
        if(it->second == this) {
            auto ret = (*root->get_function_map())[function_name];
            assert(ret->get_env() != new_env);
            ret->swap_env(new_env);
            new_env->function_map.insert(ret->get_dag()->get_name(), ret);
            return ret;
        }
        else {
            assert(false);
        }
    }

    switch (meta_type) {

        case _concretize: {
            auto ret = main_parent->reconstruct_sketch_function(root, new_env);
            assert(ret->get_env() == new_env);
            for (const auto &it_parent: parents) {
                string parent_name = it_parent.first;
                SketchFunction *parent_skfunc = it_parent.second->reconstruct_sketch_function(root, new_env);
            }
            VarStore *var_store = get_var_store();
            assert(var_store != nullptr);
            const bool_node::Type *bool_node_type = get_concretization_type();
            assert((*bool_node_type) == bool_node::CTRL);
            ret->produce_concretization(*var_store, *bool_node_type, false);
            assert(new_env->function_map.find(ret->get_dag()->get_name()) != new_env->function_map.end());
            return ret;
            break;
        }
        case _replace: {
            AssertDebug(false, "CHECK NEXT ASSERT DEBUG.");
            auto assign_map = get_assign_map();
            assert(assign_map != nullptr);
            assert(assign_map->size() == 1);
            //TODO: add responsibility in the sketch function as well.
            //calculate
            if(false)
            {
                string replace_with = assign_map->begin()->second;
                auto it = parents.find(replace_with);
                assert(it != parents.end());
                auto replace_with_dag = it->second->reconstruct_sketch_function(root, new_env)->clone();
                // add responsibility.
            }
            else
            {
                AssertDebug(false, "TODO: need to add the calculate part.")
            }

            auto ret = main_parent->reconstruct_sketch_function(root, new_env);

            ret->replace(assign_map->begin()->first, assign_map->begin()->second);
            assert(ret->get_env() == new_env);
            assert(new_env->function_map.find(ret->get_dag()->get_name()) != new_env->function_map.end());

            return ret;
            break;
        }
        case _clone: {
            assert(parents.size() == 1);
            assert(parents.begin()->second == main_parent);

            auto ret = main_parent->reconstruct_sketch_function(root, new_env)->clone(function_name);
            assert(ret->get_env() == new_env);
            assert(new_env->function_map.find(ret->get_dag()->get_name()) == new_env->function_map.end());
            new_env->function_map.insert(ret->get_dag()->get_name(), ret);
            assert(new_env->function_map.find(ret->get_dag()->get_name()) != new_env->function_map.end());

            return ret;
            break;
        }
        case _init:
            AssertDebug(false, "_inits should never be erased.")
            break;
        default:
            AssertDebug(false, "missing cases.");
    }

    return nullptr;
}

string TransformPrimitive::parents_to_str() {
    string ret;
    for(auto it:parents)
    {
        ret+=it.second->function_name+", ";
    }
    return ret;
}

TransformPrimitiveMetaType TransformPrimitive::get_meta_type() {
    return meta_type;
}

void TransformPrimitive::unerase(TransformPrimitive* parent) {
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

