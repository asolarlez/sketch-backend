//
// Created by kliment on 1/29/22.
//

#include "FunctionMapTransformerLanguage.h"
#include "SketchFunction.h"

using namespace FMTL;

const bool omit_function_map_transformer = false;

void FunctionMapTransformer::concretize(const string &function_name, VarStore &store, bool_node::Type type,
                                        const vector<string> *sub_functions) {
    if(omit_function_map_transformer) { return; }
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
    if(omit_function_map_transformer) { return; }
    assert(root_dag_reps.find(function_name) != root_dag_reps.end());
    assert(root_dag_reps[function_name]->get_function_name() == function_name);
    auto new_primitive = new ReplacePrimitive(function_name, replace_this_str, with_this_str);
    add_parent(new_primitive, function_name, true);
    add_parent(new_primitive, with_this_str);
    program.insert(new_primitive);
    root_dag_reps[function_name] = new_primitive;
    assert(root_dag_reps[function_name]->get_function_name() == function_name);
}

void FunctionMapTransformer::clone(const string &original_function_name, const string &clone_function_name) {
    if(omit_function_map_transformer) { return; }
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
    if(omit_function_map_transformer) { return; }
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

void FunctionMapTransformer::erase(const string &to_erase_name) {
    if(omit_function_map_transformer) { return; }
    assert(root_dag_reps.find(to_erase_name) != root_dag_reps.end());
    assert(root_dag_reps[to_erase_name]->get_function_name() == to_erase_name);
    auto to_erase = root_dag_reps[to_erase_name];
//    check_consistency();
    to_erase->erase(this);
//    check_consistency();
    erased_root_dag_reps.insert(to_erase_name);
}

set<TransformPrimitive *> &FunctionMapTransformer::get_program() {
    if(omit_function_map_transformer) { assert(false); }
    return program;
}

const map<string, TransformPrimitive *> & FunctionMapTransformer::get_root_dag_reps() const {
    if(omit_function_map_transformer) { assert(false); }
    return root_dag_reps;
}

void FunctionMapTransformer::check_consistency() {
    if(omit_function_map_transformer) { return; }
    for(auto it: program) {
        assert(root_dag_reps.find(it->get_function_name()) != root_dag_reps.end());
        it->check_consistency(this);
    }
    for(auto it: root_dag_reps) {
        assert(it.first == it.second->get_function_name());
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


void TransformPrimitive::set_is_erased(bool is_original)
{
    if(!is_erased) {
        is_erased = true;
        for (auto it_parent: parents) {
            auto it = it_parent.second;
            if (it->function_name == function_name) {
                it->set_is_erased(false);
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
    for(auto it_parent : parents)
    {
        auto parent = it_parent.second;
        auto this_child = parent->children.find(this);
        assert(this_child != parent->children.end());
        int prev_num_children = parent->children.size();
        parent->children.erase(this_child);
        assert(prev_num_children-1 == parent->children.size());
    }

    auto root_dag_reps = transformer->get_root_dag_reps();

    set<TransformPrimitive*>& program = transformer->get_program();

    TransformPrimitive* ret = nullptr;
    for(auto it_parent : parents)
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

            if(parent_name == function_name)
            {
                assert(ret == nullptr);
                ret = remaining_rep;
            }

        }
    }

    parents.clear();

    if(root_dag_reps.find(function_name) != root_dag_reps.end()) {
        if(root_dag_reps.at(function_name) == this) {
            if(ret == nullptr){
                root_dag_reps.erase(function_name);
            }
            else {
                root_dag_reps[function_name] = ret;
            }
        }
    }
    else {
        assert(false);
    }

    program.erase(this);

    if(get_var_store() != nullptr) {
        assert(meta_type == _concretize);
        get_var_store()->clear();
    }
    else {
        assert(meta_type != _concretize);
    }

    if(get_assign_map() != nullptr) {
        assert(meta_type == _replace);
        delete get_assign_map();
    }
    else {
        assert(meta_type != _replace);
    }

    delete this;
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
        auto ret =  the_dag->reconstruct_sketch_function(root, new FunctionMap());
        //todo probably need to erase temporary_function_map
        assert(found);
        return new SketchFunction(ret->get_dag(), ret->get_env()->shallow_copy_w_new_function_map(root->get_function_map()), ret->get_same_soluton());
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
                                                                FunctionMap *new_function_map) {

    if(current_local_function_map == new_function_map) {
        auto ret_it = new_function_map->find(function_name);
        assert(ret_it != new_function_map->end());
        auto ret = ret_it->second;
        return ret;
    }
    current_local_function_map = new_function_map;

    if(!is_erased) {
        auto it = root->get_root_dag_reps().find(function_name);
        if(it->second == this) {
            auto to_soft_copy = (*root->get_function_map())[function_name];
            auto new_env = to_soft_copy->get_env()->shallow_copy_w_new_function_map(new_function_map);
            auto shared_dag = to_soft_copy->get_dag();
            assert(to_soft_copy->solution_is_null());
            auto ret = new SketchFunction(shared_dag, new_env);
            new_env->function_map.insert(shared_dag->get_name(), ret);
            return ret;
        }
        else {
            assert(false);
        }
    }

    switch (meta_type) {

        case _concretize: {
            auto ret = main_parent->reconstruct_sketch_function(root, new_function_map);
            FunctionMap& local_function_map = ret->get_env()->function_map;
            assert(&local_function_map == new_function_map);
            for (const auto &it_parent: parents) {
                auto parent = it_parent.second;
                string parent_name = it_parent.first;
                assert(parent_name == parent->function_name);
                SketchFunction *parent_skfunc = parent->reconstruct_sketch_function(root, new_function_map);
                assert(parent_skfunc->get_dag()->get_name() == parent_name);
                assert(local_function_map.find(parent_skfunc->get_dag()->get_name()) !=
                               local_function_map.end());
                local_function_map.insert(parent_skfunc->get_dag()->get_name(), parent_skfunc);
            }
            VarStore *var_store = get_var_store();
            assert(var_store != nullptr);
            const bool_node::Type *bool_node_type = get_concretization_type();
            assert((*bool_node_type) == bool_node::CTRL);
            ret->produce_concretization(*var_store, *bool_node_type, false, &local_function_map);
            assert(local_function_map.find(ret->get_dag()->get_name()) != local_function_map.end());
            assert(ret->get_dag()->get_name() == function_name);
            return ret;
            break;
        }
        case _replace: {
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
                auto replace_with_dag = it->second->reconstruct_sketch_function(root, new_function_map)->clone();
                // add responsibility.
            }
            else
            {
                AssertDebug(false, "TODO: need to add the calculate part.")
            }

            auto ret = main_parent->reconstruct_sketch_function(root, new_function_map);

            ret->replace(assign_map->begin()->first, assign_map->begin()->second);
            FunctionMap& local_function_map = ret->get_env()->function_map;
            assert(&local_function_map == new_function_map);
            assert(local_function_map.find(ret->get_dag()->get_name()) != local_function_map.end());
            assert(ret->get_dag()->get_name() == function_name);

            return ret;
            break;
        }
        case _clone: {
            assert(parents.size() == 1);
            assert(parents.begin()->second == main_parent);

            auto ret = main_parent->reconstruct_sketch_function(root, new_function_map)->clone(function_name);
            new_function_map->insert(ret->get_dag()->get_name(), ret);

            FunctionMap& local_function_map = ret->get_env()->function_map;
            assert(&local_function_map == new_function_map);
            assert(local_function_map.find(ret->get_dag()->get_name()) != local_function_map.end());
            assert(ret->get_dag()->get_name() == function_name);

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

