//
// Created by kliment on 1/29/22.
//

#include "FunctionMapTransformerLanguage.h"

using namespace FMTL;

const bool omit_function_map_transformer = false;

void FunctionMapTransformer::concretize(const string &function_name, VarStore &store, bool_node::Type type,
                                        const vector<string> *sub_functions) {
    if(omit_function_map_transformer) { return; }
    assert(where_my_kids_at.find(function_name) != where_my_kids_at.end());
    assert(where_my_kids_at[function_name]->get_function_name() == function_name);
    TransformPrimitive* new_primitive =
            new ConcretizePrimitive(function_name, store, type);
    add_parent(new_primitive, function_name);
    if(sub_functions != nullptr) {
        for (const auto &it: *sub_functions) {
            add_parent(new_primitive, it);
        }
    }
    program.insert(new_primitive);
    where_my_kids_at[function_name] = new_primitive;
    assert(where_my_kids_at[function_name]->get_function_name() == function_name);
}

void FunctionMapTransformer::replace_label_with_another(
        const string& function_name, const string &replace_this_str, const string &with_this_str) {
    if(omit_function_map_transformer) { return; }
    assert(where_my_kids_at.find(function_name) != where_my_kids_at.end());
    assert(where_my_kids_at[function_name]->get_function_name() == function_name);
    auto new_primitive = new ReplacePrimitive(function_name, replace_this_str, with_this_str);
    add_parent(new_primitive, function_name);
    add_parent(new_primitive, with_this_str);
    program.insert(new_primitive);
    where_my_kids_at[function_name] = new_primitive;
    assert(where_my_kids_at[function_name]->get_function_name() == function_name);
}

void FunctionMapTransformer::clone(const string &original_function_name, const string &clone_function_name) {
    if(omit_function_map_transformer) { return; }
    assert(where_my_kids_at.find(clone_function_name) == where_my_kids_at.end());
    auto new_primitive = new ClonePrimitive(original_function_name, clone_function_name);
    add_parent(new_primitive, original_function_name);
    program.insert(new_primitive);
    where_my_kids_at[clone_function_name] = new_primitive;
    assert(where_my_kids_at[clone_function_name]->get_function_name() == clone_function_name);

    cout << "new clone! " << original_function_name << " -> " << clone_function_name << endl;
    cout << "program.size() " << program.size() << endl;
    cout << "where_my_kids_at.size() " << where_my_kids_at.size() << endl;
    int erased_kids = 0;
    for(auto it: where_my_kids_at)
    {
        erased_kids+=it.second->get_is_erased();
    }
    cout << "non_erased_kids " << where_my_kids_at.size() - erased_kids << endl;
    int dag_size = calc_dag_size();
    cout << "dag_size() " << calc_dag_size() << endl;
    assert(dag_size == program.size());
    cout << endl;
}

void FunctionMapTransformer::insert(const string &new_function_name) {
    if(omit_function_map_transformer) { return; }
    if(where_my_kids_at.find(new_function_name) == where_my_kids_at.end()) {
        auto new_primitive = new InitPrimitive(new_function_name);
        program.insert(new_primitive);
        where_my_kids_at[new_function_name] = new_primitive;
        assert(where_my_kids_at[new_function_name]->get_function_name() == new_function_name);
    }
    else
    {
        assert(where_my_kids_at[new_function_name]->get_function_name() == new_function_name);
    }
}

void FunctionMapTransformer::erase(const string &to_erase_name) {
    if(omit_function_map_transformer) { return; }
    assert(where_my_kids_at.find(to_erase_name) != where_my_kids_at.end());
    assert(where_my_kids_at[to_erase_name]->get_function_name() == to_erase_name);
    auto to_erase = where_my_kids_at[to_erase_name];
    check_consistency();
    to_erase->erase(this);
    check_consistency();
}

set<TransformPrimitive *> &FunctionMapTransformer::get_program() {
    if(omit_function_map_transformer) { assert(false); }
    return program;
}

map<string, TransformPrimitive *> & FunctionMapTransformer::get_where_my_kids_at() {
    if(omit_function_map_transformer) { assert(false); }
    return where_my_kids_at;
}

const VarStoreTreeNode *FunctionMapTransformer::compile_var_store_tree(const string &function_name) {
    if(omit_function_map_transformer) { assert(false); }
    assert(where_my_kids_at.find(function_name) != where_my_kids_at.end());
    const VarStoreTreeNode * ret = where_my_kids_at[function_name]->compile_var_store_tree();
    return ret;
}

void FunctionMapTransformer::check_consistency() {
    if(omit_function_map_transformer) { return; }
    for(auto it: program) {
        assert(where_my_kids_at.find(it->get_function_name()) != where_my_kids_at.end());
        it->check_consistency(this);
    }
    for(auto it: where_my_kids_at) {
        assert(it.first == it.second->get_function_name());
    }
}

void TransformPrimitive::set_is_erased(bool is_original)
{
    if(!is_erased) {
        is_erased = true;
        for (auto it: parents) {
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
    for(auto parent : parents)
    {
        auto this_child = parent->children.find(this);
        assert(this_child != parent->children.end());
        int prev_num_children = parent->children.size();
        parent->children.erase(this_child);
        assert(prev_num_children-1 == parent->children.size());
    }

    map<string, TransformPrimitive*>& where_my_kids_at = transformer->get_where_my_kids_at();

    set<TransformPrimitive*>& program = transformer->get_program();

    TransformPrimitive* ret = nullptr;
    for(auto parent : parents)
    {
        if(parent->is_erased) {
            string parent_name = parent->function_name;
            if(program.find(parent) == program.end()) {
                assert(where_my_kids_at.find(parent_name) == where_my_kids_at.end());
                assert(parent_name != function_name);
                continue;
            }
            else
            {
                assert(where_my_kids_at.find(parent_name) != where_my_kids_at.end());
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

    if(_result_var_store_tree != nullptr)
    {
        delete _result_var_store_tree;
        _result_var_store_tree = nullptr;
    }

    if(where_my_kids_at.find(function_name) != where_my_kids_at.end()) {
        if(where_my_kids_at.at(function_name) == this) {
            if(ret == nullptr){
                where_my_kids_at.erase(function_name);
            }
            else {
                where_my_kids_at[function_name] = ret;
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

const VarStoreTreeNode * TransformPrimitive::compile_var_store_tree()
{
    if(result_var_store_tree_is_calculated)
    {
        return _result_var_store_tree;
    }

    VarStoreTreeNode* tmp_ret = new VarStoreTreeNode(function_name, get_var_store(), get_assign_map());

    for(auto parent:parents)
    {
        const VarStoreTreeNode* parent_var_store = parent->compile_var_store_tree();
        if(parent_var_store != nullptr) {
            assert(tmp_ret->find(parent->function_name) == tmp_ret->end());
            (*tmp_ret)[parent->function_name] = parent_var_store;
        }
    }

    if((tmp_ret->get_var_store() == nullptr) && (tmp_ret->get_assign_map() == nullptr))
    {
        if(tmp_ret->empty())
        {
            _result_var_store_tree = tmp_ret;
        }
        else
        {
            if(tmp_ret->size() == 1 && tmp_ret->get_function_name() == tmp_ret->begin()->second->get_function_name())
            {
                _result_var_store_tree = tmp_ret->begin()->second;
                assert(_result_var_store_tree != nullptr);
            }
            else
            {
                _result_var_store_tree = tmp_ret;
            }
        }
    }
    else
    {
        _result_var_store_tree = tmp_ret;
    }

    if(meta_type == _concretize) {
        assert(_result_var_store_tree != nullptr);
        assert(_result_var_store_tree->get_var_store() != nullptr);
    }

    if(meta_type == _replace) {
        assert(_result_var_store_tree != nullptr);
        assert(_result_var_store_tree->get_assign_map() != nullptr);
    }

    result_var_store_tree_is_calculated = true;
    return _result_var_store_tree;
}

TransformPrimitiveMetaType TransformPrimitive::get_primitive_type() const {
    return meta_type;
}

vector<string> TransformPrimitive::get_inlined_fs() {
    vector<string> ret;
    for(auto parent:parents)
    {
        ret.push_back(parent->function_name);
    }
    return ret;
}

void TransformPrimitive::check_consistency(FunctionMapTransformer* transformer) {
    set<TransformPrimitive*>& program = transformer->get_program();
    auto where_my_kids_at = transformer->get_where_my_kids_at();
    for(auto it:parents)
    {
        assert(program.find(it) != program.end());
        assert(where_my_kids_at.find(it->function_name) != where_my_kids_at.end());
    }
    for(auto it:children)
    {
        assert(program.find(it) != program.end());
        assert(where_my_kids_at.find(it->function_name) != where_my_kids_at.end());
    }
}

int TransformPrimitive::num_reachable_nodes() {
    assert(!visited);
    visited = true;
    int ret = 1;
    for(auto it: parents)
    {
        if(!it->visited) {
            ret += it->num_reachable_nodes();
        }
    }
    return ret;
}
