//
// Created by kliment on 1/29/22.
//

#include "FunctionMapTransformerLanguage.h"

using namespace FMTL;

void FunctionMapTransformer::concretize(
        const string& function_name, VarStore &store, bool_node::Type type, bool do_deactivate_pcond, const vector<string>& sub_functions) {
    assert(where_my_kids_at.find(function_name) != where_my_kids_at.end());
    assert(where_my_kids_at[function_name]->get_function_name() == function_name);
    TransformPrimitive* new_primitive = new ConcretizePrimitive(function_name, store, type, do_deactivate_pcond);
    add_parent(new_primitive, function_name);
    for(const auto& it: sub_functions) {
        add_parent(new_primitive, it);
    }
    program.insert(new_primitive);
    where_my_kids_at[function_name] = new_primitive;
    assert(where_my_kids_at[function_name]->get_function_name() == function_name);
}

void FunctionMapTransformer::replace_label_with_another(
        const string& function_name, const string &replace_this_str, const string &with_this_str) {
    assert(where_my_kids_at.find(function_name) != where_my_kids_at.end());
    assert(where_my_kids_at[function_name]->get_function_name() == function_name);
    auto new_primitive = new ReplacePrimitive(function_name, replace_this_str, with_this_str);
    add_parent(new_primitive, function_name);
    program.insert(new_primitive);
    where_my_kids_at[function_name] = new_primitive;
    assert(where_my_kids_at[function_name]->get_function_name() == function_name);
}

void FunctionMapTransformer::clone(const string &original_function_name, const string &clone_function_name) {
    assert(where_my_kids_at.find(clone_function_name) == where_my_kids_at.end());
    auto new_primitive = new ClonePrimitive(original_function_name, clone_function_name);
    add_parent(new_primitive, original_function_name);
    program.insert(new_primitive);
    where_my_kids_at[clone_function_name] = new_primitive;
    assert(where_my_kids_at[clone_function_name]->get_function_name() == clone_function_name);
}

void FunctionMapTransformer::insert(const string &new_function_name) {
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
    assert(where_my_kids_at.find(to_erase_name) != where_my_kids_at.end());
    assert(where_my_kids_at[to_erase_name]->get_function_name() == to_erase_name);
    where_my_kids_at[to_erase_name]->erase(this);
}

set<TransformPrimitive *> &FunctionMapTransformer::get_program() {
    return program;
}

map<string, TransformPrimitive *> & FunctionMapTransformer::get_where_my_kids_at() {
    return where_my_kids_at;
}

TransformPrimitive *TransformPrimitive::erase(FunctionMapTransformer *transformer)
{
    map<string, TransformPrimitive*>& where_my_kids_at = transformer->get_where_my_kids_at();
    is_erased = true;
    if(!children.empty()) {
        return this;
    }
    assert(children.empty());
    for(auto parent : parents)
    {
        auto this_child = parent->children.find(this);
        assert(this_child != parent->children.end());
        parent->children.erase(this_child);
    }
    TransformPrimitive* ret = nullptr;
    for(auto parent : parents)
    {
        if(parent->is_erased || parent->function_name == function_name) {
            string parent_name = parent->function_name;
            assert(where_my_kids_at.find(parent_name) != where_my_kids_at.end());
            TransformPrimitive* remaining_rep = parent->erase(transformer);
            if(where_my_kids_at.find(parent_name) != where_my_kids_at.end()) {
                if (remaining_rep != nullptr) {
                    where_my_kids_at[parent_name] = remaining_rep;
                    if (parent_name == function_name) {
                        ret = remaining_rep;
                    }
                } else {
                    where_my_kids_at.erase(parent_name);
                }
            }
            else {
                assert(remaining_rep == nullptr);
            }
        }
    }
    set<TransformPrimitive*>& program = transformer->get_program();
    program.erase(this);
    delete this;
    return ret;
}