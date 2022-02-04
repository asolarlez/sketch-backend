//
// Created by kliment on 1/29/22.
//

#include "FunctionMapTransformerLanguage.h"

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

map<string, TransformPrimitive *> & FunctionMapTransformer::get_root_dag_reps() {
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

    map<string, TransformPrimitive*>& root_dag_reps = transformer->get_root_dag_reps();

    set<TransformPrimitive*>& program = transformer->get_program();

    TransformPrimitive* ret = nullptr;
    for(auto parent : parents)
    {
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
    for(auto parent:parents)
    {
        ret.push_back(parent->function_name);
    }
    return ret;
}

void TransformPrimitive::check_consistency(FunctionMapTransformer* transformer) {
    set<TransformPrimitive*>& program = transformer->get_program();
    auto root_dag_reps = transformer->get_root_dag_reps();
    for(auto it:parents)
    {
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
    for(auto it: parents)
    {
        if(!it->visited) {
            ret += it->num_reachable_nodes();
        }
    }
    return ret;
}
