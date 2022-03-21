//
// Created by kliment on 1/29/22.
//

#include "FunctionMap.h"
#include "SketchFunction.h"

const map<string, BooleanDAG *> * FunctionMap::to_boolean_dag_map() const{
    auto ret = new map<string, BooleanDAG*>();
    for(auto it:*this) {
        (*ret)[it.first] = it.second->get_dag();
    }
    return ret;
}

void FunctionMap::populate_boolean_dag_map(map<string, BooleanDAG*>& boolean_dag_map) const {
    for(auto it:*this) {
        boolean_dag_map[it.first] = it.second->get_dag();
    }
}


const FMTL::TransformPrimitive* FunctionMap::insert(const string &name, SketchFunction *sketch_function)
{

    assert(name == sketch_function->get_dag()->get_name());
    auto it = find(name);
    if(it == end()) {
        map<string, SketchFunction *>::operator[](name) = sketch_function;
        return FunctionMapTransformer::insert(sketch_function->get_dag()->get_name(), sketch_function->get_dag()->get_ufun_names(), sketch_function->get_unit_holes());
    }
    else
    {
        map<string, SketchFunction *>::erase(it);
        return insert(name, sketch_function);
    }
}

SketchFunction *FunctionMap::produce_get(const string &from_dag, const string &under_this_var) {
    const string& underlying_dag = find_subdag_name(from_dag, under_this_var);
    auto it = find(underlying_dag);
    if(it != end()) {
        return extract_sketch_function(from_dag, under_this_var, underlying_dag);
    }
    else
    {
        AssertDebug(false, "CHECK IF YOU CAN MERGE THE TWO BRANCES!!!");
//        cout << "FOUND from_dag " << from_dag << " under_this_var " << under_this_var << " RECONSTRUCTING NOW" << endl;
        return reconstruct_sketch_function(from_dag, under_this_var, underlying_dag);
    }
}

void FunctionMap::soft_clear_transformer() {
    FunctionMapTransformer::soft_clear();
}

void FunctionMap::clear_assert_num_shared_ptr_is_0() {
    vector<string> names;
    for(const auto& it: *this) {
        names.push_back(it.first);
    }
    for(int i = 0;i<names.size();i++) {
        string name = names[i];
        auto it = find(name);
        assert(it->first == name);
        assert(it != end());
        it->second->clear_assert_num_shared_ptr_is_0();
        assert(find(name) == end());
    }
    map::clear();
    assert(FunctionMapTransformer::empty());
    FunctionMapTransformer::soft_clear();
    delete this;
}

void FunctionMap::erase(const string &name) {
    auto it = find(name);
    if(it != end()) {
        map<string, SketchFunction *>::erase(it);
    }
    FunctionMapTransformer::erase(name);
}

void FunctionMap::check_consistency() {
    auto reps = get_root_dag_reps();
    auto erased = get_erased();
    for(const auto& it : *this)
    {
        assert(&it.second->get_env()->function_map == this);
        assert(reps.find(it.first) != reps.end());
        assert(erased.find(it.first) == erased.end());
    }
    FunctionMapTransformer::check_consistency();
}

FunctionMap::FunctionMap(ProgramEnvironment *_program_environment) : __program_environment(_program_environment), FunctionMapTransformer(this) {}

ProgramEnvironment *FunctionMap::get_env() {
    assert(&__program_environment->function_map == this);
    return __program_environment;
}

void FunctionMap::print() {
    for(const auto& it : *this) {
        assert(it.first == it.second->get_dag_name());
        cout << it.first << endl;
    }
    FMTL::FunctionMapTransformer::print();
}
