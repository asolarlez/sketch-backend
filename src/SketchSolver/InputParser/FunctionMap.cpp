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

void FunctionMap::insert(const string &name, SketchFunction *sketch_function)
{
    auto it = find(name);
    if(it == end()) {
        FunctionMapTransformer::insert(sketch_function->get_dag()->get_name(), sketch_function->get_dag()->get_ufun_names());
        map<string, SketchFunction *>::operator[](name) = sketch_function;
    }
    else
    {
        map<string, SketchFunction *>::erase(it);
        insert(name, sketch_function);
    }
}

SketchFunction *FunctionMap::produce_get(const string &from_dag, const string &subfunc_name) {
    const string& underlying_dag = find_subdag_name(from_dag, subfunc_name);
    auto it = find(underlying_dag);
    if(it != end()) {
        cout << "produce_get returns SketchFunction with name: " <<
            it->second->get_dag()->get_name() << " of underlying dag " << underlying_dag <<" hidden under name " << subfunc_name << endl;
        const VarStore* the_var_store = find_last_var_store_on_the_way_to(from_dag, underlying_dag);
//        AssertDebug(false, "NEED TO CONCRETIZE IT AS IT WAS from the point of view of 'from_dag'");
        if(the_var_store == nullptr) {
            cout << "VAR STORE IN NULLPTR" << endl;
            return it->second;
        }
        else
        {
            cout << "VAR STORE IN NOT NULLPTR" << endl;
            return it->second->produce_concretization(*the_var_store->clone(), bool_node::CTRL, true);
        }
    }
    else
    {
        AssertDebug(false, "TOOD: reconstruct the dag using the transformer");
    }
}

const VarStore *
FunctionMap::get_var_store_used_to_concretize_underlying_subdag(const string &from_dag, const string &under_this_name) {
    return find_last_var_store_on_the_way_to(
                    from_dag,
                    find_subdag_name(
                            from_dag,
                            under_this_name));
}


