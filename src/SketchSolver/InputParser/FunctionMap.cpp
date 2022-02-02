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

void FunctionMap::insert(const string &name, SketchFunction *sketch_function_map)
{
    auto it = find(name);
    if(it == end()) {
        FunctionMapTransformer::insert(sketch_function_map->get_dag()->get_name());
        map<string, SketchFunction *>::operator[](name) = sketch_function_map;
    }
    else
    {
        map<string, SketchFunction *>::erase(it);
        insert(name, sketch_function_map);
    }
}