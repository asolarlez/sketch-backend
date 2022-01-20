//
// Created by kliment on 12/20/21.
//

#include "SketchFunction.h"

#include <utility>

SketchFunction *SketchFunction::produce_concretization(VarStore &var_store, bool_node::Type var_type, bool do_deactivate_pcond,
                                                       bool do_clone)     {
    if(new_way)
    {
        assert(root_dag != nullptr);
        if(do_clone)
        {
            BooleanDAG* concretized_root_dag = root_dag->clone();
            env->doInline(*concretized_root_dag, var_store, var_type, do_deactivate_pcond);
            SketchFunction* ret = new SketchFunction(concretized_root_dag, nullptr, env);
        }
        else
        {
            env->doInline(*root_dag, var_store, var_type, do_deactivate_pcond);
            return this;
        }
    }
    else
    {
        assert(!do_deactivate_pcond);
        BooleanDAG* concretized_unrolled_dag;
        concretized_unrolled_dag = hardCodeINode(root_dag, var_store, var_type, env->get_floats());

        if(do_clone)
        {
            return new SketchFunction(concretized_unrolled_dag, nullptr, env);
        }
        else
        {
            root_dag->clear();
            root_dag = concretized_unrolled_dag;
            return this;
        }
    }
}

SketchFunction *SketchFunction::clone() {
    if(original_dag != nullptr)
    {
        assert(root_dag != nullptr);
        return new SketchFunction(root_dag->clone(), original_dag->clone(), env);
    }
    else
    {
        return new SketchFunction(root_dag->clone(), nullptr, env);
    }
}

void SketchFunction::clear()
{
    if(original_dag != nullptr)
    {
        original_dag->clear();
        delete original_dag;
        original_dag = NULL;
    }

    if(it_is_in_this_function_map != nullptr)
    {
        auto it = it_is_in_this_function_map->find(root_dag->get_name());
        assert(it != it_is_in_this_function_map->end());
        it_is_in_this_function_map->erase(it);

        auto it2 = get_env()->functionMap.find(root_dag->get_name());
        assert(it2 != get_env()->functionMap.end());
        get_env()->functionMap.erase(it2);
    }

    int prev_num = BooleanDAG::get_allocated().size();
    assert(root_dag != nullptr);
    root_dag->clear();
    assert(prev_num-1 == BooleanDAG::get_allocated().size());
    root_dag = nullptr;
    delete this;
}

//SketchFunction *SketchFunction::produce_replace(const string& replace_this, const string &with_this) {
//    assert(new_way);
//    SketchFunction* ret = clone();
//    ret->replace(replace_this, with_this);
//    return ret;
//}

void SketchFunction::replace(const string& replace_this, const string &with_this) {
    assert(new_way);
    assert(root_dag != nullptr);
    root_dag->replace_label_with_another(replace_this, with_this);
}

void SketchFunction::in_function_map(map<string, SketchFunction *> *map) {
    if(it_is_in_this_function_map == nullptr) {
        it_is_in_this_function_map = map;
    }
    else {
        assert(it_is_in_this_function_map == map);
    }
}


