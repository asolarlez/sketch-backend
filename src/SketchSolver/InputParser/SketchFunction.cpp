//
// Created by kliment on 12/20/21.
//

#include "SketchFunction.h"
#include "VarStore.h"
#include "ProgramEnvironment.h"

SketchFunction *SketchFunction::produce_concretization(VarStore &var_store, bool_node::Type var_type, bool do_deactivate_pcond,
                                                       bool do_clone)     {
    if(new_way)
    {
        if(do_clone)
        {
            BooleanDAG* concretized_root_dag = root_dag->clone();
            env->doInline(*concretized_root_dag, var_store, var_type, do_deactivate_pcond);
            return new SketchFunction(concretized_root_dag, nullptr, env);
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
    int prev_num = BooleanDAG::get_allocated().size();
    root_dag->clear();
    assert(prev_num-1 == BooleanDAG::get_allocated().size());
    root_dag = nullptr;
    delete this;
}
