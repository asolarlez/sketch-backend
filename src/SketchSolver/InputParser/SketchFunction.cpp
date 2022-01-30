//
// Created by kliment on 12/20/21.
//

#include "SketchFunction.h"

#include <utility>

SketchFunction *SketchFunction::produce_concretization(VarStore &var_store, bool_node::Type var_type, bool do_deactivate_pcond,
                                                       bool do_clone, bool update_transformer)     {
//    solution->produce_concretization(var_store, var_type, do_deactivate_pcond, do_clone);
    if(do_clone)
    {
        return clone(update_transformer)->produce_concretization(var_store, var_type, do_deactivate_pcond, false, update_transformer);
//        if(new_way)
//        {
//            assert(root_dag != nullptr);
////            SketchFunction* ret = this->clone();
////            ret->produce_concretization(var_store, var_type, do_deactivate_pcond, false);
//            BooleanDAG* concretized_root_dag = root_dag->clone();
//            env->doInline(*concretized_root_dag, var_store, var_type, do_deactivate_pcond);
//            SketchFunction* ret = new SketchFunction(concretized_root_dag, nullptr, env);
//            return ret;
//        }
//        else
//        {
//            assert(!do_deactivate_pcond);
//            BooleanDAG* concretized_unrolled_dag;
//            concretized_unrolled_dag = hardCodeINode(root_dag, var_store, var_type, env->get_floats());
//            return new SketchFunction(concretized_unrolled_dag, nullptr, env);
//        }
    }
    else {
        vector<string> inlined_functions;
        if (new_way) {
            assert(root_dag != nullptr);
            env->doInline(*root_dag, var_store, var_type, do_deactivate_pcond, inlined_functions);
        } else {
            assert(!do_deactivate_pcond);
            BooleanDAG *concretized_unrolled_dag;
            concretized_unrolled_dag = hardCodeINode(root_dag, var_store, var_type, env->get_floats());
            root_dag->clear();
            root_dag = concretized_unrolled_dag;
        }

        if(update_transformer) {
            get_env()->function_map.concretize(
                    get_dag()->get_name(), var_store, var_type, do_deactivate_pcond,
                    get_env()->function_map.get_function_names());
        }
        return this;
    }
}

SketchFunction *SketchFunction::clone(bool update_transformer) {

    BooleanDAG* cloned_dag = root_dag->clone();

    if(update_transformer) {
        get_env()->function_map.clone(get_dag()->get_name(), cloned_dag->get_name());
    }
    if(solution != nullptr) {
        if (original_dag != nullptr) {
            assert(root_dag != nullptr);
            return new SketchFunction(cloned_dag, original_dag->clone(), env,
                                      new SolverLanguagePrimitives::HoleAssignment(solution));
        } else {
            return new SketchFunction(cloned_dag, nullptr, env,
                                      new SolverLanguagePrimitives::HoleAssignment(solution));
        }
    }
    else
    {
        if (original_dag != nullptr) {
            assert(root_dag != nullptr);
            return new SketchFunction(cloned_dag, original_dag->clone(), env, nullptr);
        } else {
            return new SketchFunction(cloned_dag, nullptr, env, nullptr);
        }
    }
}

void SketchFunction::clear(bool update_transformer)
{
    if(original_dag != nullptr)
    {
        original_dag->clear();
        delete original_dag;
        original_dag = NULL;
    }

    get_env()->function_map.erase(root_dag->get_name(), update_transformer);

    int prev_num = BooleanDAG::get_allocated().size();
    assert(root_dag != nullptr);
    root_dag->clear();
    assert(prev_num-1 == BooleanDAG::get_allocated().size());
    root_dag = nullptr;
    delete this;
}

void SketchFunction::replace(const string& replace_this, const string &with_this) {
    assert(new_way);
    assert(root_dag != nullptr);
    get_env()->function_map.replace_label_with_another(get_dag()->get_name(), replace_this, with_this);
    root_dag->replace_label_with_another(replace_this, with_this);
}
