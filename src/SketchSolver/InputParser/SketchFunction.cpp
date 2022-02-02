//
// Created by kliment on 12/20/21.
//

#include "SketchFunction.h"

#include <utility>

SketchFunction *SketchFunction::produce_concretization(VarStore &var_store, bool_node::Type var_type, bool do_clone) {
    if(do_clone) {
        return clone()->produce_concretization(var_store, var_type, false);
    }
    else {
        vector<string>* inlined_functions = nullptr;

        inline_this_dag(var_store, var_type, inlined_functions);

//        if (new_way) {
//            assert(root_dag != nullptr);
//            env->doInline(*root_dag, var_store, var_type, inlined_functions);
//        } else {
//            BooleanDAG *concretized_unrolled_dag;
//            concretized_unrolled_dag = hardCodeINode(root_dag, var_store, var_type, env->get_floats());
//            root_dag->clear();
//            root_dag = concretized_unrolled_dag;
//        }

        get_env()->function_map.concretize(
                get_dag()->get_name(), var_store, var_type, inlined_functions);

        delete inlined_functions;

        return this;
    }
}

SketchFunction *SketchFunction::clone() {

    BooleanDAG* cloned_dag = get_dag()->clone();

    get_env()->function_map.clone(get_dag()->get_name(), cloned_dag->get_name());

    if(solution != nullptr) {
        return new SketchFunction(
                cloned_dag, get_env(), new SolverLanguagePrimitives::HoleAssignment(solution));
    }
    else
    {
        return new SketchFunction(cloned_dag, get_env(), nullptr);
    }
}

void SketchFunction::clear()
{
    get_env()->function_map.erase(get_dag()->get_name());

    BooleanDagUtility::clear();

    if(solution != nullptr)
    {
        solution->clear();
        delete solution;
    }
    delete this;
}

void SketchFunction::replace(const string& replace_this, const string &with_this) {
    assert(new_way);
    AssertDebug(replaced_labels.find(replace_this) == replaced_labels.end(), "If this happens, it means that you are replacing a label that has previously been replaced (used as 'replace_this'). Not yet handled.");
    AssertDebug(replaced_labels.find(with_this) == replaced_labels.end(), "If this happens, it means that you are re-instating a label that has previously been replaced. Not sure why you are doing this. Not yet handled.");

    replaced_labels[replace_this] = with_this;

    get_env()->function_map.replace_label_with_another(get_dag()->get_name(), replace_this, with_this);
    get_dag()->replace_label_with_another(replace_this, with_this);
}
