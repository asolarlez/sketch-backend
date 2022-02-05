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

        if(var_type == bool_node::CTRL && var_store.size() >= 1) {
            SATSolverResult sat_solver_result = SAT_UNDETERMINED;
            if(get_dag()->get_failed_assert() != nullptr) {
                sat_solver_result = SAT_UNSATISFIABLE;
            }
            else if(!get_dag()->getNodesByType(bool_node::CTRL).empty())
            {
                sat_solver_result = SAT_NOT_FULLY_CONCRETIZED;
            }
            else if(get_dag()->get_failed_assert() == nullptr) {
                sat_solver_result = SAT_SATISFIABLE;
            }
            else
            {
                assert(false);
            }

            auto compare_solution = new SolverLanguagePrimitives::HoleAssignment(sat_solver_result, &var_store, get_env()->floats);

            if(solution != nullptr) {
                assert(*solution == *compare_solution);
                compare_solution->clear();
                delete compare_solution;
            }
            else {
                solution = compare_solution;
            }

        }

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

SketchFunction *SketchFunction::clone(const string& explicit_name) {

    BooleanDAG* cloned_dag = get_dag()->clone(explicit_name);

    get_env()->function_map.clone(get_dag()->get_name(), cloned_dag->get_name());

    SolverLanguagePrimitives::HoleAssignment* solution_clone = nullptr;

    if(solution != nullptr) {
        solution_clone = new SolverLanguagePrimitives::HoleAssignment(solution);
    }

    return new SketchFunction(
            cloned_dag, get_env(), solution_clone, replaced_labels, original_labels);
}

void SketchFunction::clear(){
    string dag_name = get_dag()->get_name();

    if(BooleanDagUtility::soft_clear()) {
        get_env()->function_map.erase(dag_name);
        if (solution != nullptr) {
            solution->clear();
            delete solution;
        }
        delete this;
    }
}

void SketchFunction::replace(const string& replace_this, const string &with_this) {
    assert(new_way);

    if(replaced_labels.find(replace_this) == replaced_labels.end()) {
        AssertDebug(replaced_labels.find(replace_this) == replaced_labels.end(),
                    "If this happens, it means that you are replacing a label that has previously been replaced (used as 'replace_this'). Not yet handled.");

        get_env()->function_map.replace_label_with_another(get_dag()->get_name(), replace_this, with_this);
        get_dag()->replace_label_with_another(replace_this, with_this);

        auto original_it = original_labels.find(replace_this);
        assert(original_it == original_labels.end());
        original_labels[replace_this] = replace_this;
    }
    else
    {
        assert(replaced_labels.find(replace_this) != replaced_labels.end());
        get_env()->function_map.replace_label_with_another(get_dag()->get_name(), replace_this, with_this);
        get_dag()->replace_label_with_another(replaced_labels[replace_this], with_this);

        auto original_it = original_labels.find(replace_this);
        assert(original_it != original_labels.end());
        assert(original_it->second == replace_this);
    }
    replaced_labels[replace_this] = with_this;
}

SketchFunction * SketchFunction::produce_get(const string& get_the_dag_under_this_varname) {
//    cout << "in produce get " << get_dag()->get_name() <<" "<< get_the_dag_under_this_varname << endl;
    return get_env()->function_map.produce_get(get_dag()->get_name(), get_the_dag_under_this_varname);
}

bool SketchFunction::solution_is_null() {
    return solution == nullptr;
}

SolverLanguagePrimitives::HoleAssignment *SketchFunction::get_same_soluton() {
    return solution;
}

string SketchFunction::get_assignment(const string& key) {
    auto it = replaced_labels.find(key);
    assert(it != replaced_labels.end());
    return it->second;
}

void SketchFunction::reset(const string& key) {
    auto it = replaced_labels.find(key);
    assert(it != replaced_labels.end());

    auto original_it = original_labels.find(key);
    assert(original_it != original_labels.end());
    assert(original_it->second == key);

    get_env()->function_map.replace_label_with_another(get_dag()->get_name(), key, key);

    replaced_labels.erase(it);
    original_labels.erase(original_it);
}

void SketchFunction::clear_assert_num_shared_ptr_is_0() {
    string dag_name = get_dag()->get_name();

    if(BooleanDagUtility::soft_clear_assert_num_shared_ptr_is_0()) {
        get_env()->function_map.erase(dag_name);
        if (solution != nullptr) {
            solution->clear();
            delete solution;
        }
        delete this;
    }
}

void BooleanDagUtility::swap_env(ProgramEnvironment *new_env) {
    assert(original_program_env == nullptr);
    original_program_env = env;
    assert(new_env != env);
    env = new_env;
}

void BooleanDagUtility::reset_env_to_original() {
    assert(original_program_env != nullptr);
    env = original_program_env;
    original_program_env = nullptr;
}

