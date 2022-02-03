//
// Created by kliment on 12/20/21.
//

#ifndef SKETCH_SOURCE_SKETCHFUNCTION_H
#define SKETCH_SOURCE_SKETCHFUNCTION_H

//#include "BooleanDAG.h"
//#include "ProgramEnvironment.h"

#include "BooleanNodes.h"
#include "VarStore.h"
#include "SkVal.h"
#include "File.h"
#include "ProgramEnvironment.h"
#include "FunctionMapTransformerLanguage.h"

class BooleanDAG;
class SolverProgramState;

namespace SL
{
    class VarVal;
    class FunctionCall;
}

static bool new_way = true;

class BooleanDagUtility {
    BooleanDAG* root_dag = nullptr;
    ProgramEnvironment* env;
    int shared_ptr = 0;

public:
    BooleanDagUtility(BooleanDAG* _root_dag, ProgramEnvironment* _env):
        root_dag(_root_dag), env(_env) {
        assert(root_dag != nullptr);
    }

    BooleanDagUtility(BooleanDagUtility* shallow_copy): root_dag(shallow_copy->root_dag), env(shallow_copy->env) {
        assert(root_dag != nullptr);
    }

    BooleanDAG* get_dag() {
        return root_dag;
    }

    ProgramEnvironment* get_env() {
        return env;
    }

    BooleanDAG* produce_inlined_dag(VarStore& var_store, bool_node::Type var_type)
    {
        if (new_way) {
            BooleanDAG* ret = root_dag->clone();
            env->doInline(*ret, var_store, var_type);
            return ret;
        } else {
            return hardCodeINode(root_dag, var_store, var_type, env->get_floats());
        }
    }


    void inline_this_dag(VarStore& var_store, bool_node::Type var_type)
    {
        vector<string>* tmp = nullptr;
        inline_this_dag(var_store, var_type, tmp);
        delete tmp;
    }

    void inline_this_dag(VarStore& var_store, bool_node::Type var_type, vector<string> *&inlined_functions)
    {
        if (new_way) {
            env->doInline(*root_dag, var_store, var_type, inlined_functions);
        } else {
            hardCodeINodeNoClone(root_dag, var_store, var_type, env->get_floats());
            inlined_functions = nullptr;
        }
    }

    int count_passing_inputs(File* file) {
        int ret = 0;
        int num_0s = 0;
        int num_1s = 0;
        for(int i = 0;i<file->size();i++)
        {
            BooleanDAG* dag = produce_inlined_dag(*file->at(i), bool_node::SRC);
            assert(dag->getNodesByType(bool_node::CTRL).size() == 0);
            assert((dag->size() == 0) == (dag->get_failed_assert() == nullptr));
            if(dag->get_failed_assert() == nullptr) {
                ret += 1;
            }
            dag->clear();
        }
        return ret;
    }

    bool clear()
    {
        shared_ptr--;
        assert(shared_ptr>=0);
        if(shared_ptr == 0) {
            int prev_num = BooleanDAG::get_allocated().size();
            assert(root_dag != nullptr);
            root_dag->clear();
            assert(prev_num - 1 == BooleanDAG::get_allocated().size());
            root_dag = nullptr;
            return true;
        }
        else {
            return false;
        }
    }

    void increment_shared_ptr() {
        assert(shared_ptr >= 0);
        shared_ptr++;
    }
};

class SketchFunction: public BooleanDagUtility
{
    SolverLanguagePrimitives::HoleAssignment* solution = nullptr;

    bool new_way = true;

    map<string, string> replaced_labels;

    void add_solution(SolverLanguagePrimitives::HoleAssignment* _solution_holder)
    {
        assert(solution == nullptr);
        solution = new SolverLanguagePrimitives::HoleAssignment(_solution_holder);
    }

public:

    explicit SketchFunction(
            BooleanDAG *_dag_root,
            ProgramEnvironment *_env = nullptr,
            SolverLanguagePrimitives::HoleAssignment *_solution = nullptr,
            const map<string, string>& _replaced_labels = map<string, string>()) :
            BooleanDagUtility(_dag_root, _env), solution(_solution), replaced_labels(_replaced_labels)
    {
        for(auto it: get_dag()->getNodesByType(bool_node::UFUN)){
            replaced_labels[it->get_name()] = it->get_name();
        }
    }

    SketchFunction *produce_inlined_dag()
    {
        VarStore var_store;
        bool_node::Type var_type = bool_node::CTRL;
        return produce_concretization(var_store, var_type);
    }

    void concretize(VarStore &var_store, bool_node::Type var_type)
    {
        produce_concretization(var_store, var_type, false);
    }

    SketchFunction *produce_concretization(VarStore &var_store, bool_node::Type var_type, bool do_clone = true);

    SketchFunction *clone();

    void clear();

private:
    VarStore* solution_ctrl_var_store = nullptr;
public:
    VarStore* get_solution_ctrl_var_store()
    {
        return solution_ctrl_var_store;
    }
    void set_solution_ctrl_var_store(VarStore* _ctrl_var_store)
    {
        solution_ctrl_var_store = _ctrl_var_store;
    }

private:
    static SkValType bool_node_out_type_to_sk_val_type(OutType* out_type)
    {
        assert(out_type == OutType::INT || out_type == OutType::BOOL || OutType::FLOAT);
        if(out_type == OutType::INT)
        {
            return sk_type_int;
        }
        else if(out_type == OutType::BOOL)
        {
            return sk_type_bool;
        }
        else if(out_type == OutType::FLOAT)
        {
            return sk_type_float;
        }
        else
        {
            assert(false);
        }
    }
public:
    vector<SkHoleSpec>* get_holes()
    {
        SketchFunction* inlined_harness = produce_inlined_dag();
        auto ctrl_nodes = inlined_harness->get_dag()->getNodesByType(bool_node::CTRL);
        auto* ret = new vector<SkHoleSpec>();
        for(auto & ctrl_node : ctrl_nodes)
        {
            ret->push_back(
                    SkHoleSpec(
                            ctrl_node->get_name(),
                            bool_node_out_type_to_sk_val_type(ctrl_node->getOtype())));
        }
        inlined_harness->clear();
        return ret;
    }

    SketchFunction *produce_with_concretized_holes(SolverLanguagePrimitives::HoleAssignment *solution_holder)
    {
        VarStore* var_store = solution_holder->to_var_store();
        SketchFunction* ret = produce_concretization(*var_store, bool_node::CTRL);
        var_store->clear();
        ret->add_solution(solution_holder);
        return ret;
    }

    SolverLanguagePrimitives::HoleAssignment* get_solution()
    {
        assert(solution != nullptr);
        return new SolverLanguagePrimitives::HoleAssignment(solution);
    }

    SolverLanguagePrimitives::HoleAssignment* get_solution(const string& sub_solution_label)
    {
        get_env()->function_map.print_extras();

        cout << "START find_subdag_name" << endl;

//        string underlying_function_name = get_env()->function_map.find_subdag_name(get_dag()->get_name(), sub_solution_label);

        cout << "DONE find_subdag_name" << endl;
        cout << "START find_last_var_store_on_the_way_to" << endl;

        const VarStore* var_store_used_to_concretize_underlying_subdag =
                get_env()->function_map.get_var_store_used_to_concretize_underlying_subdag(get_dag()->get_name(),
                                                                                           sub_solution_label);
//                get_env()->function_map.find_last_var_store_on_the_way_to(get_dag()->get_name(), underlying_function_name);

        cout << "DONE find_last_var_store_on_the_way_to" << endl;

        assert(var_store_used_to_concretize_underlying_subdag != nullptr);

        SATSolverResult dummy_sat_solver_result = SAT_SATISFIABLE;

        if(solution != nullptr)
        {
            dummy_sat_solver_result = solution->get_sat_solver_result();
        }

        auto new_solution = (new SolverLanguagePrimitives::HoleAssignment(
                dummy_sat_solver_result, var_store_used_to_concretize_underlying_subdag,
                get_env()->floats));
        if(solution != nullptr)
        {
            cout << solution->to_string() << endl;
            cout << new_solution->to_string() << endl;
            assert(*solution == *new_solution);
            new_solution->clear();
            delete new_solution;
            new_solution = nullptr;
        }
        else {
            TransformPrimitive* transform_program_root = get_env()->function_map.get_root_dag_reps()[get_dag()->get_name()];
            AssertDebug(transform_program_root->get_primitive_type() == FMTL::_replace,
                        "could also be clone (bc if it is concretize, the solution gets stored automatically, checked by the previous if branch)."
                        "TODO: think how it works if it is clone.");

            assert(solution == nullptr);
            solution = new_solution;
        }
        assert(solution != nullptr);
        cout << "RETURN SOLUTION" << endl;
        return new SolverLanguagePrimitives::HoleAssignment(solution);
    }

    void concretize(SolverLanguagePrimitives::HoleAssignment *solution_holder)
    {
        VarStore* local_solution = solution_holder->to_var_store();
        concretize(*local_solution, bool_node::CTRL);
        local_solution->clear();
        local_solution = nullptr;
        add_solution(solution_holder);
    }

    SketchFunction* produce_with_concretized_inputs(SolverLanguagePrimitives::InputAssignment* input_holder)
    {
        VarStore* inputs = input_holder->to_var_store();
        SketchFunction* ret = produce_concretization(*inputs, bool_node::SRC);
        inputs->clear();
        inputs = nullptr;
        return ret;
    }

    void replace(const string& replace_this, const string &with_this);

    SketchFunction * produce_get(const string& subfunc_name);
};

#endif //SKETCH_SOURCE_SKETCHFUNCTION_H
