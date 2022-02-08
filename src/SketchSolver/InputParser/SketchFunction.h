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
    BooleanDAG* const root_dag = nullptr;
    ProgramEnvironment* env;
    int shared_ptr = 0;

    ProgramEnvironment* original_program_env = nullptr;

protected:
    const string& dag_name;
public:
    BooleanDagUtility(BooleanDAG* _root_dag, ProgramEnvironment* _env):
        root_dag(_root_dag), env(_env), dag_name(_root_dag->get_name()) {
        assert(root_dag != nullptr);
    }

    BooleanDagUtility(BooleanDagUtility* to_copy): root_dag(to_copy->root_dag->clone()), env(to_copy->env), dag_name(to_copy->dag_name) {
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

    void inline_this_dag()
    {
        VarStore var_store;
        inline_this_dag(var_store, bool_node::CTRL);
    }

    void inline_this_dag(VarStore& var_store, bool_node::Type var_type)
    {
        vector<string>* tmp = nullptr;
        inline_this_dag(var_store, var_type, tmp);
        delete tmp;
    }

    void inline_this_dag(VarStore &var_store, bool_node::Type var_type, vector<string> *&inlined_functions)
    {
        assert(!get_dag()->get_failed_assert());
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

    virtual void clear() {
        if(soft_clear()){
            assert(shared_ptr == 0);
            delete this;
        }
        else {
            assert(shared_ptr >= 1);
        }
    }

    bool soft_clear()
    {
        shared_ptr--;
        assert(shared_ptr>=0);
        if(shared_ptr == 0) {
            bool ret = soft_clear_assert_num_shared_ptr_is_0();
            assert(ret);
            return ret;
        }
        else {
            return false;
        }
    }


    bool soft_clear_assert_num_shared_ptr_is_0();

    void increment_shared_ptr() {
        assert(shared_ptr >= 0);
        shared_ptr++;
    }


    void decrement_shared_ptr_wo_clear() {
        assert(shared_ptr >= 1);
        shared_ptr--;
    }

    int get_num_shared_ptr() const
    {
        assert(shared_ptr >= 0);
        return shared_ptr;
    }

    void swap_env(ProgramEnvironment *new_env);

    void reset_env_to_original();

    bool env_was_swapped() {
        return original_program_env != nullptr;
    }

    void hard_swap_env(ProgramEnvironment* new_env)
    {
        assert(original_program_env == nullptr);
        env = new_env;
    }
};

static long long global_clear_id = 0;

class SketchFunction: public BooleanDagUtility
{
    SolverLanguagePrimitives::HoleAssignment* solution = nullptr;

    bool new_way = true;

    map<string, string> replaced_labels;
    map<string, string> original_labels;

    map<string, SketchFunction*> responsibility;

    long long local_clear_id = -1;

    const TransformPrimitive* rep = nullptr;
    const TransformPrimitive* mirror_rep = nullptr;

public:

    const map<string, SketchFunction*>& get_responsibilities() const
    {
        return responsibility;
    }

    void add_responsibility(SketchFunction* to_add)
    {
        string name = to_add->get_dag()->get_name();
        assert(responsibility.find(name) == responsibility.end());
        to_add->increment_shared_ptr();
        responsibility[name] = to_add;
    }

    explicit SketchFunction(
            BooleanDAG *_dag_root,
            ProgramEnvironment *_env = nullptr,
            SolverLanguagePrimitives::HoleAssignment *_solution = nullptr,
            const map<string, string>& _replaced_labels = map<string, string>(),
            const map<string, string>& _original_labels = map<string, string>(),
            const TransformPrimitive* _rep = nullptr,
            map<string, SketchFunction*> _responsibility = map<string, SketchFunction*>()) :
            BooleanDagUtility(_dag_root, _env), solution(_solution),
            replaced_labels(_replaced_labels), original_labels(_original_labels),
            rep(_rep), responsibility(_responsibility) {
        for(auto dependency: responsibility) {
            dependency.second->increment_shared_ptr();
        }
    }

    SketchFunction *produce_inlined_dag()
    {
        VarStore var_store;
        bool_node::Type var_type = bool_node::CTRL;
        return produce_concretization(var_store, var_type, true);
    }

    void concretize(VarStore &var_store, bool_node::Type var_type)
    {
        produce_concretization(var_store, var_type, false);
    }

    SketchFunction *produce_concretization(VarStore &var_store, bool_node::Type var_type, bool do_clone);

    SketchFunction *clone(const string& explicit_name = "");

    void clear() override;
    void _clear();

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
        SketchFunction* ret = produce_concretization(*var_store, bool_node::CTRL, true);
        var_store->clear();
        return ret;
    }

    SolverLanguagePrimitives::HoleAssignment* set_and_get_solution_from_var_store(const VarStore* var_store)
    {
        SATSolverResult dummy_sat_solver_result = SAT_SATISFIABLE;

        if(solution != nullptr)
        {
            dummy_sat_solver_result = solution->get_sat_solver_result();
        }

        auto new_solution = (new SolverLanguagePrimitives::HoleAssignment(
                dummy_sat_solver_result, var_store,
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
            auto reps = get_env()->function_map.get_root_dag_reps();
            assert(reps.find(get_dag()->get_name()) != reps.end());
            TransformPrimitive* transform_program_root = reps.at(get_dag()->get_name());
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


    SolverLanguagePrimitives::HoleAssignment* get_solution()
    {
        assert(solution != nullptr);
        return new SolverLanguagePrimitives::HoleAssignment(solution);
    }

    void concretize(SolverLanguagePrimitives::HoleAssignment *solution_holder)
    {
        VarStore* local_solution = solution_holder->to_var_store();
        concretize(*local_solution, bool_node::CTRL);
        local_solution->clear();
        local_solution = nullptr;
    }

    void replace(const string replace_this, const string with_this);

    SketchFunction * produce_get(const string& subfunc_name);

    bool solution_is_null();

    SolverLanguagePrimitives::HoleAssignment *get_same_soluton();

    string get_assignment(const string& key);

    void reset(const string& basicString);

    void clear_assert_num_shared_ptr_is_0();

    const map<string, string> &get_replace_map() const;

    void set_rep(const TransformPrimitive *pPrimitive);

    const TransformPrimitive * get_rep();

    void set_mirror_rep(const TransformPrimitive *) ;
    const TransformPrimitive * get_mirror_rep() const;
};

#endif //SKETCH_SOURCE_SKETCHFUNCTION_H
