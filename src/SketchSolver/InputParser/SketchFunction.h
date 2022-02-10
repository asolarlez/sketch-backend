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
#include "BooleanDagUtility.h"

class BooleanDAG;
class SolverProgramState;

namespace SL
{
    class VarVal;
    class FunctionCall;
}

static long long global_clear_id = 0;

class InliningTree
{
    const string& dag_name;
    map<string, InliningTree*> var_name_to_dag_name;


    void add_subdag_under_var_name(const string& var_name, SketchFunction* sub_dag);

    const string& get_dag_name() {
        return dag_name;
    }

    static const string& choose_dag_name(const string& new_dag_name, const string& prev_dag_name)
    {
        assert(!prev_dag_name.empty());
        if(new_dag_name.empty()) {
            return prev_dag_name;
        }
        else {
            return new_dag_name;
        }
    }

public:
    explicit InliningTree(SketchFunction* sk_func);
    InliningTree(InliningTree* to_copy, const string& new_dag_name = ""): dag_name(choose_dag_name(new_dag_name, to_copy->dag_name))
    {
        for(auto it:to_copy->var_name_to_dag_name) {
            var_name_to_dag_name[it.first] = new InliningTree(it.second);
        }
    }

    void clone(const string &basicString);
};

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

    InliningTree* inlining_tree = nullptr;

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
            map<string, SketchFunction*> _responsibility = map<string, SketchFunction*>(),
            InliningTree* _inlining_tree = nullptr) :
            BooleanDagUtility(_dag_root, _env), solution(_solution),
            replaced_labels(_replaced_labels), original_labels(_original_labels),
            rep(_rep), responsibility(_responsibility), inlining_tree(_inlining_tree) {
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

    SketchFunction *produce_concretization(const VarStore &var_store, const bool_node::Type var_type, const bool do_clone);

    SketchFunction *clone(const string& explicit_name = "");

    void clear() override;
    void _clear();

//    SolverLanguagePrimitives::HoleAssignment* set_and_get_solution_from_var_store(const VarStore* var_store)
//    {
//        SATSolverResult dummy_sat_solver_result = SAT_SATISFIABLE;
//
//        if(solution != nullptr)
//        {
//            dummy_sat_solver_result = solution->get_sat_solver_result();
//        }
//
//        auto new_solution = (new SolverLanguagePrimitives::HoleAssignment(
//                dummy_sat_solver_result, var_store,
//                get_env()->floats));
//        if(solution != nullptr)
//        {
//            cout << solution->to_string() << endl;
//            cout << new_solution->to_string() << endl;
//            assert(*solution == *new_solution);
//            new_solution->clear();
//            delete new_solution;
//            new_solution = nullptr;
//        }
//        else {
//            auto reps = get_env()->function_map.get_root_dag_reps();
//            assert(reps.find(get_dag()->get_name()) != reps.end());
//            TransformPrimitive* transform_program_root = reps.at(get_dag()->get_name());
//            AssertDebug(transform_program_root->get_primitive_type() == FMTL::_replace,
//                        "could also be clone (bc if it is concretize, the solution gets stored automatically, checked by the previous if branch)."
//                        "TODO: think how it works if it is clone.");
//
//            assert(solution == nullptr);
//            solution = new_solution;
//        }
//        assert(solution != nullptr);
//        cout << "RETURN SOLUTION" << endl;
//        return new SolverLanguagePrimitives::HoleAssignment(solution);
//    }


    SolverLanguagePrimitives::HoleAssignment* get_solution()
    {
        assert(solution != nullptr);
        return new SolverLanguagePrimitives::HoleAssignment(solution);
    }

    void concretize(SolverLanguagePrimitives::HoleAssignment *solution_holder)
    {
        VarStore* local_solution = solution_holder->to_var_store();
        produce_concretization(*local_solution, bool_node::CTRL, false);
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
