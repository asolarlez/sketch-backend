//
// Created by kliment on 12/20/21.
//

#ifndef SKETCH_SOURCE_SKETCHFUNCTION_H
#define SKETCH_SOURCE_SKETCHFUNCTION_H

//#include "BooleanDAG.h"
//#include "ProgramEnvironment.h"

#include <utility>

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

class SketchFunction: public BooleanDagUtility
{
    const SolverLanguagePrimitives::HoleAssignment* solution = nullptr;

    bool new_way = true;

    map<string, string> replaced_labels;
    map<string, string> original_labels;

    map<string, SketchFunction*> responsibility;

    long long local_clear_id = -1;

    const TransformPrimitive* rep = nullptr;
    const TransformPrimitive* mirror_rep = nullptr;

    void core_clear(const string& dag_name);

public:

    const map<string, SketchFunction*>& get_responsibilities() const
    {
        return responsibility;
    }

    void add_responsibility(SketchFunction* to_add)
    {
        string name = to_add->get_dag()->get_name();
        assert(responsibility.find(name) == responsibility.end());

        assert(to_add != this);
        assert(to_add->get_dag_name() != get_dag_name());

        responsibility[name] = to_add;
        to_add->increment_shared_ptr();
    }

    explicit SketchFunction(
            BooleanDAG *_dag_root,
            ProgramEnvironment *_env = nullptr,
            const SolverLanguagePrimitives::HoleAssignment *_solution = nullptr,
            const map<string, string>& _replaced_labels = map<string, string>(),
            const map<string, string>& _original_labels = map<string, string>(),
            const TransformPrimitive* _rep = nullptr,
            map<string, SketchFunction*> _responsibility = map<string, SketchFunction*>(),
            InliningTree* _inlining_tree = nullptr,
            bool _has_been_concretized = false) :
            BooleanDagUtility(_dag_root, _env, _inlining_tree, _has_been_concretized), solution(_solution),
            replaced_labels(_replaced_labels), original_labels(_original_labels),
            rep(_rep), responsibility(std::move(_responsibility)) {
        for(auto dependency: responsibility) {
            assert(dependency.second != this);
            assert(dependency.second->get_dag_name() != get_dag_name());
            dependency.second->increment_shared_ptr();
        }
        if(solution != nullptr) {
            VarStore* var_store = solution->to_var_store();

            get_inlining_tree()->rename_var_store(*var_store);
            solution = new SolverLanguagePrimitives::HoleAssignment(solution->get_sat_solver_result(), var_store, get_env()->floats);
            assert(solution != nullptr);
            increment_shared_ptr();
            var_store->clear();
            decrement_shared_ptr_wo_clear();
            assert(solution != nullptr);

            assert(solution->get_assignment()->get_inlining_tree()->get_skfunc() == this);
        }
    }

    SketchFunction *produce_inlined_dag()
    {
        bool_node::Type var_type = bool_node::CTRL;
        return produce_concretization(nullptr, var_type, true);
    }

    SketchFunction *inline_this_dag()
    {
        bool_node::Type var_type = bool_node::CTRL;
        return produce_concretization(nullptr, var_type, false);
    }

    SketchFunction *produce_concretization(const VarStore *var_store, const bool_node::Type var_type, const bool do_clone, const bool do_deep_clone = true);

    SketchFunction *clone(const string& explicit_name = "");

    void clear() override;
    void _clear();

    const VarStore* get_solution_var_store()
    {
        const SolverLanguagePrimitives::HoleAssignment* local_solution = get_solution();
        const VarStore* ret = solution->get_assignment()->to_var_store();
        assert(local_solution->get_num_shared_ptr() == 0);
        local_solution->clear_assert_num_shared_ptr_is_0();
        return ret;
    }

    const SolverLanguagePrimitives::HoleAssignment* get_solution()
    {
        if(solution != nullptr) {
            assert(get_inlining_tree() != nullptr);
            auto ret = new SolverLanguagePrimitives::HoleAssignment(solution);
            assert(ret->get_assignment()->get_inlining_tree() != nullptr);
            assert(ret->get_assignment()->to_var_store()->check_rep_and_clear());
            assert(ret->get_assignment()->get_inlining_tree() != get_inlining_tree());
            return ret;
        }
        else
        {
            assert(!is_inlining_tree_nonnull());
            InliningTree* local_inlining_tree = new InliningTree(this);
            auto ret = local_inlining_tree->get_solution();
            assert(ret->get_assignment()->get_inlining_tree() != nullptr);
            assert(ret->get_assignment()->get_inlining_tree() != local_inlining_tree);

            assert(ret->get_assignment()->to_var_store()->check_rep_and_clear());

            local_inlining_tree->clear();
            local_inlining_tree = nullptr;

            assert(ret != nullptr);

            return ret;
        }
    }

    void concretize(const SolverLanguagePrimitives::HoleAssignment *solution_holder)
    {
        VarStore* local_solution = solution_holder->to_var_store();
        produce_concretization(local_solution, bool_node::CTRL, false);
        local_solution->clear();
        local_solution = nullptr;
    }

    void replace(const string replace_this, const string with_this);

    SketchFunction * produce_get(const string& subfunc_name);

    bool solution_is_null();

    const SolverLanguagePrimitives::HoleAssignment * get_same_soluton();

    string get_assignment(const string& key);

    void reset(const string& basicString);

    void clear_assert_num_shared_ptr_is_0();

    const map<string, string> &get_replace_map() const;

    void set_rep(const TransformPrimitive *pPrimitive);

    const TransformPrimitive * get_rep();

    void set_mirror_rep(const TransformPrimitive *) ;
    const TransformPrimitive * get_mirror_rep() const;

    void deep_clone_tail();
};

#include "NodeEvaluator.h"

namespace SketchFunctionEvaluator
{
    SL::VarVal* eval(SketchFunction* sk_func, SolverLanguagePrimitives::InputAssignment *input_assignment);

    SL::VarVal* passes(const SketchFunction *sk_func, const SolverLanguagePrimitives::InputAssignment *input_assignment);

    SL::VarVal* new_passes(SketchFunction* sk_func, SolverLanguagePrimitives::InputAssignment *input_assignment);
};

#endif //SKETCH_SOURCE_SKETCHFUNCTION_H
