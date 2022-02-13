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
            map<string, SketchFunction*> _responsibility = map<string, SketchFunction*>(),
            InliningTree* _inlining_tree = nullptr,
            bool _has_been_concretized = false) :
            BooleanDagUtility(_dag_root, _env, _inlining_tree, _has_been_concretized), solution(_solution),
            replaced_labels(_replaced_labels), original_labels(_original_labels),
            rep(_rep), responsibility(std::move(_responsibility)) {
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

    SketchFunction *produce_concretization(const VarStore &var_store, const bool_node::Type var_type, const bool do_clone, const bool do_deep_clone = true);

    SketchFunction *clone(const string& explicit_name = "");

    void clear() override;
    void _clear();

    SolverLanguagePrimitives::HoleAssignment* get_solution()
    {
        if(solution != nullptr) {
            auto ret = new SolverLanguagePrimitives::HoleAssignment(solution);
            assert(ret->get_assignment()->get_inlining_tree() != nullptr);
            assert(ret->get_assignment()->to_var_store()->check_rep());
            return ret;
        }
        else
        {
            assert(!is_inlining_tree_nonnull());
            InliningTree* inlining_tree = new InliningTree(this);
            auto ret = inlining_tree->get_solution();
            assert(ret->get_assignment()->get_inlining_tree() != nullptr);

            assert(ret->get_assignment()->to_var_store()->check_rep());

            inlining_tree->clear();
            inlining_tree = nullptr;

            assert(ret != nullptr);

            return ret;
        }
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

    void deep_clone_tail() {
        VarStore var_store;
        deep_clone_tail(var_store);
    }

    void deep_clone_tail(const VarStore&);
};

#endif //SKETCH_SOURCE_SKETCHFUNCTION_H
