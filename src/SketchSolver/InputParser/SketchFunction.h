//
// Created by kliment on 12/20/21.
//

#ifndef SKETCH_SOURCE_SKETCHFUNCTION_H
#define SKETCH_SOURCE_SKETCHFUNCTION_H


#include "BooleanDagUtility.h"
#include <utility>


class BooleanDAG;
class SolverProgramState;

namespace SL
{
    class VarVal;
    class FunctionCall;
}

#ifndef REMOVE_SkVal
#include "SkVal.h"
#endif

class SketchFunction: public BooleanDagUtility
{
    static long long global_clear_id;

#ifndef REMOVE_SkVal
    const SolverLanguagePrimitives::HoleAssignment* solution = nullptr;
#endif
    bool new_way = true;

    map<string, string> replaced_labels;
    map<string, string> original_labels;

    class Dependencies: private map<string, SketchFunction*> {

    public:
    public:
        Dependencies() = default;

        auto find(const string &key) const {
            return map<string, SketchFunction *>::find(key);
        }

        auto end() const {
            return map<string, SketchFunction *>::end();
        }

        void insert(const string &key, SketchFunction *val) {
            assert(find(key) == end());
            map<string, SketchFunction *>::operator[](key) = val;
            val->increment_shared_ptr();
        }

        auto at(const string& key) const {
            assert(has(key));
            return map<string, SketchFunction *>::at(key);
        }

        auto begin() const {
            return map<string, SketchFunction *>::begin();
        }

        bool has(const string& key) const {
            return find(key) != end();
        }

        void erase(const string& key) {
            assert(has(key));
            at(key)->clear();
            map<string, SketchFunction *>::erase(key);
        }

        bool empty() const {
            return map<string, SketchFunction *>::empty();
        }
    };

    Dependencies dependencies;

    long long local_clear_id = -1;

    const FMTL::TransformPrimitive* rep = nullptr;
    const FMTL::TransformPrimitive* mirror_rep = nullptr;

    void core_clear(const string& dag_name);

    set<string> get_deep_holes();

public:

    const Dependencies& get_dependencies() const {
        return dependencies;
    }

    void add_dependency(SketchFunction* to_add)
    {
        string name = to_add->get_dag()->get_name();
        assert(dependencies.find(name) == dependencies.end());

        dependencies.insert(name, to_add);
    }

    explicit SketchFunction(
            BooleanDAG *_dag_root,
            ProgramEnvironment *_env = nullptr,
#ifndef REMOVE_SkVal
            const SolverLanguagePrimitives::HoleAssignment *_solution = nullptr,
#endif
            const map<string, string>& _replaced_labels = map<string, string>(),
            const map<string, string>& _original_labels = map<string, string>(),
            const FMTL::TransformPrimitive* _rep = nullptr,
            Dependencies _responsibility = Dependencies(),
            LightInliningTree* _inlining_tree = nullptr,
            bool _has_been_concretized = false) :
            BooleanDagUtility(_dag_root, _env, _inlining_tree, _has_been_concretized),
#ifndef REMOVE_SkVal
             solution(_solution),
#endif
            replaced_labels(_replaced_labels), original_labels(_original_labels),
            rep(_rep), dependencies(std::move(_responsibility)) {

        for(const auto& dependency: dependencies) {
            assert(dependency.second != this);
            assert(dependency.second->get_dag_name() != get_dag_name());
            dependency.second->increment_shared_ptr();
        }
#ifndef REMOVE_SkVal
        if(solution != nullptr) {
            VarStore* var_store = get_inlining_tree()->get_solution();
            var_store->set_inlining_tree(get_inlining_tree());
            solution = new SolverLanguagePrimitives::HoleAssignment(solution->get_sat_solver_result(), var_store, get_env()->floats);
            assert(solution != nullptr);
            increment_shared_ptr();
            var_store->clear();
            decrement_shared_ptr_wo_clear();
            assert(solution != nullptr);

            assert(solution->get_assignment()->get_inlining_tree()->get_dag_id() == get_dag_id());
        }
#endif
    }

    SketchFunction *produce_executable()
    {
        // inline, and then assert that everything has been concretized and inlined.
        bool_node::Type var_type = bool_node::CTRL;
        SketchFunction* ret = produce_concretization(nullptr, var_type, true);

        AssertDebug(ret->get_dag()->getNodesByType(bool_node::UFUN).empty(), "failed to produce executable, bc there are uninlined ufuns.");
        AssertDebug(ret->get_dag()->getNodesByType(bool_node::CTRL).empty(), "failed to produce executable, bc there are unconcretized holes.");

        return ret;
    }

    SketchFunction *make_executable()
    {
        bool_node::Type var_type = bool_node::CTRL;
        return produce_concretization(nullptr, var_type, false);
    }

    SketchFunction *produce_concretization(const VarStore *var_store, const bool_node::Type var_type, const bool do_clone, const bool do_deep_clone = true, const bool do_recursive_concretize = true);

    SketchFunction* unit_clone_and_insert_in_function_map();
    SketchFunction *unit_clone(const string& explicit_name = "");
    SketchFunction *deep_clone(bool only_tail = false);
    void deep_clone_tail();

    void clear() override;
    bool _clear();

    VarStore* get_solution_var_store()
    {
#ifdef REMOVE_SkVal
        //TODO
#else
        const SolverLanguagePrimitives::HoleAssignment* local_solution = get_solution();
        VarStore* ret = solution->get_assignment()->to_var_store();
        assert(local_solution->get_num_shared_ptr() == 0);
        local_solution->clear_assert_num_shared_ptr_is_0();
        return ret;
#endif
    }

#ifdef REMOVE_SkVal
    VarStore* get_solution();
#else
    const SolverLanguagePrimitives::HoleAssignment* get_solution()
    {
        if(solution != nullptr) {
            assert(get_inlining_tree() != nullptr);
            auto ret = new SolverLanguagePrimitives::HoleAssignment(solution);
            assert(ret->get_assignment()->get_inlining_tree() != nullptr);
            assert(ret->get_assignment()->to_var_store()->check_rep_and_clear());
            return ret;
        }
        else
        {
            assert(!is_inlining_tree_nonnull());
            LightInliningTree* local_inlining_tree = new LightInliningTree(this);
            const VarStore* almost_ret = local_inlining_tree->get_solution();

            const SolverLanguagePrimitives::HoleAssignment* ret =
                    new SolverLanguagePrimitives::HoleAssignment(SAT_UNDETERMINED, almost_ret, get_env()->get_floats());

            ret->set_inlining_tree(local_inlining_tree);

            assert(ret->get_assignment()->get_inlining_tree() != nullptr);

            assert(ret->get_assignment()->to_var_store()->check_rep_and_clear());

            local_inlining_tree->clear();
            local_inlining_tree = nullptr;

            assert(ret != nullptr);

            return ret;
        }
    }
#endif

#ifdef REMOVE_SkVal
    void concretize(const VarStore *local_solution) {
        produce_concretization(local_solution, bool_node::CTRL, false);
    }
#else
    void concretize(const SolverLanguagePrimitives::HoleAssignment *solution_holder)
    {
        VarStore* local_solution = solution_holder->to_var_store();
        produce_concretization(local_solution, bool_node::CTRL, false);
        local_solution->clear();
        local_solution = nullptr;
    }
#endif

    void replace(const string replace_this, const string with_this);

    SketchFunction * produce_get(const string& subfunc_name);

    bool solution_is_null();

#ifndef REMOVE_SkVal
    const SolverLanguagePrimitives::HoleAssignment * get_same_solution() const;
#endif

    string get_assignment(const string& key);

    void reset(const string& basicString);

    void clear_assert_num_shared_ptr_is_0();

    const map<string, string> &get_replace_map() const;

    void set_rep(const FMTL::TransformPrimitive *pPrimitive);

    const FMTL::TransformPrimitive * get_rep();

    void set_mirror_rep(const FMTL::TransformPrimitive *) ;
    const FMTL::TransformPrimitive * get_mirror_rep() const;

    set<string> ufun_names();

    void set_dependencies(const FunctionMap* fmap);
};

#include "NodeEvaluator.h"

namespace SketchFunctionEvaluator
{
#ifdef REMOVE_SkVal

    SL::VarVal* eval(SketchFunction* skfunc, const VarStore *input_var_store);

    SL::VarVal* passes(const SketchFunction *skfunc, const VarStore *input_var_store);

    SL::VarVal* new_passes(SketchFunction* skfunc, const VarStore *input_var_store);
#else

    SL::VarVal* eval(SketchFunction* skfunc, SolverLanguagePrimitives::InputAssignment *input_assignment);

    SL::VarVal* passes(const SketchFunction *skfunc, const SolverLanguagePrimitives::InputAssignment *input_assignment);

    SL::VarVal* new_passes(SketchFunction* skfunc, SolverLanguagePrimitives::InputAssignment *input_assignment);

#endif
};

#endif //SKETCH_SOURCE_SKETCHFUNCTION_H
