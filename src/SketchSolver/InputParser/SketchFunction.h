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

class SketchFunction: public BooleanDagUtility
{
    static long long global_clear_id;

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
            const map<string, string>& _replaced_labels = map<string, string>(),
            const map<string, string>& _original_labels = map<string, string>(),
            const FMTL::TransformPrimitive* _rep = nullptr,
            Dependencies _responsibility = Dependencies(),
            const LightInliningTree* _inlining_tree = nullptr,
            bool _has_been_concretized = false) :
            BooleanDagUtility(_dag_root, _env, _inlining_tree, _has_been_concretized),
            replaced_labels(_replaced_labels), original_labels(_original_labels),
            rep(_rep), dependencies(std::move(_responsibility)) {

        for(const auto& dependency: dependencies) {
            assert(dependency.second != this);
            assert(dependency.second->get_dag_name() != get_dag_name());
            dependency.second->increment_shared_ptr();
        }
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

    SketchFunction *produce_concretization(const VarStore *var_store, const bool_node::Type var_type, bool do_clone, const bool do_deep_clone = true, const bool do_recursive_concretize = true);
    SketchFunction *_inplace_recursive_concretize(VarStore *var_store, const bool_node::Type var_type, const bool do_recursive_concretize);
    SketchFunction *_inplace_concretize(const VarStore *var_store, const bool_node::Type var_type);

    SketchFunction* unit_clone_and_insert_in_function_map();
    SketchFunction *unit_clone(const string& explicit_name = "");
    SketchFunction *deep_clone(bool only_tail = false);
    void deep_clone_tail();

    void clear() override;
    bool _clear();

    VarStore* get_solution();

    void concretize(const VarStore *local_solution) {
        produce_concretization(local_solution, bool_node::CTRL, false);
    }

    void replace(const string replace_this, const string with_this);

    SketchFunction * produce_get(const string& subfunc_name);

    bool solution_is_null();

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

    const vector<string> &get_unit_holes();

    const map<string, string> &get_unit_ufuns_map();
};

#include "NodeEvaluator.h"

namespace SketchFunctionEvaluator
{
    SL::VarVal* eval(SketchFunction *skfunc, const string& _line);

    SL::VarVal* eval(SketchFunction* skfunc, const VarStore *input_var_store);

    SL::VarVal* passes(const SketchFunction *skfunc, const VarStore *input_var_store);

    SL::VarVal* new_passes(SketchFunction *skfunc, const string& _line);

    SL::VarVal* new_passes(SketchFunction* skfunc, const VarStore *input_var_store);

};

#endif //SKETCH_SOURCE_SKETCHFUNCTION_H
