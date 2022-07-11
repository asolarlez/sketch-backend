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
    class PolyVec;
    class PolyType;
}

class SketchFunction: public BooleanDagUtility
{
    static long long global_clear_id;

    map<string, string> replaced_labels;
    map<string, string> original_labels;

    class Dependencies: private map<string, SketchFunction*> {


    public:
        Dependencies() = default;

        const_iterator find(const string &key) const {
            return map<string, SketchFunction *>::find(key);
        }

        const_iterator end() const {
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

        void clear() {
            vector<string> keys;
            for(const auto& key : *this) {
                keys.push_back(key.first);
            }
            for(const auto& key: keys) {
                erase(key);
            }
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

        to_add->increment_shared_ptr();
        dependencies.insert(name, to_add);
    }
    explicit SketchFunction(
            BooleanDAG *_dag_root,
            ProgramEnvironment *_env = nullptr/*,
            const map<string, string>& _replaced_labels = map<string, string>(),
            const map<string, string>& _original_labels = map<string, string>(),
            const FMTL::TransformPrimitive* _rep = nullptr,
            Dependencies _responsibility = Dependencies(),
            const LightInliningTree* _inlining_tree = nullptr,
            bool _has_been_concretized = false*/) :
            BooleanDagUtility(_dag_root, _env, nullptr, false),
            replaced_labels(map<string, string>()), original_labels(map<string, string>()),
            rep(nullptr), dependencies(Dependencies()) {

    }

    explicit SketchFunction(
            BooleanDAG *_dag_root,
            ProgramEnvironment *_env,
            const map<string, string>& _replaced_labels,
            const map<string, string>& _original_labels,
            const FMTL::TransformPrimitive* _rep,
            Dependencies _responsibility,
            const LightInliningTree* _inlining_tree,
            bool _has_been_concretized) :
            BooleanDagUtility(_dag_root, _env, _inlining_tree, _has_been_concretized),
            replaced_labels(_replaced_labels), original_labels(_original_labels),
            rep(_rep), dependencies(std::move(_responsibility)) {

        for(const auto& dependency: dependencies) {
            assert(dependency.second != this);
            assert(dependency.second->get_dag_name() != get_dag_name());
            dependency.second->increment_shared_ptr();
        }

        //add self-loop dependency if it exists.
        for(auto it: get_dag()->getNodesByType(bool_node::UFUN)) {
            string ufun_name = ((UFUN_node*)it)->get_ufun_name();
            if(dependencies.find(ufun_name) == dependencies.end()) {
                assert(ufun_name == get_dag_name());
                dependencies.insert(ufun_name, this);
            }
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
    void _inplace_recursive_concretize(VarStore *var_store, const bool_node::Type var_type, bool do_recursive_concretize);
    SketchFunction *_inplace_concretize__assert_subfuncts_are_concretized(const VarStore *var_store, const bool_node::Type var_type);

    SketchFunction* unit_clone_and_insert_in_function_map();
    SketchFunction *unit_clone(const string& explicit_name = "", const map<string, string>* hole_rename_map = nullptr);
    SketchFunction *deep_clone(bool only_tail = false);
    SketchFunction* deep_exact_clone_and_fresh_function_map(ProgramEnvironment* new_environment = nullptr, map<SketchFunction*, SketchFunction*>* dp = new map<SketchFunction*, SketchFunction*>());
    SketchFunction *unit_exact_clone_in_fresh_env(Dependencies &new_dependencies, ProgramEnvironment *fresh_env);
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

    vector<string> get_unit_holes();

    const map<string, string> &get_unit_ufuns_map();

//    bool has_unit_self_loop() const;

    int count_passing_inputs(File *file, bool do_assert = true) override;
    SL::PolyVec* evaluate_inputs(File *file, unsigned int repeat = 30);

};

#include "NodeEvaluator.h"

namespace SketchFunctionEvaluator
{
    SL::VarVal* eval(SketchFunction *skfunc, const string& _line);

    SL::VarVal* eval(SketchFunction* skfunc, const VarStore *input_var_store);

    SL::VarVal* new_passes(BooleanDagLightUtility *skfunc, const string& _line);

    SL::VarVal* new_passes(BooleanDagLightUtility* skfunc, const VarStore *input_var_store, bool do_assert = true);

};

#endif //SKETCH_SOURCE_SKETCHFUNCTION_H
