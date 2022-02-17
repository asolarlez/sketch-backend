//
// Created by kliment on 2/16/22.
//

#ifndef SKETCH_SOURCE_BOOLEANDAGLIGHTUTILITY_H
#define SKETCH_SOURCE_BOOLEANDAGLIGHTUTILITY_H

#include "ProgramEnvironment.h"
#include "SkVal.h"

class File;

static bool new_way = true;

class BooleanDagUtility;

class SkFuncSetter
{
public:
    static long long inlining_tree_global_id;
    static set<const SkFuncSetter*> all_inlining_trees;
private:

    const long long inlining_tree_id;
protected:
    BooleanDagUtility * const skfunc = nullptr;
    SkFuncSetter(BooleanDagUtility* _skfunc);
    void clear(bool clear_dag = true, bool sub_clear = false) const;
public:
    const BooleanDagUtility* get_skfunc() const
    {
        return skfunc;
    }

    int get_id() const;
};

class InliningTree: private SkFuncSetter
{
    mutable bool deleted = false;
    map<string, const InliningTree*> var_name_to_inlining_subtree;

    mutable map<string, const vector<string>* > dag_name_to_path;

public:
    InliningTree(BooleanDagUtility* _skfunc, bool do_recurse): SkFuncSetter(_skfunc){ assert(!do_recurse); };
    //copy by replacing root skfunc
    InliningTree(BooleanDagUtility* to_replace_root, const InliningTree *to_copy, map<BooleanDagUtility *, const InliningTree *> *visited = new map<BooleanDagUtility *, const InliningTree *>());
    //pure construct from skfunct
    InliningTree(BooleanDagUtility* sk_func, map<BooleanDagUtility *, const InliningTree *> *visited = new map<BooleanDagUtility *, const InliningTree *>());
    //pure copy
    InliningTree(const InliningTree *to_copy, map<BooleanDagUtility *, const InliningTree *> *visited = new map<BooleanDagUtility *, const InliningTree *>()):
            SkFuncSetter(to_copy->skfunc)
    {
        assert(visited->find(skfunc) == visited->end());
        (*visited)[skfunc] = this;
        for(const auto& it: to_copy->var_name_to_inlining_subtree)
        {
            if(visited->find(it.second->skfunc) == visited->end()) {
                var_name_to_inlining_subtree[it.first] = new InliningTree(it.second, visited);
            }
            else
            {
                var_name_to_inlining_subtree[it.first] = (*visited)[it.second->skfunc];
            }
        }
        assert_nonnull();
    }

    bool assert_nonnull(set<const InliningTree*>* visited = new set<const InliningTree*>()) const {
        assert(visited->find(this) == visited->end());
        visited->insert(this);
        assert(skfunc != nullptr);
        for(auto it: var_name_to_inlining_subtree)
        {
            assert(it.second != nullptr);
            if(visited->find(it.second) == visited->end())
            {
                it.second->assert_nonnull(visited);
            }
        }
        return true;
    }

    void clear(bool clear_root = true, bool sub_clear = false) const;

    const InliningTree *get_sub_inlining_tree(const string &under_this_name) const {
        assert(var_name_to_inlining_subtree.find(under_this_name) != var_name_to_inlining_subtree.end());
        return var_name_to_inlining_subtree.at(under_this_name);
    }

    SolverLanguagePrimitives::HoleAssignment *get_solution(set<const InliningTree *> *visited = new set<const InliningTree*>()) const;

    const vector<string>* find(const string& target_dag, set<BooleanDagUtility*>* visited = new set<BooleanDagUtility*>()) const;

    bool match_topology(const InliningTree *other, set<string> *visited = new set<string>(), set<string> *other_visited = new set<string>()) const;

    void concretize(const VarStore *var_store, bool is_root = false, set<BooleanDagUtility*>* visited = new set<BooleanDagUtility*>()) const;

    const BooleanDagUtility *get_skfunc() const ;

    void print(int ntabs = 0, set<const InliningTree*>* visited = new set<const InliningTree*>()) const;

    void rename_var_store(VarStore &var_store, set<const InliningTree*> *visited = new set<const InliningTree*>(), const InliningTree *root = nullptr) const;

//    set<string> *get_inlined_function(set<string> * = new set<string>(), set<const InliningTree*>* visited = new set<const InliningTree*>()) const ;

    bool has_no_holes(set<string>* hole_names = new set<string>(), set<const InliningTree*>* visited = new set<const InliningTree*>()) const;
};


class BooleanDagLightUtility
{
    BooleanDAG* const root_dag = nullptr;
    ProgramEnvironment* env = nullptr;
    int shared_ptr = 0;
    const string& dag_name;

public:

    BooleanDagLightUtility(BooleanDAG* _root_dag):
            root_dag(_root_dag), dag_name(_root_dag->get_name()) {
        assert(root_dag != nullptr);
        AssertDebug(env != nullptr, "env needs to be defined.");
    }

    BooleanDagLightUtility(BooleanDAG* _root_dag, ProgramEnvironment* _env): root_dag(_root_dag), env(_env), dag_name(_root_dag->get_name()) {
        assert(root_dag != nullptr);
    }

    explicit BooleanDagLightUtility(BooleanDagLightUtility* to_copy): root_dag(to_copy->root_dag->clone()), env(to_copy->env), dag_name(to_copy->dag_name) {
        assert(root_dag != nullptr);
    }

    set<string>* get_inlined_functions(set<string>* ret = new set<string>());

    BooleanDAG* get_dag() const {
        return root_dag;
    }

    const string & get_dag_name() const {
        return dag_name;
    }

    ProgramEnvironment* get_env() const {
        return env;
    }

    ProgramEnvironment*& get_env_ref() {
        return env;
    }

    BooleanDagLightUtility* clone(bool use_same_name = false) {
        BooleanDAG* new_dag = nullptr;
        if(use_same_name) {
            new_dag = get_dag()->clone(get_dag_name());
        }
        else {
            new_dag = get_dag()->clone();
        }
        return new BooleanDagLightUtility(new_dag, get_env());
    }

    BooleanDagLightUtility* produce_inlined_dag(bool use_same_name = false) {
        BooleanDagLightUtility* ret = clone(use_same_name);
        vector<string>* inlined_functions = nullptr;
        ret->concretize_this_dag(nullptr, bool_node::CTRL, inlined_functions);
        delete inlined_functions;
        return ret;
    }


    BooleanDagLightUtility* produce_concretization(const VarStore* var_store, const bool_node::Type var_type) {
        BooleanDagLightUtility* ret = clone();
        vector<string>* inlined_functions = nullptr;
        ret->concretize_this_dag(var_store, var_type, inlined_functions);
        delete inlined_functions;
        return ret;
    }

    void concretize_this_dag(const VarStore* var_store, bool_node::Type var_type) {
        vector<string>* inlined_functions = nullptr;
        concretize_this_dag(var_store, var_type, inlined_functions);
    }

    void concretize_this_dag(const VarStore* var_store, bool_node::Type var_type, vector<string>*& inlined_functions)
    {
        if(var_store == nullptr) {
            var_store = new VarStore();
        }

        if (new_way) {
            assert(var_store != nullptr);
            env->doInline(*root_dag, *var_store, var_type, inlined_functions);
        } else {
            assert(var_store != nullptr);
            hardCodeINodeNoClone(root_dag, *var_store, var_type, env->get_floats());
            inlined_functions = nullptr;
        }
    }

    virtual bool soft_clear_assert_num_shared_ptr_is_0();

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


    vector<SkHoleSpec>* get_holes()
    {
        BooleanDagLightUtility* inlined_harness = produce_inlined_dag();
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


    int count_passing_inputs(File* file);

    virtual void clear(InliningTree*& inlining_tree, const SolverLanguagePrimitives::HoleAssignment*& solution) {
        if(soft_clear(inlining_tree, solution)){
            assert(shared_ptr == 0);
            delete this;
        }
        else {
            assert(shared_ptr >= 1);
        }
    }


    virtual void clear() {
        InliningTree* tmp = nullptr;
        const SolverLanguagePrimitives::HoleAssignment* solution = nullptr;
        clear(tmp, solution);
    }

    virtual bool soft_clear(InliningTree*& inlining_tree, const SolverLanguagePrimitives::HoleAssignment*& solution)
    {
        decrement_shared_ptr_wo_clear();

        bool clear_inlining_tree = false;
        bool clear_solution = false;

        if(solution != nullptr) {
            assert(solution->get_assignment()->get_inlining_tree() != nullptr);
            assert(solution->get_assignment()->get_inlining_tree()->get_skfunc() == (const BooleanDagUtility*)this);
        }

        if(inlining_tree != nullptr) {
            assert(inlining_tree->get_skfunc() == (const BooleanDagUtility*)this);
            if(shared_ptr == 1) {
                assert(solution == nullptr ||
                       solution->get_assignment()->get_inlining_tree() == nullptr ||
                       solution->get_assignment()->get_inlining_tree()->get_skfunc() != (const BooleanDagUtility*)this);

                clear_inlining_tree = true;

                InliningTree* tmp_inlining_tree = inlining_tree;
                inlining_tree = nullptr;
                tmp_inlining_tree->clear(false, true);
                assert(shared_ptr == 1);
                decrement_shared_ptr_wo_clear();
                assert(shared_ptr == 0);
            }
            else if(shared_ptr == 2)
            {
                if(solution != nullptr) {
                    assert(solution->get_assignment()->get_inlining_tree() != nullptr);
                    if(solution->get_assignment()->get_inlining_tree()->get_skfunc() == (const BooleanDagUtility*)this) {
                        if (solution->get_num_shared_ptr() == 0) {

                            clear_solution = true;

                            const SolverLanguagePrimitives::HoleAssignment *tmp_solution = solution;
                            solution = nullptr;
                            tmp_solution->clear_assert_num_shared_ptr_is_0(false, true);
                            assert(shared_ptr == 2);
                            decrement_shared_ptr_wo_clear();
                            assert(shared_ptr == 1);

                            clear_inlining_tree = true;

                            InliningTree* tmp_inlining_tree = inlining_tree;
                            inlining_tree = nullptr;
                            tmp_inlining_tree->clear(false, true);
                            assert(shared_ptr == 1);
                            decrement_shared_ptr_wo_clear();
                            assert(shared_ptr == 0);
                        }
                        else
                        {
                            //don't clear bc inlining skfunc doesnt match.
                            AssertDebug(false, "all non-0 shared_ptr of solution are cloned before going to the solver language");
                        }
                    }
                    else
                    {
                        AssertDebug(false, "invariant not maintained");
                        //don't clear either bc solution is still used.
                    }
                }
                else
                {
                    //don't clear bc skfunc still used
                }
            }
        }
        else
        {
            assert(solution == nullptr);
        }

        if(shared_ptr == 0) {
            bool ret = soft_clear_assert_num_shared_ptr_is_0();
            assert(ret);
            return ret;
        }
        else {
            return false;
        }
    }

    void increment_shared_ptr() {
//        if(get_dag()->dag_id == 260)
//        {
//            cout << "here" << endl;
//        }
        assert(shared_ptr >= 0);
        shared_ptr++;
    }

    void decrement_shared_ptr_wo_clear() {
//        if(get_dag()->dag_id == 260)
//        {
//            cout << "here" << endl;
//        }
        assert(shared_ptr >= 1);
        shared_ptr--;
        assert(shared_ptr >= 0);
    }

    int get_num_shared_ptr() const
    {
        assert(shared_ptr >= 0);
        return shared_ptr;
    }
};


#endif //SKETCH_SOURCE_BOOLEANDAGLIGHTUTILITY_H
