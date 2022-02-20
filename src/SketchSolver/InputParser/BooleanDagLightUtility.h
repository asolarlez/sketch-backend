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



class LightSkFuncSetter
{
    mutable bool cleared = false;
    mutable int num_shared_ptr = 1;

    const string dag_name;
    const int dag_id;

    const VarStore* var_store = nullptr;

    int num_unconcretized_holes = -1;


protected:

    bool assert_nonnull() const {
        assert(!dag_name.empty());
        assert(0 <= dag_id && dag_id < BooleanDAG::get_global_boolean_dag_id());
        return true;
    }

    int get_num_shared_ptr() const {
        return num_shared_ptr;
    }

    void decrement_num_shared_ptr() const {
        assert(num_shared_ptr >= 1);
        num_shared_ptr--;
        assert(num_shared_ptr == 0);
    }

    void soft_clear(bool _clear_root, bool _sub_clear) const {
        assert(get_num_shared_ptr() == 0);
        assert(!cleared);
        cleared = true;
    }

    const VarStore* get_var_store_used_for_concretization() const {
        return var_store;
    }

    int get_num_unconcretized_holes() const
    {
        return num_unconcretized_holes;
    }

public:

    explicit LightSkFuncSetter(const LightSkFuncSetter* to_copy): dag_name(to_copy->dag_name), dag_id(to_copy->dag_id), var_store(to_copy->var_store), num_unconcretized_holes(to_copy->num_unconcretized_holes) {
    }
    explicit LightSkFuncSetter(const BooleanDagUtility* _skfunc);

    const int get_dag_id() const
    {
        return dag_id;
    }

    const string&  get_dag_name() const
    {
        return dag_name;
    }

    void increment_num_shared_ptr() {
        num_shared_ptr++;
    }
};

class SkFuncSetter: public LightSkFuncSetter
{
public:
    static long long inlining_tree_global_id;
    static set<const SkFuncSetter*> all_inlining_trees;
    static map<string, int> name_to_count;
    static int max_count;
private:

    const long long inlining_tree_id;
    const BooleanDagUtility * const skfunc = nullptr;

    void init();

protected:
    SkFuncSetter(const BooleanDagUtility* _skfunc);
    SkFuncSetter(const SkFuncSetter* to_copy);
    void soft_clear(bool clear_dag = true, bool sub_clear = false) const;

    bool assert_nonnull() const
    {
        assert(skfunc != nullptr);
        assert(LightSkFuncSetter::assert_nonnull());
        return true;
    }

public:

    const BooleanDagUtility* get_skfunc() const
    {
        return skfunc;
    }

    long long int get_tree_id() const;

};

template<typename BaseClass>
class TemplateInliningTree: public BaseClass
{

    mutable bool deleted = false;
    mutable map<string, string> find_dag_name_to_child_name;


    bool soft_clear(bool clear_root = true, bool sub_clear = false) const {
        if (deleted) {
            return true;
        }
        BaseClass::decrement_num_shared_ptr();
        if(BaseClass::get_num_shared_ptr() == 0) {
            deleted = true;
            for (const auto &it: var_name_to_inlining_subtree) {
                it.second->soft_clear(true, sub_clear);
            }
            BaseClass::soft_clear(clear_root, sub_clear);
            return true;
        }
        else
        {
            return false;
        }
    }

    void _clear(set<const TemplateInliningTree*>* visited = new set<const TemplateInliningTree*>()) const {
        if(deleted) {
            assert(visited->find(this) == visited->end());
            bool is_root = visited->empty();
            visited->insert(this);

            for (const auto &it: var_name_to_inlining_subtree) {
                if (visited->find(it.second) == visited->end()) {
                    it.second->_clear(visited);
                }
            }

            if (is_root) delete visited;
            delete this;
        }
    }

protected:
    map<string, const TemplateInliningTree*> var_name_to_inlining_subtree;
public:
    void insert_into_var_name_to_inlining_subtree(const string& key, const TemplateInliningTree* val) {
        assert(var_name_to_inlining_subtree.find(key) == var_name_to_inlining_subtree.end());
        var_name_to_inlining_subtree[key] = val;
    }
    void erase_from_var_name_to_inlining_subtree(const string& key) {
        assert(var_name_to_inlining_subtree.find(key) != var_name_to_inlining_subtree.end());
        var_name_to_inlining_subtree.at(key)->clear();
        var_name_to_inlining_subtree.erase(key);
    }
    const map<string, const TemplateInliningTree*>& get_var_name_to_inlining_subtree() const {
        return var_name_to_inlining_subtree;
    }
    void clear(bool clear_root = true, bool sub_clear = false) const {
        assert(!deleted);
        if(soft_clear(clear_root, sub_clear)) {
            _clear();
        }
    }
    int get_dag_id() const {return BaseClass::get_dag_id(); };
    const string& get_dag_name() const {return BaseClass::get_dag_name(); };
    TemplateInliningTree(const BooleanDagUtility* _skfunc, bool do_recurse): BaseClass(_skfunc) {assert(!do_recurse);}
    template<typename OtherBaseClass>
    TemplateInliningTree(const TemplateInliningTree<OtherBaseClass>* _to_copy, bool do_recurse): BaseClass(_to_copy) {assert(!do_recurse);}
    template<typename OtherBaseClass>
    explicit TemplateInliningTree(
            const TemplateInliningTree<OtherBaseClass>* to_copy,
            map<int, const TemplateInliningTree *> *visited = new map<int, const TemplateInliningTree *>()): BaseClass(to_copy) {
        bool is_root = visited->empty();
        assert(visited->find(get_dag_id()) == visited->end());
        (*visited)[get_dag_id()] = this;
        for (const auto &it: to_copy->get_var_name_to_inlining_subtree()) {
            if (visited->find(it.second->get_dag_id()) == visited->end()) {
                var_name_to_inlining_subtree[it.first] = new TemplateInliningTree(it.second, visited);
            } else {
                var_name_to_inlining_subtree[it.first] = (*visited)[it.second->get_dag_id()];
            }
        }
        assert_nonnull();
        if (is_root) {
            delete visited;
        }
    }

    explicit TemplateInliningTree(const BooleanDagUtility* _skfunc, map<const BooleanDagUtility*, const TemplateInliningTree *> *visited = new map<const BooleanDagUtility*, const TemplateInliningTree *>());

    TemplateInliningTree(const BooleanDagUtility *to_replace_root, const TemplateInliningTree *to_copy, map<int, const TemplateInliningTree *> *visited = new map<int, const TemplateInliningTree *>()):
        BaseClass(to_replace_root)
        {
        bool is_root = visited->empty();
        assert(visited->find(get_dag_id()) == visited->end());
        assert(visited->find(to_copy->get_dag_id()) == visited->end());
        (*visited)[get_dag_id()] = this;
        (*visited)[to_copy->get_dag_id()] = this;

        for(const auto& it: to_copy->var_name_to_inlining_subtree) {
            if(visited->find(it.second->get_dag_id()) == visited->end() ) {
                var_name_to_inlining_subtree[it.first] = new TemplateInliningTree(it.second, visited);
            }
            else {
                var_name_to_inlining_subtree[it.first] = (*visited)[it.second->get_dag_id()];
            }
        }
        if(is_root) delete visited;
    }

    bool assert_nonnull(set<const TemplateInliningTree*>* visited = new set<const TemplateInliningTree*>()) const {
        bool is_root = visited->empty();
        assert(visited->find(this) == visited->end());
        visited->insert(this);
        assert(BaseClass::assert_nonnull());
        for(auto it: var_name_to_inlining_subtree)
        {
            assert(it.second != nullptr);
            if(visited->find(it.second) == visited->end())
            {
                it.second->assert_nonnull(visited);
            }
        }
        if(is_root) delete visited;
        return true;
    }

    template<typename OtherBaseClass>
    bool match_topology(
            const TemplateInliningTree<OtherBaseClass> *other,
            set<const TemplateInliningTree*> *visited = new set<const TemplateInliningTree*>(),
            set<const TemplateInliningTree<OtherBaseClass>*> *other_visited = new set<const TemplateInliningTree<OtherBaseClass>*>()) const
    {
        assert(visited->empty() == other_visited->empty());
        bool is_root = visited->empty();

        assert_nonnull();
        other->assert_nonnull();

        assert(visited->size() == other_visited->size());
        assert(visited->find(this) == visited->end());
        visited->insert(this);
        assert(other_visited->find(other) == other_visited->end());
        other_visited->insert(other);
        assert(visited->size() == other_visited->size());

        for(const auto& it: var_name_to_inlining_subtree)
        {
            if(visited->find(it.second) == visited->end()) {
                assert(other->get_var_name_to_inlining_subtree().find(it.first) != other->get_var_name_to_inlining_subtree().end());
                assert(other_visited->find(other->get_var_name_to_inlining_subtree().at(it.first)) == other_visited->end());
                assert(it.second->match_topology(other->get_var_name_to_inlining_subtree().at(it.first), visited, other_visited));
            }
            else {
                assert(other->get_var_name_to_inlining_subtree().find(it.first) != other->get_var_name_to_inlining_subtree().end());
                assert(other_visited->find(other->get_var_name_to_inlining_subtree().at(it.first)) != other_visited->end());
            }
        }

        if(is_root)
        {
            delete visited;
            delete other_visited;
        }
        return true;
    }

    vector<string>* _find(const string& target_dag, set<const TemplateInliningTree*>* visited = new set<const TemplateInliningTree*>()) const
    {
        bool is_root = visited->empty();
        assert(visited->find(this) == visited->end());
        visited->insert(this);

        if(find_dag_name_to_child_name.find(target_dag) != find_dag_name_to_child_name.end())
        {
            string head_str = find_dag_name_to_child_name[target_dag];
            if(head_str == "#done") {
                if(is_root) delete visited;
                return new vector<string>();
            } else if(head_str == "#nullptr") {
                if(is_root) delete visited;
                return nullptr;
            }
            assert(var_name_to_inlining_subtree.find(head_str) != var_name_to_inlining_subtree.end());
            vector<string>* ret = var_name_to_inlining_subtree.at(head_str)->_find(target_dag, visited);
            ret->push_back(head_str);
            if(is_root) delete visited;
            return ret;
        }

        vector<string>* ret = nullptr;
        string head_str = "#nullptr";

        if(get_dag_name() == target_dag)
        {
            ret = new vector<string>();
            head_str = "#done";
        }
        else {
            for(const auto& it: var_name_to_inlining_subtree)  {
                if(visited->find(it.second) == visited->end()) {
                    vector<string>* tmp_ret = it.second->_find(target_dag);
                    if(tmp_ret != nullptr) {
                        AssertDebug(ret == nullptr, "IF THIS IS TRIGGERED, IT MEANS THAT THERE ARE MULTIPLE PATHS TO target_dag ("+target_dag+"). FIGURE OUT WHAT TO DO IN THIS CASE. PROBABLY MORE ASSERTS NEED TO BE ADDED WHERE THIS RESULT IS USED IN ORDER TO MAKE SURE THE USER KNOWS WHAT THEY ARE DOING. POSSIBLY NEED TO RETURN ALL PATHS.");
                        tmp_ret->push_back(it.first);
                        ret = tmp_ret;
                        head_str = it.first;
                        //break;
                    }
                }
            }
        }
        assert(find_dag_name_to_child_name.find(target_dag) == find_dag_name_to_child_name.end());
        find_dag_name_to_child_name[target_dag] = head_str;
        if(is_root) delete visited;
        return ret;
    }


    const vector<string>* find(const string& target_dag) const {
        return _find(target_dag);
    }

    virtual const TemplateInliningTree *get_sub_inlining_tree(const string &under_this_name) const {
        assert(var_name_to_inlining_subtree.find(under_this_name) != var_name_to_inlining_subtree.end());
        return var_name_to_inlining_subtree.at(under_this_name);
    }

    void _get_solution(VarStore* running_var_store, set<const TemplateInliningTree *> *visited = new set<const TemplateInliningTree *>()) const;

    const VarStore * get_solution() const {
        VarStore* ret = new VarStore();
        _get_solution(ret);
        return ret;
    }

    bool contains(const string& target_dag) const
    {
        const vector<string>* tmp_path = find(target_dag);
        bool ret = tmp_path != nullptr;
        delete tmp_path;
        return ret;
    }

    bool has_no_holes(set<string>* hole_names = new set<string>(), set<const TemplateInliningTree*>* visited = new set<const TemplateInliningTree*>()) const;
};

class LightInliningTree: public TemplateInliningTree<LightSkFuncSetter>
{

public:

    template<typename OtherBaseClass>
    LightInliningTree(const TemplateInliningTree<OtherBaseClass>* _to_copy, bool do_recurse): TemplateInliningTree<LightSkFuncSetter>(_to_copy, do_recurse){ assert(!do_recurse); };
    explicit LightInliningTree(const LightInliningTree* to_copy): TemplateInliningTree<LightSkFuncSetter>(to_copy) {}
    template<typename OtherBaseClass>
    explicit LightInliningTree(const TemplateInliningTree<OtherBaseClass>* to_copy): TemplateInliningTree<LightSkFuncSetter>(to_copy) {}
    explicit LightInliningTree(const BooleanDagUtility* _sk_func): TemplateInliningTree(_sk_func) {}
    LightInliningTree(
            const BooleanDagUtility* to_replace_root, const LightInliningTree *to_copy):
            TemplateInliningTree<LightSkFuncSetter>(to_replace_root, to_copy) {}

    const LightInliningTree *get_sub_inlining_tree(const string &under_this_name) const override {
        assert(var_name_to_inlining_subtree.find(under_this_name) != var_name_to_inlining_subtree.end());
        return (LightInliningTree*)var_name_to_inlining_subtree.at(under_this_name);
    }
};

class InliningTree: public TemplateInliningTree<SkFuncSetter>
{

public:
    //copy by replacing root skfunc
    InliningTree(const BooleanDagUtility* to_replace_root, const InliningTree *to_copy, map<const BooleanDagUtility *, const InliningTree *> *visited = new map<const BooleanDagUtility *, const InliningTree *>());
    //pure construct from skfunct
    InliningTree(const BooleanDagUtility* skfunc);
    //pure copy
    InliningTree(const InliningTree *to_copy):
            TemplateInliningTree<SkFuncSetter>(to_copy) {}

    void concretize(const VarStore * const var_store, bool is_root = false, set<const BooleanDagUtility*>* visited = new set<const BooleanDagUtility*>()) const;

    void print(int ntabs = 0, set<const InliningTree*>* visited = new set<const InliningTree*>()) const;

    void rename_var_store(VarStore &var_store, set<const InliningTree*> *visited = new set<const InliningTree*>(), const InliningTree *root = nullptr) const;

};


class BooleanDagLightUtility
{
    BooleanDAG* const root_dag = nullptr;
    ProgramEnvironment* env = nullptr;
    mutable int shared_ptr = 0;
    const string& dag_name;
    const int dag_id;

public:

    const int get_dag_id() const {
        return dag_id;
    }

    BooleanDagLightUtility(BooleanDAG* _root_dag):
            root_dag(_root_dag), dag_name(_root_dag->get_name()), dag_id(_root_dag->get_dag_id()) {
        assert(root_dag != nullptr);
        AssertDebug(env != nullptr, "env needs to be defined.");
    }

    BooleanDagLightUtility(BooleanDAG* _root_dag, ProgramEnvironment* _env): root_dag(_root_dag), env(_env), dag_name(_root_dag->get_name()), dag_id(_root_dag->get_dag_id()) {
        assert(root_dag != nullptr);
    }

    explicit BooleanDagLightUtility(BooleanDagLightUtility* to_copy): root_dag(to_copy->root_dag->clone()), env(to_copy->env), dag_name(to_copy->dag_name), dag_id(root_dag->get_dag_id()) {
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


    BooleanDagLightUtility* produce_concretization(const VarStore* const var_store, const bool_node::Type var_type) {
        BooleanDagLightUtility* ret = clone();
        vector<string>* inlined_functions = nullptr;
        ret->concretize_this_dag(var_store, var_type, inlined_functions);
        delete inlined_functions;
        return ret;
    }

    void concretize_this_dag(const VarStore* const var_store, bool_node::Type var_type) {
        vector<string>* inlined_functions = nullptr;
        concretize_this_dag(var_store, var_type, inlined_functions);
    }

    void concretize_this_dag(const VarStore* const _var_store, bool_node::Type var_type, vector<string>*& inlined_functions)
    {
        const VarStore* var_store = nullptr;
        if(_var_store == nullptr) {
            var_store = new VarStore();
        }
        else {
            var_store = _var_store;
        }

        if (new_way) {
            assert(var_store != nullptr);
            env->doInline(*root_dag, *var_store, var_type, inlined_functions);
        } else {
            assert(var_store != nullptr);
            hardCodeINodeNoClone(root_dag, *var_store, var_type, env->get_floats());
            inlined_functions = nullptr;
        }

        if(_var_store == nullptr)
        {
            assert(var_store != nullptr);
            assert(var_store->size() == 0);
            delete var_store;
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

    virtual void clear(LightInliningTree*& inlining_tree) {
        if(soft_clear(inlining_tree)){
            assert(shared_ptr == 0);
            delete this;
        }
        else {
            assert(shared_ptr >= 1);
        }
    }


    virtual void clear() {
        LightInliningTree* tmp = nullptr;
        clear(tmp);
    }

    virtual bool soft_clear(LightInliningTree*& inlining_tree)
    {
        decrement_shared_ptr_wo_clear();

        bool clear_inlining_tree = false;
        bool clear_solution = false;

        if(inlining_tree != nullptr) {
            assert(inlining_tree->get_dag_id() == dag_id);
            if(shared_ptr == 0) {
                clear_inlining_tree = true;

                LightInliningTree* tmp_inlining_tree = inlining_tree;
                inlining_tree = nullptr;
                tmp_inlining_tree->clear(false, true);
//                assert(shared_ptr == 1);
//                decrement_shared_ptr_wo_clear();
//                assert(shared_ptr == 0);
            }

//            else if(shared_ptr == 2)
//            {
//                if(solution != nullptr) {
//                    assert(solution->get_assignment()->get_inlining_tree() != nullptr);
//                    if(solution->get_assignment()->get_inlining_tree()->get_dag_id() == dag_id) {
//                        if (solution->get_num_shared_ptr() == 0) {
//
//                            clear_solution = true;
//
//                            const SolverLanguagePrimitives::HoleAssignment *tmp_solution = solution;
//                            solution = nullptr;
//                            tmp_solution->clear_assert_num_shared_ptr_is_0(false, true);
//                            assert(shared_ptr == 2);
//                            decrement_shared_ptr_wo_clear();
//                            assert(shared_ptr == 1);
//
//                            clear_inlining_tree = true;
//
//                            InliningTree* tmp_inlining_tree = inlining_tree;
//                            inlining_tree = nullptr;
//                            tmp_inlining_tree->clear(false, true);
//                            assert(shared_ptr == 1);
//                            decrement_shared_ptr_wo_clear();
//                            assert(shared_ptr == 0);
//                        }
//                        else
//                        {
//                            //don't clear bc inlining skfunc doesnt match.
//                            AssertDebug(false, "all non-0 shared_ptr of solution are cloned before going to the solver language");
//                        }
//                    }
//                    else
//                    {
//                        AssertDebug(false, "invariant not maintained");
//                        //don't clear either bc solution is still used.
//                    }
//                }
//                else
//                {
//                    //don't clear bc skfunc still used
//                }
//            }
//
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

    void increment_shared_ptr() const {
//        if(get_dag()->get_dag_id() == 91)
//        {
//            cout << "here" << endl;
//        }
        assert(shared_ptr >= 0);
        shared_ptr++;
    }

    void decrement_shared_ptr_wo_clear() {
//        if(get_dag()->get_dag_id() == 91)
//        {
//            cout << "DECREMENTING (--) shared_ptr of " << get_dag_name() <<" from " << shared_ptr <<" to " << shared_ptr-1 << endl;
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
