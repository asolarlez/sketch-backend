//
// Created by kliment on 2/16/22.
//

#ifndef SKETCH_SOURCE_BOOLEANDAGLIGHTUTILITY_H
#define SKETCH_SOURCE_BOOLEANDAGLIGHTUTILITY_H

#include <utility>

#include "ProgramEnvironment.h"

class File;

class BooleanDagUtility;

class LightSkFuncSetter
{

public:
    static long long inlining_tree_global_id;
    static set<const LightSkFuncSetter*> all_inlining_trees;
    static map<string, int> name_to_count;
    static int max_count;
    long long int get_tree_id() const;
private:
    long long inlining_tree_id = -1;
    mutable bool cleared = false;
    mutable int num_shared_ptr = 1;

    string dag_name;
    int dag_id;

    VarStore* var_store = nullptr;

    map<string, map<string, string> > unconc_hole_original_name_to_name;

    void init();

protected:

    void rename_dag(string new_name)
    {
        assert(name_to_count.find(dag_name) != name_to_count.end());
        name_to_count[dag_name]--;
        if(name_to_count[dag_name] == 0)
        {
            name_to_count.erase(dag_name);
        }
        dag_name = std::move(new_name);
        if(name_to_count.find(dag_name) == name_to_count.end()) {
            name_to_count[dag_name] = 0;
        }
        name_to_count[dag_name]+=1;
        max_count = max(max_count, name_to_count[dag_name]);
    }

    void change_id(int new_id){
        dag_id = new_id;
    }

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
//        if(inlining_tree_id == 0)
//        {
//            cout << "here" << endl;
//        }
        num_shared_ptr--;
    }

    void soft_clear() const {
        assert(get_num_shared_ptr() == 0);
        assert(!cleared);
        cleared = true;
        assert(name_to_count.find(get_dag_name()) != name_to_count.end());
        name_to_count[get_dag_name()] --;
//        if(dag_name == "printf__id14__id54")
//        {
//            cout << "here" << endl;
//        }
        if(name_to_count[get_dag_name()] == 0) {
            name_to_count.erase(get_dag_name());
        }
        assert(all_inlining_trees.find(this) != all_inlining_trees.end());
        all_inlining_trees.erase(this);
    }


    int _get_num_unconcretized_holes() const
    {
        int size = 0;
        for(const auto& it: unconc_hole_original_name_to_name)
        {
            assert(!it.second.empty());
            size+=it.second.size();
        }
        return size;
    }

    bool contains_var(
            const string &name, int nbits, const OutType *otype, bool_node::Type type, const string& original_name, const string& source_dag_name) const
    {
        assert(get_dag_name() == source_dag_name);
        assert(type == bool_node::CTRL);
        assert(var_store->contains(name));
        auto obj = var_store->getObjConst(name);
        assert(obj.otype == otype);
        assert(!obj.get_is_array());
        assert(obj.element_size() == nbits);
        assert(obj.get_original_name() == original_name);
        return true;
    }

    bool contains_var(
            const objP& obj) const
    {
        assert(contains_var(obj.get_name(), obj.element_size(), obj.otype, obj.get_type(), obj.get_original_name(), obj.get_source_dag_name()));
        return true;
    }

    bool insert_var(
            const objP& obj)
    {
        assert(get_dag_name() == obj.get_source_dag_name());
        assert(obj.get_type() == bool_node::CTRL);
        if(var_store == nullptr) {
            var_store = new VarStore();
        }
        if(!var_store->contains(obj.get_name())) {
            assert(unconc_hole_original_name_to_name.find(obj.get_original_name()) != unconc_hole_original_name_to_name.end());
            assert(unconc_hole_original_name_to_name[obj.get_original_name()].find(get_dag_name()) != unconc_hole_original_name_to_name[obj.get_original_name()].end());
            assert(unconc_hole_original_name_to_name[obj.get_original_name()][get_dag_name()] == obj.get_name());
            unconc_hole_original_name_to_name.erase(obj.get_original_name());
            var_store->insertObj(obj.get_name(), var_store->size(), objP(obj));
//            var_store->newVar(obj.get_name(), obj.element_size(), obj.otype, obj.get_type(), obj.get_original_name(), obj.get_source_dag_name());
        }
        else
        {
            auto _obj = var_store->getObjConst(obj.get_name());
            assert(obj.get_name() == _obj.get_name() && obj.element_size() == _obj.element_size() &&
                   obj.get_type() == _obj.get_type() &&
                   obj.get_original_name() == _obj.get_original_name() &&
                   obj.get_source_dag_name() == _obj.get_source_dag_name());
        }
        return true;
    }

    bool set_var_val(
            const string &name, int val, int nbits, const OutType *otype, bool_node::Type type, const string& original_name, const string& source_dag_name) const
    {
        assert(contains_var(name, nbits, otype, type, original_name, source_dag_name));
        var_store->setVarVal(name, val, otype, type);
        return true;
    }

    bool rename_var(
            const objP& obj, const string& new_name, const string& new_source_dag) const
    {
        assert(contains_var(obj));
        var_store->rename(obj, new_name, new_source_dag);
        return true;
    }

public:

    const map<string, map<string, string> >& get_unconc_map() const {
        return unconc_hole_original_name_to_name;
    }

    const VarStore* get_var_store() const {
        return var_store;
    }

    explicit LightSkFuncSetter(const LightSkFuncSetter* to_copy):
            dag_name(to_copy->dag_name), dag_id(to_copy->dag_id), unconc_hole_original_name_to_name(to_copy->unconc_hole_original_name_to_name) {
        assert(this != to_copy);
        assert(all_inlining_trees.find(this) == all_inlining_trees.end());
        init();
        if(to_copy->var_store != nullptr) {
            var_store = to_copy->var_store->clone();
        }
        if(inlining_tree_id == 0)
        {
            cout << "here" << endl;
        }
    }

    explicit LightSkFuncSetter(const BooleanDagUtility* _skfunc);

    int get_dag_id() const
    {
        return dag_id;
    }

    const string&  get_dag_name() const
    {
        return dag_name;
    }

    void increment_num_shared_ptr() {
        if(inlining_tree_id == 0)
        {
            cout << "here" << endl;
        }
        num_shared_ptr++;
    }
};
class LightInliningTree: public LightSkFuncSetter
{
    static long long global_clear_id;
    mutable long long local_clear_id = -1;
    mutable bool deleted = false;
public:
    bool has_this_been_deleted() const
    {
        return deleted;
    }
private:

    bool soft_clear(bool clear_root = true, bool sub_clear = false) const {
        if (deleted) {
            return true;
        }
        if(local_clear_id != global_clear_id) {
            local_clear_id = global_clear_id;
        }
        else {
            return false;
        }
        LightSkFuncSetter::decrement_num_shared_ptr();
        if(LightSkFuncSetter::get_num_shared_ptr() == 0) {
            deleted = true;
            for (const auto &it: var_name_to_inlining_subtree) {
                it.second->soft_clear(true, sub_clear);
            }
            LightSkFuncSetter::soft_clear();
            return true;
        }
        else
        {
            return false;
        }
    }

    void _clear(set<const LightInliningTree*>* visited = new set<const LightInliningTree*>()) const {
        if(deleted) {
            assert(get_num_shared_ptr() == 0);
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
        else{
            assert(get_num_shared_ptr() >= 1);
        }
    }

    map<string, LightInliningTree*> var_name_to_inlining_subtree;

    set<LightInliningTree*> local_not_owned;

public:
    const LightInliningTree* get_target(const string& target_subdag) const
    {
        const vector<string>* path = find_any_path(target_subdag);
        const LightInliningTree* at = this;
        for(int i = path->size()-1;i>=0;i--) {
            assert(at->var_name_to_inlining_subtree.find(path->at(i)) != at->var_name_to_inlining_subtree.end());
            at = at->var_name_to_inlining_subtree.at(path->at(i));
        }
        return at;
    }

    LightInliningTree* get_target_non_const(const string& target_subdag)
    {
        const vector<string>* path = find_any_path(target_subdag);
        assert(path != nullptr);
        LightInliningTree* at = this;
        for(int i = path->size()-1;i>=0;i--) {
            assert(at->var_name_to_inlining_subtree.find(path->at(i)) != at->var_name_to_inlining_subtree.end());
            at = at->var_name_to_inlining_subtree.at(path->at(i));
        }
        return at;
    }

    const map<string, LightInliningTree*>& get_var_name_to_inlining_subtree() const {
        return var_name_to_inlining_subtree;
    }

    void clear(bool clear_root = true, bool sub_clear = false) const {
        assert(!deleted);

        global_clear_id++;

        if(soft_clear(clear_root, sub_clear)) {
            _clear();
        }
    }

    int get_dag_id() const {return LightSkFuncSetter::get_dag_id(); };

    const string& get_dag_name() const {return LightSkFuncSetter::get_dag_name(); };

    void check_rep(const VarStore* to_check, set<const LightInliningTree*>* visited = new set<const LightInliningTree*>(), vector<string>* path = new vector<string>()) const
    {
        bool is_root = visited->empty();
        visited->insert(this);
        const VarStore* _var_store = LightSkFuncSetter::get_var_store();
        assert(_var_store != nullptr || LightSkFuncSetter::_get_num_unconcretized_holes() == 0);
        if(_var_store != nullptr){
            for(const objP& obj : *_var_store)
            {
                assert(to_check->contains(obj, path));
            }
        }
        for(const auto& it: var_name_to_inlining_subtree)
        {
            if(visited->find(it.second) == visited->end()) {
                path->push_back(it.first);
                it.second->check_rep(to_check, visited, path);
                path->pop_back();
            }
        }
        if(is_root) delete visited;
    }

    explicit LightInliningTree(
            const LightInliningTree* to_copy,
            bool deep_clone = false,
            map<int, LightInliningTree *> *visited = new map<int, LightInliningTree *>(),
            set<LightInliningTree *> *not_owned = new set<LightInliningTree *>()
    ):
            LightSkFuncSetter(to_copy)
    {
        bool is_root = visited->empty();
        assert(visited->find(get_dag_id()) == visited->end());
        (*visited)[get_dag_id()] = this;
        for (const auto &it: to_copy->get_var_name_to_inlining_subtree()) {
            if (visited->find(it.second->get_dag_id()) == visited->end()) {
                //todo: if it.second is not owned by to_copy; don't clone it
                if(!deep_clone && to_copy->local_not_owned.find(it.second) != to_copy->local_not_owned.end())
                {
                    var_name_to_inlining_subtree[it.first] = it.second;
                    var_name_to_inlining_subtree[it.first]->populate_and_assert_not_visited(visited, not_owned);
                    assert(not_owned->find(var_name_to_inlining_subtree[it.first]) != not_owned->end());
                    local_not_owned.insert(var_name_to_inlining_subtree[it.first]);
                    var_name_to_inlining_subtree[it.first]->increment_num_shared_ptr();
                }
                else {
                    var_name_to_inlining_subtree[it.first] = new LightInliningTree(it.second, deep_clone, visited, not_owned);
                }
            } else {
                var_name_to_inlining_subtree[it.first] = (*visited)[it.second->get_dag_id()];
                if(!deep_clone && not_owned->find(var_name_to_inlining_subtree[it.first]) != not_owned->end()) {
                    local_not_owned.insert(var_name_to_inlining_subtree[it.first]);
                    var_name_to_inlining_subtree[it.first]->increment_num_shared_ptr();
                }
            }
        }
        assert_nonnull();
        if (is_root) {
            delete visited;
            delete not_owned;
        }
    }

    explicit LightInliningTree(const BooleanDagUtility* _skfunc, const VarStore* to_set_var_store = nullptr, map<int, LightInliningTree *> *visited = new map<int, LightInliningTree *>(),
                               set<LightInliningTree *> *not_owned = new set<LightInliningTree *>());

    void populate_and_assert_not_visited(map<int, LightInliningTree *> *visited, set<LightInliningTree*>* not_owned);

    LightInliningTree(const BooleanDagUtility *to_replace_root, const LightInliningTree *to_copy,
                      map<int, LightInliningTree *> *visited = new map<int, LightInliningTree *>(),
                      set<LightInliningTree *> *not_owned = new set<LightInliningTree *>()):
            LightSkFuncSetter(to_replace_root)
    {
        bool is_root = visited->empty();
        assert(visited->find(get_dag_id()) == visited->end());
        assert(visited->find(to_copy->get_dag_id()) == visited->end());
        (*visited)[get_dag_id()] = this;
        (*visited)[to_copy->get_dag_id()] = this;

        for(const auto& it: to_copy->var_name_to_inlining_subtree) {
            if(visited->find(it.second->get_dag_id()) == visited->end() ) {
                var_name_to_inlining_subtree[it.first] = it.second;
                var_name_to_inlining_subtree[it.first]->populate_and_assert_not_visited(visited, not_owned);
                assert(not_owned->find(var_name_to_inlining_subtree[it.first]) != not_owned->end());
                local_not_owned.insert(var_name_to_inlining_subtree[it.first]);
                var_name_to_inlining_subtree[it.first]->increment_num_shared_ptr();
            }
            else {
                var_name_to_inlining_subtree[it.first] = (*visited)[it.second->get_dag_id()];
                if(not_owned->find(var_name_to_inlining_subtree[it.first]) != not_owned->end()) {
                    local_not_owned.insert(var_name_to_inlining_subtree[it.first]);
                    var_name_to_inlining_subtree[it.first]->increment_num_shared_ptr();
                }
            }
        }
        if(is_root)
        {
            delete visited;
            delete not_owned;
        }
    }

    bool assert_nonnull(set<const LightInliningTree*>* visited = new set<const LightInliningTree*>()) const {
        bool is_root = visited->empty();
        assert(visited->find(this) == visited->end());
        visited->insert(this);
        assert(LightSkFuncSetter::assert_nonnull());
        for(const auto& it: var_name_to_inlining_subtree)
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

    bool match_topology(const LightInliningTree *other) const;

//    const LightInliningTree *
//    get_corresponding(const string &target_dag_name, const LightInliningTree *in_this_tree) const {
//        TopologyMatcher topology_matcher(this, in_this_tree);
//        return topology_matcher.get_corresponding(target_dag_name);
//    }

    vector<string>* _find(const string& target_dag, bool assert_at_most_one_path, set<const LightInliningTree*>* visited = new set<const LightInliningTree*>()) const
    {
        bool is_root = visited->empty();
        assert(visited->find(this) == visited->end());
        visited->insert(this);

//        if(find_dag_name_to_child_name.find(target_dag) != find_dag_name_to_child_name.end())
//        {
//            string head_str = find_dag_name_to_child_name[target_dag];
//            if(head_str == "#done") {
//                if(is_root) delete visited;
//                return new vector<string>();
//            } else if(head_str == "#nullptr") {
//                if(is_root) delete visited;
//                return nullptr;
//            }
//            assert(var_name_to_inlining_subtree.find(head_str) != var_name_to_inlining_subtree.end());
//            vector<string>* ret = var_name_to_inlining_subtree.at(head_str)->_find(target_dag, visited);
//            ret->push_back(head_str);
//            if(is_root) delete visited;
//            return ret;
//        }

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
                    vector<string>* tmp_ret = it.second->_find(target_dag, assert_at_most_one_path);
                    if(tmp_ret != nullptr) {
                        AssertDebug(ret == nullptr, "IF THIS IS TRIGGERED, IT MEANS THAT THERE ARE MULTIPLE PATHS TO target_dag ("+target_dag+"). FIGURE OUT WHAT TO DO IN THIS CASE. PROBABLY MORE ASSERTS NEED TO BE ADDED WHERE THIS RESULT IS USED IN ORDER TO MAKE SURE THE USER KNOWS WHAT THEY ARE DOING. POSSIBLY NEED TO RETURN ALL PATHS.");
                        tmp_ret->push_back(it.first);
                        ret = tmp_ret;
                        head_str = it.first;
                        if(!assert_at_most_one_path) {
                            break;
                        }
                    }
                }
            }
        }
//        assert(find_dag_name_to_child_name.find(target_dag) == find_dag_name_to_child_name.end());
//        find_dag_name_to_child_name[target_dag] = head_str;
        if(is_root) delete visited;
        return ret;
    }

    const vector<string>* find(const string& target_dag) const {
        return _find(target_dag, true);
    }

    const vector<string>* find_any_path(const string& target_dag) const {
        return _find(target_dag, false);
    }

    virtual const LightInliningTree *get_sub_inlining_tree(const string &under_this_name) const {
        assert(var_name_to_inlining_subtree.find(under_this_name) != var_name_to_inlining_subtree.end());
        return var_name_to_inlining_subtree.at(under_this_name);
    }

    virtual LightInliningTree *get_sub_inlining_tree_non_const(const string &under_this_name) const {
        assert(var_name_to_inlining_subtree.find(under_this_name) != var_name_to_inlining_subtree.end());
        return var_name_to_inlining_subtree.at(under_this_name);
    }

    void _get_solution(VarStore* running_var_store, set<const LightInliningTree *> *visited = new set<const LightInliningTree *>()) const;

    VarStore * get_solution() const {
        VarStore* ret = new VarStore();
        _get_solution(ret);
        return ret;
    }

    bool contains(const string& target_dag) const
    {
        const vector<string>* tmp_path = find_any_path(target_dag);
        bool ret = tmp_path != nullptr;
        delete tmp_path;
        return ret;
    }

    void print(int ntabs = 0, set<const LightInliningTree*>* visited = new set<const LightInliningTree*>()) const;

    bool contains_var(
            const string &name, int nbits, const OutType *otype, bool_node::Type type, string original_name, string source_dag_name) const
    {
        const LightInliningTree* target = get_target(source_dag_name);
        assert(target->LightSkFuncSetter::contains_var(name, nbits, otype, type, original_name, source_dag_name));
        return true;
    }

    bool contains_var(
            const objP& obj) const
    {
        const LightInliningTree* target = get_target(obj.get_source_dag_name());
        assert(target->LightSkFuncSetter::contains_var(obj.get_name(), obj.element_size(), obj.otype, obj.get_type(), obj.get_original_name(), obj.get_source_dag_name()));
        return true;
    }

    void insert_var(const objP& obj)
    {
        LightInliningTree* target = get_target_non_const(obj.get_source_dag_name());
        target->LightSkFuncSetter::insert_var(obj);
    }

    void set_var_val(
            const string &name, int val, int nbits, const OutType *otype, bool_node::Type type, string original_name, string source_dag_name) const
    {
        const LightInliningTree* target = get_target(source_dag_name);
        target->LightSkFuncSetter::set_var_val(name, val, nbits, otype, type, original_name, source_dag_name);
    }

    void rename_var(const objP& obj, const string& new_name, const string& new_source_dag) const
    {
        const string& prev_source_dag_name = obj.get_source_dag_name();
        const LightInliningTree* target = get_target(prev_source_dag_name);
        target->LightSkFuncSetter::rename_var(obj, new_name, new_source_dag);
//        target->LightSkFuncSetter::rename_dag(new_source_dag);
    }

    void rename_subdag(const string& prev_name, const string& new_name)
    {
        LightInliningTree* target = get_target_non_const(prev_name);
        target->LightSkFuncSetter::rename_dag(new_name);
    }

    void change_id(const string& prev_name, int new_id)
    {
        LightInliningTree* target = get_target_non_const(prev_name);
        target->LightSkFuncSetter::change_id(new_id);
    }

    void set_var_store(const VarStore* new_var_store)
    {
        assert(new_var_store != nullptr);
        for(const auto& obj: *new_var_store){
            insert_var(obj);
        }
    }

    bool has_no_holes(set<const LightInliningTree*>* visited = new set<const LightInliningTree*>()) const;

    void concretize(SL::SketchFunction* skfunc, const VarStore * const var_store, set<int>* visited = new set<int>()) const;

    void rename_var_store(VarStore &var_store, const LightInliningTree* var_store_inlining_tree = nullptr, set<const LightInliningTree*> *visited = new set<const LightInliningTree*>(), const TopologyMatcher* topology_matcher = nullptr) const;
};


class TopologyMatcher{

    map<const LightInliningTree *, const LightInliningTree *> visited = map<const LightInliningTree *, const LightInliningTree *>();
    map<const LightInliningTree *, const LightInliningTree *> other_visited = map<const LightInliningTree *, const LightInliningTree *>();

    bool match_topology(
            const LightInliningTree *this_, const LightInliningTree *other) {
        assert(visited.empty() == other_visited.empty());
        bool is_root = visited.empty();

        this_->assert_nonnull();
        other->assert_nonnull();

        assert(visited.size() == other_visited.size());
        assert(visited.find(this_) == visited.end());
        assert(other_visited.find(other) == other_visited.end());
        visited[this_] = other;
        other_visited[other] = this_;
        assert(visited.size() == other_visited.size());

        for (const auto &it: this_->get_var_name_to_inlining_subtree()) {
            if (visited.find(it.second) == visited.end()) {
                assert(other->get_var_name_to_inlining_subtree().find(it.first) !=
                       other->get_var_name_to_inlining_subtree().end());
                assert(other_visited.find(other->get_var_name_to_inlining_subtree().at(it.first)) ==
                       other_visited.end());
                assert(match_topology(it.second, other->get_var_name_to_inlining_subtree().at(it.first)));
            } else {
                assert(other->get_var_name_to_inlining_subtree().find(it.first) !=
                       other->get_var_name_to_inlining_subtree().end());
                assert(other_visited.find(other->get_var_name_to_inlining_subtree().at(it.first)) !=
                       other_visited.end());
            }
        }

        return true;
    }
    const LightInliningTree* this_ = nullptr;
    const LightInliningTree* other = nullptr;
public:

    TopologyMatcher(const LightInliningTree* _this_, const LightInliningTree *_other): this_(_this_), other(_other) {
        assert(match_topology(this_, other));
    }

    const LightInliningTree* get_corresponding(const string& target_dag_name) const {
        const LightInliningTree* to_this = this_->get_target(target_dag_name);
        assert(visited.find(to_this) != visited.end());
        return visited.at(to_this);
    }

    const LightInliningTree* get_this_() const {
        return this_;
    }

    const LightInliningTree* get_other() const {
        return other;
    }
};

class BooleanDagLightUtility
{
    BooleanDAG* const root_dag = nullptr;
    ProgramEnvironment* _env = nullptr;

    mutable int shared_ptr = 0;
    const string& dag_name;
    const long long dag_id;

    bool has_been_concretized = false;

public:

    void force_set_has_not_been_concretized()
    {
        has_been_concretized = false;
    }

    static bool new_way;

    vector<bool> evaluate_inputs();

    bool get_has_been_concretized() const ;

    bool has_been_inlined() const
    {
        for(auto _it : get_dag()->getNodesByType(bool_node::UFUN)) {
            auto it = (UFUN_node*)_it;
            if(get_env()->function_map.find(it->get_name()) != get_env()->function_map.end())
            {
                return false;
            }
        }
        return true;
    }

    const int get_dag_id() const {
        return dag_id;
    }

    BooleanDagLightUtility(BooleanDAG* _root_dag):
            root_dag(_root_dag), dag_name(_root_dag->get_name()), dag_id(_root_dag->get_dag_id()) {
        assert(root_dag != nullptr);
//        assert(_root_dag->getNodesByType(bool_node::UFUN).empty()); TODO: uncomment this: disambiguate funs from ufuns.
    }

    BooleanDagLightUtility(BooleanDAG* _root_dag, ProgramEnvironment* __env, bool _has_been_concretized):
        root_dag(_root_dag), _env(__env), dag_name(_root_dag->get_name()), dag_id(_root_dag->get_dag_id()), has_been_concretized(_has_been_concretized) {
        assert(root_dag != nullptr);
    }

    explicit BooleanDagLightUtility(BooleanDagLightUtility* to_copy):
        root_dag(to_copy->root_dag->clone()), _env(to_copy->get_env()), dag_name(to_copy->dag_name), dag_id(to_copy->get_dag_id()), has_been_concretized(to_copy->has_been_concretized) {
        assert(root_dag != nullptr);
    }

    set<string>* get_inlined_functions(set<string>* ret = new set<string>()) const;

    BooleanDAG* get_dag() const {
        return root_dag;
    }

    const string & get_dag_name() const {
        return dag_name;
    }

    ProgramEnvironment* get_env() const {
        assert(_env != nullptr);
        return _env;
    }

    ProgramEnvironment*& get_env_ref() {
        return _env;
    }

    BooleanDagLightUtility* _clone(bool use_same_name = false) {

//        AssertDebug(get_dag()->getNodesByType(bool_node::UFUN).empty(),
//                    "NEED THIS TO HOLD BC WHEN YOU CLONE YOU WANT A DEEP CLONE; "
//                    "BUT THERE IS NO DEEP CLONE IN THIS CLASS. HERE CLONE DOES UNIT_CLONE, WHICH IS NOT WHAT YOU WANT (DUE TO RECURSIVE SKETCHES).");
//                    TODO: uncomment this: disambiguate funs from ufuns.

        BooleanDAG* new_dag = nullptr;
        if(use_same_name) {
            new_dag = get_dag()->clone(get_dag_name());
        }
        else {
            new_dag = get_dag()->clone();
        }
        return new BooleanDagLightUtility(new_dag, get_env(), has_been_concretized);
    }

//    BooleanDagLightUtility* produce_inlined_dag(bool use_same_name = false) {
//        BooleanDagLightUtility* ret = clone(use_same_name);
//        vector<string>* inlined_functions = nullptr;
//        ret->concretize_this_dag(nullptr, bool_node::CTRL, inlined_functions);
//        delete inlined_functions;
//        return ret;
//    }

    BooleanDagLightUtility* produce_inlined_dag(bool use_same_name = false) {
        AssertDebug(get_dag()->getNodesByType(bool_node::UFUN).empty(), "NEED THIS TO HOLD BC WHEN YOU CLONE YOU WANT A DEEP CLONE; BUT THERE IS NO DEEP CLONE IN THIS CLASS. HERE CLONE DOES UNIT_CLONE, WHICH IS NOT WHAT YOU WANT (DUE TO RECURSIVE SKETCHES).");
        BooleanDagLightUtility* ret = _clone(use_same_name);
        vector<string>* inlined_functions = nullptr;
        ret->concretize_this_dag(nullptr, bool_node::CTRL, inlined_functions);
        delete inlined_functions;
        return ret;
    }


//    BooleanDagLightUtility* produce_concretization(const VarStore* const var_store, const bool_node::Type var_type) {
//        BooleanDagLightUtility* ret = clone();
//        vector<string>* inlined_functions = nullptr;
//        ret->concretize_this_dag(var_store, var_type, inlined_functions);
//        delete inlined_functions;
//        return ret;
//    }

    BooleanDagLightUtility* produce_concretization(const VarStore* const var_store, const bool_node::Type var_type) {
//        AssertDebug(get_dag()->getNodesByType(bool_node::UFUN).empty(),
//        "NEED THIS TO HOLD BC WHEN YOU CLONE YOU WANT A DEEP CLONE;
//        BUT THERE IS NO DEEP CLONE IN THIS CLASS.
//        HERE CLONE DOES UNIT_CLONE, WHICH IS NOT WHAT YOU WANT (DUE TO RECURSIVE SKETCHES).");
//        TODO: uncomment this: disambiguate funs from ufuns.
        BooleanDagLightUtility* ret = _clone();
        vector<string>* inlined_functions = nullptr;
        ret->concretize_this_dag(var_store, var_type, inlined_functions);
        delete inlined_functions;
        return ret;
    }


    void concretize_this_dag(const VarStore* const var_store, bool_node::Type var_type) {
        vector<string>* inlined_functions = nullptr;
        concretize_this_dag(var_store, var_type, inlined_functions);
    }

    void inline_this_dag() {
        vector<string>* inlined_functions = nullptr;
        concretize_this_dag(nullptr, bool_node::CTRL, inlined_functions);
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

        assert(get_dag()->check_ctrl_node_source_dag_naming_invariant());

        if (new_way) {
            assert(var_store != nullptr);
            get_env()->doInline(*root_dag, *var_store, var_type, inlined_functions);
        } else {
            assert(var_store != nullptr);
            hardCodeINodeNoClone(root_dag, *var_store, var_type, get_env()->get_floats());
            inlined_functions = nullptr;
        }

//        assert(get_dag()->check_ctrl_node_source_dag_naming_invariant());

        if(_var_store == nullptr)
        {
            assert(var_store != nullptr);
            assert(var_store->size() == 0);
            delete var_store;
        }

        has_been_concretized = true;

    }

    virtual bool soft_clear_assert_num_shared_ptr_is_0();

    virtual int count_passing_inputs(const File *file);

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

                const LightInliningTree* tmp_inlining_tree = inlining_tree;
                inlining_tree = nullptr;
                tmp_inlining_tree->clear(false, true);
            }
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
//        if(get_dag()->get_dag_id() == 306)
//        {
//            cout << "here" << endl;
//        }
        assert(shared_ptr >= 0);
        shared_ptr++;
    }

    void decrement_shared_ptr_wo_clear() {
//        if(get_dag()->get_dag_id() == 306)
//        {
//            cout << "here " << "DECREMENTING (--) shared_ptr of " << get_dag_name() <<" from " << shared_ptr <<" to " << shared_ptr-1 << endl;
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
