//
// Created by kliment on 2/16/22.
//

#include "BooleanDagLightUtility.h"

long long LightInliningTree::global_clear_id = 0;

bool BooleanDagLightUtility::soft_clear_assert_num_shared_ptr_is_0()
{
    assert(shared_ptr == 0);
    size_t prev_num = BooleanDAG::get_allocated().size();
    assert(root_dag != nullptr);
    root_dag->clear();
    assert(prev_num - 1 == BooleanDAG::get_allocated().size());
    return true;
}

#include "File.h"

int BooleanDagLightUtility::count_passing_inputs(File *file) {
    int ret = 0;
    int num_0s = 0;
    int num_1s = 0;
    for(int i = 0;i<file->size();i++)
    {
//            file->at(i)->printBrief(cout);
        BooleanDagLightUtility* _dag = produce_concretization(file->at(i), bool_node::SRC);
        _dag->increment_shared_ptr();
        auto dag = _dag->get_dag();
        assert(dag->getNodesByType(bool_node::CTRL).empty());
        assert((dag->size() == 0) == (dag->get_failed_assert() == nullptr));
        if(dag->get_failed_assert() == nullptr) {
            ret += 1;
        }
        _dag->clear();
    }
    return ret;
}

#include "SketchFunction.h"

set<string> *BooleanDagLightUtility::get_inlined_functions(set<string> *ret) const {
    for(auto node_it: get_dag()->getNodesByType(bool_node::UFUN)) {
        auto* ufun_it = (UFUN_node*)node_it;
        string ufname = ufun_it->get_ufname();
        if(ret->find(ufname) == ret->end()) {
            ret->insert(ufname);
            assert(get_env()->function_map.find(ufname) != get_env()->function_map.end());
            ((BooleanDagUtility*)(get_env()->function_map.find(ufname)->second))->get_inlined_functions(ret);
        }
    }
    return ret;
}

#include <VarStore.h>


void LightInliningTree::
    _get_solution(VarStore* running_var_store, set<const LightInliningTree *> *visited) const {
    bool is_root = visited->empty();
    assert(visited->find(this) == visited->end());
    visited->insert(this);
    const VarStore* at_var_store = LightSkFuncSetter::get_var_store();
    if(at_var_store == nullptr){
        at_var_store = new VarStore();
    }
    else {
        append_join(*running_var_store, *at_var_store);
    }
    for(const auto& it: var_name_to_inlining_subtree){
        if(visited->find(it.second) == visited->end()) {
            it.second->_get_solution(running_var_store, visited);
        }
    }
    if(is_root) delete visited;
}

LightInliningTree::LightInliningTree(const BooleanDagUtility *_skfunc, map<int, LightInliningTree*>* visited): LightSkFuncSetter(_skfunc) {
    bool is_root = visited->empty();
    assert(visited->find(get_dag_id()) == visited->end());

//    if(_skfunc->get_dag_name() == "printf")
//    {
//        cout << "here" << endl
//    }

    (*visited)[get_dag_id()] = this;

    auto ufun_nodes = _skfunc->get_dag()->getNodesByType(bool_node::UFUN);
    auto skfunc_inlining_tree = _skfunc->get_inlining_tree(false);
    if(skfunc_inlining_tree == nullptr) {
        assert(_skfunc->get_var_store() == nullptr);
    }

    if(!ufun_nodes.empty()) {
        assert(skfunc_inlining_tree == nullptr);
    }

    if(skfunc_inlining_tree != nullptr) {
        assert(ufun_nodes.empty());
    }

    if(ufun_nodes.empty()) {
        if(skfunc_inlining_tree != nullptr) {
            assert(_skfunc->get_has_been_concretized());
            for(const auto& it: skfunc_inlining_tree->get_var_name_to_inlining_subtree()) {
                if(visited->find(it.second->get_dag_id()) == visited->end()) {
#ifndef NO_CLONE_INLINING_TREE
                    var_name_to_inlining_subtree[it.first] = new LightInliningTree(it.second, visited);
#else
                    it.second->increment_num_shared_ptr();
                    var_name_to_inlining_subtree[it.first] = it.second;
#endif
                }
                else
                {
//                    (*visited)[it.second->get_dag_id()]->increment_num_shared_ptr();
                    var_name_to_inlining_subtree[it.first] = (*visited)[it.second->get_dag_id()];
                }
            }
        }
    }
    else {
        for (auto it: ufun_nodes) {
            string var_name = ((UFUN_node *) it)->get_original_ufname();
            string sub_dag_name = ((UFUN_node *) it)->get_ufname();
            assert(_skfunc->get_env()->function_map.find(sub_dag_name) != _skfunc->get_env()->function_map.end());
            const BooleanDagUtility *sub_dag = _skfunc->get_env()->function_map[sub_dag_name];
            if (visited->find(sub_dag->get_dag_id()) == visited->end()) {
                if (var_name_to_inlining_subtree.find(var_name) != var_name_to_inlining_subtree.end()) {
                    assert(get_var_name_to_inlining_subtree().at(var_name)->get_dag_id() == sub_dag->get_dag_id());
                    assert(sub_dag != nullptr);
                } else {
                    var_name_to_inlining_subtree[var_name] = new LightInliningTree(sub_dag, visited);
                }
            } else {
                if (var_name_to_inlining_subtree.find(var_name) != var_name_to_inlining_subtree.end()) {
                    assert(var_name_to_inlining_subtree[var_name] == (*visited)[sub_dag->get_dag_id()]);
                } else {
//                    (*visited)[sub_dag->get_dag_id()]->increment_num_shared_ptr();
                    var_name_to_inlining_subtree[var_name] = (*visited)[sub_dag->get_dag_id()];
                }
            }
        }
    }
    assert_nonnull();
    get_solution();
    if(is_root){
        delete visited;
    }
}


void LightInliningTree::concretize(SketchFunction* skfunc, const VarStore * const var_store, set<int>* visited) const {

    bool is_root = visited->empty();

    assert(skfunc->get_dag_id() == get_dag_id());
    assert(visited->find(get_dag_id()) == visited->end());
    visited->insert(get_dag_id());


    bool has_been_concretized = skfunc->get_has_been_concretized();

    bool recurse = true;
    if(!is_root) {
        if(has_been_concretized) {
            recurse = false;
        }
    } else
    {
        assert(!has_been_concretized);
    }

    if(recurse) {
        set<string> ufuns = skfunc->ufun_names();
        for (const auto& it: var_name_to_inlining_subtree) {

            SketchFunction* next_skfunc = nullptr;
            string next_dag_name = it.second->get_dag_name();
            assert(ufuns.find(next_dag_name) != ufuns.end());
            assert(skfunc->get_env()->function_map.find(next_dag_name) != skfunc->get_env()->function_map.end());
            next_skfunc = skfunc->get_env()->function_map[next_dag_name];

            if (visited->find( it.second->get_dag_id()) == visited->end()) {
                if(var_store == nullptr) {
                    it.second->concretize(next_skfunc, var_store, visited);
                }
                else {
                    const VarStore* sub_var_store = var_store->get_sub_var_store(it.first);
                    it.second->concretize(next_skfunc, sub_var_store, visited);
                    delete sub_var_store;
                }
            }
            else
            {
                //do nothing, it has already been visited, which means it's already been concretized;
            }
        }
        if(!is_root && !has_been_concretized) {
            skfunc->_inplace_concretize(var_store, bool_node::CTRL);
        }
    }

    if(is_root) delete visited;
}

string tabs(int ntabs)
{
    string ret;
    for(int i = 0;i<ntabs;i++)
    {
        ret+="|  ";
    }
    return ret;
}

void LightInliningTree::print(int ntabs, set<const LightInliningTree*>* visited) const {
    bool is_root = visited->empty();
    assert(visited->find(this) == visited->end());

    for(auto it: *visited)
    {
        assert(it->get_dag_name() != get_dag_name());
    }

    visited->insert(this);

    cout << tabs(ntabs) << get_dag_name() << " { " << endl;
    for(const auto& it: var_name_to_inlining_subtree){
        cout << tabs(ntabs+1)<< it.first << " : " << endl;
        if(visited->find((const LightInliningTree*)it.second) == visited->end()) {
            ((const LightInliningTree*)it.second)->print(ntabs + 2, visited);
        }
        else
        {
            cout << tabs(ntabs+2) << "*" << it.second->get_dag_name() << endl;
        }
    }
    cout << tabs(ntabs) << "} " << endl;
    if(is_root) delete visited;
}

void LightInliningTree::rename_var_store(VarStore &var_store, const LightInliningTree* var_store_sub_inlining_tree, set<const LightInliningTree*>* visited, const TopologyMatcher* topology_matcher) const {

    bool is_root = visited->empty();
    if(is_root) {
        assert(var_store_sub_inlining_tree == nullptr);
        assert(topology_matcher == nullptr);
        is_root = true;
        var_store_sub_inlining_tree = var_store.get_inlining_tree();
        topology_matcher = new TopologyMatcher(this, var_store_sub_inlining_tree);
    }
    else {
        assert(!visited->empty());
        assert(var_store_sub_inlining_tree != nullptr);
        assert(topology_matcher != nullptr);
        //TODO: can assert that the inlining tree corresponding to this one in var_store.inlining_tree is the same as var_store_sub_inlining_tree
    }

    assert(visited->find(this) == visited->end());
    visited->insert(this);
//    this is backwards, should assert not this
//    if(SkFuncSetter::get_var_store() != nullptr) {
//        for (const auto& it: *SkFuncSetter::get_var_store()) {
//            const string& original_name = it.get_original_name();
//            if (original_name != "#PC") {
//                AssertDebug(var_store.has_original_name(original_name),
//                            "NODE.original_name(): " + original_name + " DOESN'T EXIST.");
//            }
//        }
//    }

    for (const auto& it: LightSkFuncSetter::get_unconc_map()) {
        const string& original_name = it.first;
        if (original_name != "#PC") {
            AssertDebug(var_store.has_original_name(original_name),
                        "NODE.original_name(): " + original_name + " DOESN'T EXIST.");
        }
    }

    var_store.check_rep();

    map<string, string> prev_name_to_new_name;

    for (const auto& it: LightSkFuncSetter::get_unconc_map()) {
        assert(it.second.find(get_dag_name()) != it.second.end());
        string new_name = it.second.at(get_dag_name());
        string original_name = it.first;
        if (original_name != "#PC") {
            const string& subdag_name = get_dag_name();

            string prev_name;

            var_store.rename(original_name, subdag_name, new_name, topology_matcher, prev_name);
            assert(!prev_name.empty());
            if (prev_name_to_new_name.find(prev_name) == prev_name_to_new_name.end()) {
                prev_name_to_new_name[prev_name] = subdag_name;
            } else {
                assert(prev_name_to_new_name[prev_name] == subdag_name);
            }
        } else {
            assert(new_name == "#PC");
        }
    }

    assert(prev_name_to_new_name.size() <= 1);
    if(!prev_name_to_new_name.empty()) {
        assert(prev_name_to_new_name.size() == 1);
        assert(var_store_sub_inlining_tree->get_dag_name() == prev_name_to_new_name.begin()->first);
        assert(get_dag_name() == prev_name_to_new_name.begin()->second);
        for(const auto& it: prev_name_to_new_name){
            var_store.rename_subdag(it.first, it.second);
        }
        //TODO: optimize this, doesn't need to go though var_store.change_id bc you already have access to the particular inlining_tree in the var_store.
        var_store.change_id(var_store_sub_inlining_tree->get_dag_name(), get_dag_id());
    }
    else {
        assert(prev_name_to_new_name.empty());
        if(var_store_sub_inlining_tree->get_var_store() == nullptr ||
                var_store_sub_inlining_tree->get_var_store()->size() == 0) {
            var_store.rename_subdag(var_store_sub_inlining_tree->get_dag_name(), get_dag_name());
            var_store.change_id(var_store_sub_inlining_tree->get_dag_name(), get_dag_id());
        }
        else
        {
            //means that you have hole values for already concretized holes; do nothing.
            assert(var_store_sub_inlining_tree->get_var_store()->size() >= 1);
            assert(LightSkFuncSetter::_get_num_unconcretized_holes() == 0);
        }
    }

    var_store.check_rep();

    for(const auto& it: var_name_to_inlining_subtree) {
        if(visited->find(it.second) == visited->end()) {
            it.second->rename_var_store(var_store, var_store_sub_inlining_tree->get_sub_inlining_tree(it.first), visited, topology_matcher);
        }
    }

    var_store.check_rep();

    if(is_root) delete visited;
}

bool LightInliningTree::has_no_holes(set<const LightInliningTree*>* visited) const {
    bool is_root = visited->empty();
    assert(visited->find(this) == visited->end());
    visited->insert(this);

    bool ret = true;
    if(LightSkFuncSetter::_get_num_unconcretized_holes() >= 1) {
        ret = false;
    }
    else {
        for (const auto &it: var_name_to_inlining_subtree) {
            assert(ret);
            if (visited->find(it.second) == visited->end()) {
                if (!(it.second)->has_no_holes(visited)) {
                    ret = false;
                    break;
                }
            }
        }
    }

    if(is_root) delete visited;

    return ret;
}

bool LightInliningTree::match_topology(const LightInliningTree *other) const
{
    TopologyMatcher topology_matcher(this, other);
    return true;
}

long long LightSkFuncSetter::inlining_tree_global_id = 0;
set<const LightSkFuncSetter*> LightSkFuncSetter::all_inlining_trees = set<const LightSkFuncSetter*>();
map<string, int> LightSkFuncSetter::name_to_count = map<string, int>();
int LightSkFuncSetter::max_count = 0;

long long int LightSkFuncSetter::get_tree_id() const {
    return inlining_tree_id;
}

void LightSkFuncSetter::init() {
    inlining_tree_id = inlining_tree_global_id++;
//    if(dag_id == 23)
//    {
//        cout << "here" << endl;
//    }
    assert(all_inlining_trees.find(this) == all_inlining_trees.end());
    all_inlining_trees.insert(this);
    if(name_to_count.find(get_dag_name()) == name_to_count.end()) {
        name_to_count[get_dag_name()] = 0;
    }
    name_to_count[get_dag_name()]+=1;
    max_count = max(max_count, name_to_count[get_dag_name()]);
//    cout << "#trees " << all_inlining_trees.size() <<", tree_id: " << inlining_tree_global_id << ", dag_name: " << get_dag_name() << ", count: "<< name_to_count[get_dag_name()] << ", ptr: " << this << endl;
//    cout << "";
}

LightSkFuncSetter::LightSkFuncSetter(const BooleanDagUtility *_skfunc): dag_name(_skfunc->get_dag_name()), dag_id(_skfunc->get_dag_id()){
    init();
    const VarStore* _var_store = _skfunc->get_var_store();
    if(_var_store != nullptr) {
        var_store = _var_store->clone();
    }
    for(auto it: _skfunc->get_dag()->getNodesByType(bool_node::CTRL)) {
        if(it->get_name() != "#PC") {
            string org_name = ((CTRL_node*)it)->get_original_name();
            string name = ((CTRL_node*)it)->get_name();
            assert(unconc_hole_original_name_to_name.find(org_name) == unconc_hole_original_name_to_name.end());
            unconc_hole_original_name_to_name[org_name] = map<string, string>();
            unconc_hole_original_name_to_name[org_name][get_dag_name()] = name;
        }
    }
}