//
// Created by kliment on 2/16/22.
//

#include "BooleanDagLightUtility.h"

long long LightInliningTree::global_clear_id = 0;
bool BooleanDagLightUtility::new_way = false;


bool BooleanDagLightUtility::soft_clear_assert_num_shared_ptr_is_0()
{
    assert(shared_ptr == 0);
    size_t prev_num = BooleanDAG::get_allocated().size();
    assert(root_dag != nullptr);
    root_dag->clear();
    assert(prev_num - 1 == BooleanDAG::get_allocated().size());
    return true;
}



bool BooleanDagLightUtility::get_has_been_concretized() const {
    return has_been_concretized;
}

#include "SketchFunction.h"

#include "File.h"
#include "SolverLanguageLexAndYaccHeader.h"

int BooleanDagLightUtility::count_passing_inputs(const File *file) {
    int ret = 0;
    for(int i = 0;i<file->size();i++) {
        bool passes = SketchFunctionEvaluator::new_passes(this, file->at(i))->get_bool(true, false);
        if(passes) {
            ret += 1;
        }
    }
    return ret;
}

set<string> *BooleanDagLightUtility::get_inlined_functions(set<string> *ret) const {
    for(auto node_it: get_dag()->getNodesByType(bool_node::UFUN)) {
        auto* ufun_it = (UFUN_node*)node_it;
        string ufname = ufun_it->get_ufun_name();
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

LightInliningTree::LightInliningTree(
        const BooleanDagUtility *_skfunc,
        const VarStore* _to_set_var_store, map<int, LightInliningTree*>* visited, set<LightInliningTree*>* not_owned): LightSkFuncSetter(_skfunc) {
    AssertDebug(_skfunc->get_inlining_tree(false) == nullptr, "if _skfunc has an inlining_tree, it shouldn't need to reconstruct it.");
    assert(!_skfunc->get_has_been_concretized());
    bool is_root = visited->empty();

    assert(visited->find(get_dag_id()) == visited->end());

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

//    if(get_dag_name() == "program_lvl0__id341__id374")
//    {
//        cout << "HERE" << endl;
//    }

    if(ufun_nodes.empty()) {
        if(skfunc_inlining_tree != nullptr) {
            skfunc_inlining_tree->clear();
            skfunc_inlining_tree = nullptr;
        }
        if(skfunc_inlining_tree != nullptr) {
            AssertDebug(false, "DEAD CODE (skfunc_inlining_tree should always be == nullptr here), otherwise you don't need to construct the inliing_tree");
            assert(_skfunc->get_has_been_concretized());
            for(const auto& it: skfunc_inlining_tree->get_var_name_to_inlining_subtree()) {
                if(visited->find(it.second->get_dag_id()) == visited->end()) {
                    var_name_to_inlining_subtree[it.first] = it.second;
                    var_name_to_inlining_subtree[it.first]->populate_and_assert_not_visited(visited, not_owned);
                    assert(not_owned->find(var_name_to_inlining_subtree[it.first]) != not_owned->end());
                    var_name_to_inlining_subtree[it.first]->increment_num_shared_ptr();
                    local_not_owned.insert(var_name_to_inlining_subtree[it.first]);
                }
                else
                {
                    var_name_to_inlining_subtree[it.first] = (*visited)[it.second->get_dag_id()];
                    if(not_owned->find(var_name_to_inlining_subtree[it.first]) != not_owned->end()) {
                        var_name_to_inlining_subtree[it.first]->increment_num_shared_ptr();
                        local_not_owned.insert(var_name_to_inlining_subtree[it.first]);
                    }
                }
            }
        }
    }
    else {
        for (auto it: ufun_nodes) {
            string var_name = ((UFUN_node *) it)->get_original_ufname();
            string sub_dag_name = ((UFUN_node *) it)->get_ufun_name();
            assert(_skfunc->get_env()->function_map.find(sub_dag_name) != _skfunc->get_env()->function_map.end());
            const BooleanDagUtility *sub_dag = _skfunc->get_env()->function_map[sub_dag_name];
            if (visited->find(sub_dag->get_dag_id()) == visited->end()) {
                if (var_name_to_inlining_subtree.find(var_name) != var_name_to_inlining_subtree.end()) {
                    assert(get_var_name_to_inlining_subtree().at(var_name)->get_dag_id() == sub_dag->get_dag_id());
                    assert(sub_dag != nullptr);
                } else {
                    if(sub_dag->get_inlining_tree(false) == nullptr) {
                        var_name_to_inlining_subtree[var_name] = new LightInliningTree(sub_dag, (const VarStore*)nullptr, visited, not_owned);
                    } else
                    {
                        var_name_to_inlining_subtree[var_name] = sub_dag->get_inlining_tree_non_const();
                        var_name_to_inlining_subtree[var_name]->populate_and_assert_not_visited(visited, not_owned);
                        assert(not_owned->find(var_name_to_inlining_subtree[var_name]) != not_owned->end());
                        var_name_to_inlining_subtree[var_name]->increment_num_shared_ptr();
                        local_not_owned.insert(var_name_to_inlining_subtree[var_name]);
                    }
                }
            } else {
                if (var_name_to_inlining_subtree.find(var_name) != var_name_to_inlining_subtree.end()) {
                    assert(var_name_to_inlining_subtree[var_name] == (*visited)[sub_dag->get_dag_id()]);
                } else {
                    var_name_to_inlining_subtree[var_name] = (*visited)[sub_dag->get_dag_id()];
                    if(not_owned->find(var_name_to_inlining_subtree[var_name]) != not_owned->end()) {
                        var_name_to_inlining_subtree[var_name]->increment_num_shared_ptr();
                        local_not_owned.insert(var_name_to_inlining_subtree[var_name]);
                    }
                }
            }
        }
    }
    assert_nonnull();
    get_solution();
    if(is_root){
        if(_to_set_var_store != nullptr) {
            set_var_store(_to_set_var_store);
        }
        delete visited;
        delete not_owned;
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
        has_been_concretized = false;
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
                    assert(var_store != nullptr);
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
            skfunc->_inplace_concretize__assert_subfuncts_are_concretized(var_store, bool_node::CTRL);
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

void LightInliningTree::rename_var_store(
        VarStore &var_store,
        const LightInliningTree* var_store_sub_inlining_tree,
        set<const LightInliningTree*>* visited, const TopologyMatcher* topology_matcher) const {

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

//    for (const auto& it: LightSkFuncSetter::get_unconc_map()) {
//        const string& original_name = it.first;
//        if (original_name != "#PC") {
//            AssertDebug(var_store.has_original_name(original_name),
//                        "NODE.original_name(): " + original_name + " DOESN'T EXIST IN VAR STORE. ARE YOU NOT CONCRETIZING FULLY?.");
//        }
//    }

    map<string, string> prev_name_to_new_name;

    for (const auto& it: LightSkFuncSetter::get_unconc_map()) {
        const string& original_name = it.first;
        if (original_name != "#PC") {
            if(!var_store.has_original_name(original_name)) {
                continue;
            }
        }
        assert(it.second.find(get_dag_name()) != it.second.end());
        string new_name = it.second.at(get_dag_name());
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

    for(const auto& it: var_name_to_inlining_subtree) {
        if(visited->find(it.second) == visited->end()) {
            it.second->rename_var_store(var_store, var_store_sub_inlining_tree->get_sub_inlining_tree(it.first), visited, topology_matcher);
        }
    }

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
    assert(other != nullptr);
    TopologyMatcher topology_matcher(this, other);
    return true;
}

void LightInliningTree::populate_and_assert_not_visited(map<int, LightInliningTree *> *visited, set<LightInliningTree *> *not_owned) {
    assert(visited->find(get_dag_id()) == visited->end());
    assert(not_owned->find(this) == not_owned->end());
    (*visited)[get_dag_id()] = this;
    not_owned->insert(this);
    for(const auto& it: var_name_to_inlining_subtree) {
        if(visited->find(it.second->get_dag_id()) == visited->end()) {
            it.second->populate_and_assert_not_visited(visited, not_owned);
        }
        else {
            assert(it.second == visited->find(it.second->get_dag_id())->second);
        }
    }
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

    assert(all_inlining_trees.find(this) == all_inlining_trees.end());
    all_inlining_trees.insert(this);
    if(name_to_count.find(get_dag_name()) == name_to_count.end()) {
        name_to_count[get_dag_name()] = 0;
    }
    name_to_count[get_dag_name()]+=1;
    max_count = max(max_count, name_to_count[get_dag_name()]);
//    if(inlining_tree_id == 470)
//    {
//        cout << "HERE" << endl;
//    }
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
//    if(inlining_tree_id == 0)
//    {
//        cout << "here" << endl;
//    }
}