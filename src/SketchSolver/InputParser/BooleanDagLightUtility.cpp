//
// Created by kliment on 2/16/22.
//

#include "BooleanDagLightUtility.h"

bool BooleanDagLightUtility::soft_clear_assert_num_shared_ptr_is_0()
{
    assert(shared_ptr == 0);
    int prev_num = BooleanDAG::get_allocated().size();
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
        assert(dag->getNodesByType(bool_node::CTRL).size() == 0);
        assert((dag->size() == 0) == (dag->get_failed_assert() == nullptr));
        if(dag->get_failed_assert() == nullptr) {
            ret += 1;
        }
        _dag->clear();
    }
    return ret;
}

#include "SketchFunction.h"

set<string> *BooleanDagLightUtility::get_inlined_functions(set<string> *ret) {
    for(auto node_it: get_dag()->getNodesByType(bool_node::UFUN)) {
        UFUN_node* ufun_it = (UFUN_node*)node_it;
        string ufname = ufun_it->get_ufname();
        if(ret->find(ufname) == ret->end()) {
            ret->insert(ufname);
            assert(get_env()->function_map.find(ufname) != get_env()->function_map.end());
            ((BooleanDagUtility*)(get_env()->function_map.find(ufname)->second))->get_inlined_functions(ret);
        }
    }
    return ret;
}



InliningTree::InliningTree(BooleanDagUtility *_skfunc, map<BooleanDagUtility *, const InliningTree *> *visited): SkFuncSetter(_skfunc) {
    bool is_root = visited->empty();
    assert(visited->find(skfunc) == visited->end());

    for(auto it: *visited)
    {
        assert(it.second->skfunc->get_dag_name() != skfunc->get_dag_name());
    }

    (*visited)[skfunc] = this;

    auto ufun_nodes = skfunc->get_dag()->getNodesByType(bool_node::UFUN);
    auto skfunc_inlining_tree = skfunc->get_inlining_tree(false);

    if(!ufun_nodes.empty()) {
        assert(skfunc_inlining_tree == nullptr);
    }

    if(skfunc_inlining_tree != nullptr) {
        assert(ufun_nodes.empty());
    }

    if(ufun_nodes.empty()) {
        if(skfunc_inlining_tree != nullptr) {
            assert(skfunc_inlining_tree->skfunc == skfunc);
            for(const auto& it: skfunc->get_inlining_tree()->var_name_to_inlining_subtree) {
                if(visited->find(it.second->skfunc) == visited->end()) {
                    var_name_to_inlining_subtree[it.first] = new InliningTree(it.second, visited);
                }
                else {
                    var_name_to_inlining_subtree[it.first] = (*visited)[it.second->skfunc];
                }
            }
        }
    }
    else {
        for (auto it: ufun_nodes) {
            string var_name = ((UFUN_node *) it)->get_original_ufname();
            string sub_dag_name = ((UFUN_node *) it)->get_ufname();
            assert(skfunc->get_env()->function_map.find(sub_dag_name) != skfunc->get_env()->function_map.end());
            BooleanDagUtility *sub_dag = skfunc->get_env()->function_map[sub_dag_name];
            if (visited->find(sub_dag) == visited->end()) {
                if (var_name_to_inlining_subtree.find(var_name) != var_name_to_inlining_subtree.end()) {
                    assert(var_name_to_inlining_subtree[var_name]->skfunc == sub_dag);
                    assert(sub_dag != nullptr);
                } else {
                    var_name_to_inlining_subtree[var_name] = new InliningTree(sub_dag, visited);
                }
            } else {
                if (var_name_to_inlining_subtree.find(var_name) != var_name_to_inlining_subtree.end()) {
                    assert(var_name_to_inlining_subtree[var_name] == (*visited)[sub_dag]);
                } else {
                    var_name_to_inlining_subtree[var_name] = (*visited)[sub_dag];
                }
            }
        }
    }
    assert_nonnull();
    if(is_root) delete visited;
}

void InliningTree::soft_clear(bool clear_root, bool sub_clear) const{
    if(deleted)
    {
        return;
    }
    deleted = true;
    for(const auto& it: var_name_to_inlining_subtree)
    {
        it.second->soft_clear(true, sub_clear);
    }
    SkFuncSetter::soft_clear(clear_root, sub_clear);
//    delete this;
}

void InliningTree::_clear(set<const InliningTree*>* visited) const {
    assert(deleted);
    assert(visited->find(this) == visited->end());
    bool is_root = visited->empty();
    visited->insert(this);

    for(const auto& it: var_name_to_inlining_subtree)
    {
        if(visited->find(it.second) == visited->end()) {
            it.second->_clear(visited);
        }
    }

    if(is_root) delete visited;
    delete this;
}

void InliningTree::clear(bool clear_root, bool sub_clear) const{
    assert(!deleted);
    soft_clear(clear_root, sub_clear);
    _clear();
}

SolverLanguagePrimitives::HoleAssignment *InliningTree::get_solution(set<const InliningTree *> *visited) const {
    bool is_root = visited->empty();
    assert(visited->find(this) == visited->end());
    visited->insert(this);
    auto root_solution = ((SketchFunction*)skfunc)->get_same_soluton();

    bool root_defied = false;
    SolverLanguagePrimitives::HoleAssignment* ret = nullptr;
    if(root_solution != nullptr)
    {
        root_defied = true;
        ret = new SolverLanguagePrimitives::HoleAssignment(root_solution);
        assert(ret->get_assignment()->get_inlining_tree()->get_skfunc() == skfunc);
    }

    for(const auto& it: var_name_to_inlining_subtree)
    {
        if(visited->find(it.second) == visited->end()) {
            if (ret == nullptr) {
                assert(!root_defied);
                ret = it.second->get_solution();
                const InliningTree* local_inlining_tree = ret->get_assignment()->get_inlining_tree();
                InliningTree* ret_inlining_tree = new InliningTree(skfunc, false);
                ret_inlining_tree->var_name_to_inlining_subtree[it.first] = local_inlining_tree;
                ret->get_assignment()->update_inlining_tree(ret_inlining_tree);
            } else {
                auto sub_solution = it.second->get_solution();
                auto assignment = sub_solution->get_assignment();
                ret->get_assignment()->disjoint_join_with(assignment);

                const InliningTree *local_inlining_tree = assignment->get_inlining_tree();
                assignment->set_inlining_tree_to_nullptr();
                sub_solution->clear_assert_num_shared_ptr_is_0();

                InliningTree *ret_inlining_tree = ret->get_assignment()->get_inlining_tree_nonconst();

                if(ret_inlining_tree->var_name_to_inlining_subtree.find(it.first) !=
                           ret_inlining_tree->var_name_to_inlining_subtree.end()) {
                    ret_inlining_tree->var_name_to_inlining_subtree[it.first]->clear();
                }

                ret_inlining_tree->var_name_to_inlining_subtree[it.first] = local_inlining_tree;
            }
        }
    }

    assert(ret->get_assignment()->get_inlining_tree()->get_skfunc() == skfunc);
    assert(skfunc != nullptr);

    if(is_root) delete visited;

    return ret;
}

///THIS RUNS IN n^2 memory and time. TODO: make it O(n) memory and time.
vector<string>* InliningTree::_find(const string& target_dag, set<BooleanDagUtility*>* visited) const {
    bool is_root = visited->empty();
    assert(visited->find(skfunc) == visited->end());
    visited->insert(skfunc);

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

    if(skfunc->get_dag_name() == target_dag)
    {
        ret = new vector<string>();
        head_str = "#done";
    }
    else {
        for(const auto& it: var_name_to_inlining_subtree)  {
            if(visited->find(it.second->skfunc) == visited->end()) {
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

bool InliningTree::match_topology(const InliningTree *other, set<string> *visited, set<string> *other_visited) const {
    assert(visited->empty() == other_visited->empty());
    bool is_root = visited->empty();

    assert_nonnull();
    other->assert_nonnull();

    assert(visited->size() == other_visited->size());
    assert(visited->find(skfunc->get_dag_name()) == visited->end());
    visited->insert(skfunc->get_dag_name());
    assert(other_visited->find(other->skfunc->get_dag_name()) == other_visited->end());
    other_visited->insert(other->skfunc->get_dag_name());
    assert(visited->size() == other_visited->size());

    for(const auto& it: var_name_to_inlining_subtree)
    {
        if(visited->find(it.second->skfunc->get_dag_name()) == visited->end()) {

            assert(other->var_name_to_inlining_subtree.find(it.first) != other->var_name_to_inlining_subtree.end());
            assert(other_visited->find(other->var_name_to_inlining_subtree.at(it.first)->skfunc->get_dag_name()) == other_visited->end());
            assert(it.second->match_topology(other->var_name_to_inlining_subtree.at(it.first), visited, other_visited));
        }
        else
        {
            assert(other->var_name_to_inlining_subtree.find(it.first) != other->var_name_to_inlining_subtree.end());
            assert(other_visited->find(other->var_name_to_inlining_subtree.at(it.first)->skfunc->get_dag_name()) != other_visited->end());
        }
    }

    if(is_root)
    {
        delete visited;
        delete other_visited;
    }
    return true;
}

void InliningTree::concretize(const VarStore * const var_store, bool is_root, set<BooleanDagUtility*>* visited) const {
    assert(is_root == visited->empty());
    assert(visited->find(skfunc) == visited->end());
    visited->insert(skfunc);

    bool recurse = true;
    if(!is_root) {
        if(skfunc->get_has_been_concretized()) {
            recurse = false;
        }
    }

    if(recurse) {
        for (const auto& it: var_name_to_inlining_subtree) {
            if (visited->find(it.second->skfunc) == visited->end()) {
                if(var_store == nullptr) {
                    it.second->concretize(var_store, false, visited);
                }
                else {
                    VarStore* sub_var_store = var_store->get_sub_var_store(it.first);
                    it.second->concretize(sub_var_store, false, visited);
                    sub_var_store->clear();
                }
            }
        }
        if(!is_root && !skfunc->get_has_been_concretized()) {
            ((SketchFunction *) skfunc)->produce_concretization(var_store, bool_node::CTRL, false, false);
        }
    }
    if(is_root) delete visited;
}

const BooleanDagUtility *InliningTree::get_skfunc() const {
    return skfunc;
}

InliningTree::InliningTree(BooleanDagUtility *to_replace_root, const InliningTree *to_copy, map<BooleanDagUtility *, const InliningTree *> *visited): SkFuncSetter(to_replace_root) {
    bool is_root = visited->empty();
    assert(visited->find(skfunc) == visited->end());
    assert(visited->find(to_copy->skfunc) == visited->end());
    (*visited)[skfunc] = this;
    (*visited)[to_copy->skfunc] = this;

    for(const auto& it: to_copy->var_name_to_inlining_subtree) {
        if(visited->find(it.second->skfunc) == visited->end() ) {
            var_name_to_inlining_subtree[it.first] = new InliningTree(it.second, visited);
        }
        else
        {
            var_name_to_inlining_subtree[it.first] = (*visited)[it.second->skfunc];
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

void InliningTree::print(int ntabs, set<const InliningTree*>* visited) const {
    bool is_root = visited->empty();
    assert(visited->find(this) == visited->end());

    for(auto it: *visited)
    {
        assert(it->skfunc->get_dag_name() != skfunc->get_dag_name());
    }

    visited->insert(this);

    cout << tabs(ntabs) << skfunc->get_dag_name() << " { " << endl;
    for(const auto& it: var_name_to_inlining_subtree){
        cout << tabs(ntabs+1)<< it.first << " : " << endl;
        if(visited->find(it.second) == visited->end()) {
            it.second->print(ntabs + 2, visited);
        }
        else
        {
            cout << tabs(ntabs+2) << "*" << it.second->skfunc->get_dag_name() << endl;
        }
    }
    cout << tabs(ntabs) << "} " << endl;
    if(is_root) delete visited;
}

#include <VarStore.h>

void InliningTree::rename_var_store(VarStore &var_store, set<const InliningTree*>* visited, const InliningTree *root) const {

    bool is_root = false;
    if(root == nullptr)
    {
        assert(visited->empty());
        is_root = true;
        root = this;
    }

    assert(visited->find(this) == visited->end());
    visited->insert(this);


    for (auto it:skfunc->get_dag()->getNodesByType(bool_node::CTRL)) {
        string original_name = ((CTRL_node *) it)->get_original_name();
        if(original_name != "#PC") {
            AssertDebug(var_store.has_original_name(original_name), "NODE.original_name(): " + original_name + " DOESN'T EXIST.");
        }
    }

    for (auto it: skfunc->get_dag()->getNodesByType(bool_node::CTRL)) {
        string new_name =  ((CTRL_node*)it)->get_name();
        string original_name = ((CTRL_node*)it)->get_original_name();
        if(original_name != "#PC") {
            string subdag_name = ((CTRL_node*)it)->get_source_dag_name();
            var_store.rename(original_name, subdag_name, new_name, root);
        }
        else {
            assert(new_name == "#PC");
        }
    }

    for(const auto& it: var_name_to_inlining_subtree) {
        if(visited->find(it.second) == visited->end()) {
            it.second->rename_var_store(var_store, visited, root);
        }
    }

    if(is_root) {
        var_store.set_inlining_tree(this);
    }

    if(is_root) delete visited;
}

bool InliningTree::has_no_holes(set<string>* hole_names, set<const InliningTree*>* visited) const {
    bool is_root = visited->empty();
    assert(visited->find(this) == visited->end());
    visited->insert(this);

    for(auto it:skfunc->get_dag()->getNodesByType(bool_node::CTRL))
    {
        hole_names->insert(((CTRL_node*)it)->get_name());
    }

    for(const auto& it: var_name_to_inlining_subtree)
    {
        if(visited->find(it.second) == visited->end()) {
            it.second->has_no_holes(hole_names, visited);
        }
    }

    bool ret = false;
    if(hole_names->empty()) {
        ret = true;
    }
    else {
        if (hole_names->size() == 1) {
            if (*hole_names->begin() == "#PC") {
                ret = true;
            }
        }
    }

    if(is_root)
    {
        delete visited;
        delete hole_names;
    }

    return ret;
}


long long SkFuncSetter::inlining_tree_global_id = 0;
set<const SkFuncSetter*> SkFuncSetter::all_inlining_trees = set<const SkFuncSetter*>();


SkFuncSetter::SkFuncSetter(BooleanDagUtility *_skfunc): skfunc(_skfunc), inlining_tree_id(inlining_tree_global_id++) {
    assert(skfunc != nullptr);
    skfunc->increment_shared_ptr();

    assert(all_inlining_trees.find(this) == all_inlining_trees.end());
    all_inlining_trees.insert(this);

//    if(inlining_tree_id == 680) {
//        cout << "break" << endl; //sol
//    }
}

void SkFuncSetter::soft_clear(bool clear_dag, bool sub_clear) const {

    assert(skfunc != nullptr);
    if(clear_dag)
    {
        if(sub_clear) {
            ((SketchFunction *) skfunc)->_clear();
        }
        else {
            ((SketchFunction *) skfunc)->clear();
        }
    }

    assert(all_inlining_trees.find(this) != all_inlining_trees.end());
    all_inlining_trees.erase(this);
}


int SkFuncSetter::get_id() const {
    return inlining_tree_id;
}
