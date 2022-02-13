//
// Created by kliment on 2/9/22.
//

#include "BooleanDagUtility.h"
#include "SketchFunction.h"

void BooleanDagUtility::swap_env(ProgramEnvironment *new_env) {
    assert(original_program_env == nullptr);
    original_program_env = env;
    assert(new_env != env);
    env = new_env;
}

void BooleanDagUtility::reset_env_to_original() {
    assert(original_program_env != nullptr);
    env = original_program_env;
    original_program_env = nullptr;
}

bool BooleanDagUtility::soft_clear_assert_num_shared_ptr_is_0()
{
    assert(shared_ptr == 0);
    int prev_num = BooleanDAG::get_allocated().size();
    assert(root_dag != nullptr);
    root_dag->clear();
    assert(prev_num - 1 == BooleanDAG::get_allocated().size());
//    if(inlining_tree != nullptr)
//    {
//        inlining_tree->clear();
//    }
    return true;
}

bool BooleanDagUtility::is_inlining_tree_nonnull() {
    bool ret = inlining_tree != nullptr;
    if(inlining_tree != nullptr) {
        assert(inlining_tree->get_skfunc() == this);
    }
    return ret;
}

InliningTree *& BooleanDagUtility::get_inlining_tree(bool assert_nonnull) {
    if(assert_nonnull)
    assert(inlining_tree != nullptr);
    if(inlining_tree != nullptr) {
        assert(inlining_tree->get_skfunc() == this);
    }
    return inlining_tree;
}

bool BooleanDagUtility::has_been_concretized() {
    return get_dag()->getNodesByType(bool_node::CTRL).empty();
    //|| ( get_dag()->getNodesByType(bool_node::CTRL).size() == 1 && ((CTRL_node*)*get_dag()->getNodesByType(bool_node::CTRL).begin())->get_name() == "#PC");
}

InliningTree::InliningTree(BooleanDagUtility *_sk_func, map<BooleanDagUtility*, InliningTree*>* visited): skfunc(_sk_func) {
    assert(visited->find(skfunc) == visited->end());

    for(auto it: *visited)
    {
        assert(it.second->skfunc->get_dag_name() != skfunc->get_dag_name());
    }

    (*visited)[skfunc] = this;
    skfunc->increment_shared_ptr();

    auto ufun_nodes = skfunc->get_dag()->getNodesByType(bool_node::UFUN);
    auto skfunc_inlining_tree = skfunc->get_inlining_tree(false);

    if(!ufun_nodes.empty()) {
        assert(skfunc_inlining_tree == nullptr);
    }

    if(skfunc_inlining_tree != nullptr) {
        assert(ufun_nodes.empty());
    }

    if(ufun_nodes.empty())
    {
        if(skfunc_inlining_tree != nullptr)
        {
            assert(skfunc_inlining_tree->skfunc == skfunc);
            for(const auto& it: skfunc->get_inlining_tree()->var_name_to_inlining_subtree)
            {
                if(visited->find(it.second->skfunc) == visited->end()) {
                    var_name_to_inlining_subtree[it.first] = new InliningTree(it.second, visited);
                }
                else
                {
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
                var_name_to_inlining_subtree[var_name] = (*visited)[sub_dag];
            }
        }
    }
    assert_nonnull();
}

void InliningTree::clear() {
    if(deleted)
    {
        return;
    }
    deleted = true;
    for(auto it: var_name_to_inlining_subtree)
    {
        it.second->clear();
    }
    var_name_to_inlining_subtree.clear();
    delete this;
}

SolverLanguagePrimitives::HoleAssignment *InliningTree::get_solution(set<InliningTree*>* visited) {
    assert(visited->find(this) == visited->end());
    visited->insert(this);
    auto root_solution = ((SketchFunction*)skfunc)->get_same_soluton();

    SolverLanguagePrimitives::HoleAssignment* ret = nullptr;
    if(root_solution != nullptr)
    {
        ret = new SolverLanguagePrimitives::HoleAssignment(root_solution);
    }

    for(auto it: var_name_to_inlining_subtree)
    {
        if(visited->find(it.second) == visited->end()) {
            if (ret == nullptr) {
                ret = new SolverLanguagePrimitives::HoleAssignment(it.second->get_solution());
            } else {
                ret->get_assignment()->disjoint_join_with(it.second->get_solution()->get_assignment());
            }
        }
    }
    return ret;
}

vector<string>* InliningTree::find(const string target_dag, set<BooleanDagUtility*>* visited) {
    assert(visited->find(skfunc) == visited->end());
    visited->insert(skfunc);
    if(skfunc->get_dag_name() == target_dag)
    {
        return new vector<string>();
    }
    else
    {
        for(auto it: var_name_to_inlining_subtree)
        {
            if(visited->find(it.second->skfunc) == visited->end()) {
                auto ret = it.second->find(target_dag);
                if(ret != nullptr) {
                    ret->push_back(it.first);
                    return ret;
                }
            }
        }
    }
    return nullptr;
}

bool InliningTree::match_topology(InliningTree *other, set<string> *visited, set<string> *other_visited) {

    assert_nonnull();
    other->assert_nonnull();

    assert(visited->find(skfunc->get_dag_name()) == visited->end());
    visited->insert(skfunc->get_dag_name());
    assert(other_visited->find(other->skfunc->get_dag_name()) == other_visited->end());
    other_visited->insert(other->skfunc->get_dag_name());
    assert(visited->size() == other_visited->size());
    cout << "UNDER " << skfunc->get_dag_name() <<" ~~ " << other->skfunc->get_dag_name() << endl;
    for(const auto& it: var_name_to_inlining_subtree)
    {
        string name = it.first;
        assert(other->var_name_to_inlining_subtree.find(it.first) != other->var_name_to_inlining_subtree.end());
        cout << name << " -> " << it.second->skfunc->get_dag_name() <<" ~~ " << other->var_name_to_inlining_subtree.find(it.first)->second->skfunc->get_dag_name() << endl;
    }

    for(auto it: var_name_to_inlining_subtree)
    {
        if(visited->find(it.second->skfunc->get_dag_name()) == visited->end()) {
            assert(other_visited->find(other->var_name_to_inlining_subtree[it.first]->skfunc->get_dag_name()) == other_visited->end());
            assert(it.second->match_topology(other->var_name_to_inlining_subtree[it.first], visited, other_visited));
        }
        else
        {
            assert(other_visited->find(other->var_name_to_inlining_subtree[it.first]->skfunc->get_dag_name()) != other_visited->end());
        }
    }
    return true;
}

InliningTree *InliningTree::get_sub_inlining_tree(const string &under_this_name) {
    assert(var_name_to_inlining_subtree.find(under_this_name) != var_name_to_inlining_subtree.end());
    return var_name_to_inlining_subtree[under_this_name];
}

void InliningTree::concretize(const VarStore& var_store, bool is_root, set<BooleanDagUtility*>* visited) {
    assert(visited->find(skfunc) == visited->end());
    visited->insert(skfunc);

    bool recurse = true;
    if(!is_root) {
        if(!skfunc->has_been_concretized()) {
//            ((SketchFunction *) skfunc)->produce_concretization(var_store, bool_node::CTRL, false, true);
        }
        else
        {
            recurse = false;
        }
    }

    if(recurse) {
        for (auto it: var_name_to_inlining_subtree) {
            VarStore* new_var_store = var_store.get_sub_var_store(it.first);
            if (visited->find(it.second->skfunc) == visited->end()) {
                it.second->concretize(*new_var_store, false, visited);
            }
        }
        if(!is_root && !skfunc->has_been_concretized()) {
            ((SketchFunction *) skfunc)->produce_concretization(var_store, bool_node::CTRL, false, false);
        }
    }
}

const BooleanDagUtility *InliningTree::get_skfunc() {
    return skfunc;
}

InliningTree::InliningTree(BooleanDagUtility *_skfunc, InliningTree *to_copy, map<BooleanDagUtility*, InliningTree*>* visited): skfunc(_skfunc) {
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

void InliningTree::print(int ntabs, set<InliningTree*>* visited) {
    assert(visited->find(this) == visited->end());

    for(auto it: *visited)
    {
        assert(it->skfunc->get_dag_name() != skfunc->get_dag_name());
    }

    visited->insert(this);

    cout << tabs(ntabs) << skfunc->get_dag_name() << " { " << endl;
    for(auto it: var_name_to_inlining_subtree){
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

}

#include <VarStore.h>

void InliningTree::rename_var_store(VarStore &var_store, set<InliningTree*>* visited, InliningTree* root) {
    assert(visited->find(this) == visited->end());
    visited->insert(this);

    bool is_root = false;
    if(root == nullptr)
    {
        is_root = true;
        root = this;
    }

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
            cout << "RENAME " << original_name <<" -> " << new_name << " OF DAG " << subdag_name << endl;
            var_store.rename(original_name, subdag_name, new_name, root);
        }
        else {
            assert(new_name == "#PC");
        }
    }

    for(auto it: var_name_to_inlining_subtree) {
        if(visited->find(it.second) == visited->end()) {
            it.second->rename_var_store(var_store, visited, root);
        }
    }

    if(is_root)
    {
        var_store.set_inlining_tree(this);
        var_store.check_rep();
    }
}

set<string> *InliningTree::get_inlined_function(set<string>* inlined_functions, set<InliningTree*>* visited) {
    assert(visited->find(this) == visited->end());
    visited->insert(this);

    for(auto it: var_name_to_inlining_subtree)
    {
        inlined_functions->insert(it.second->skfunc->get_dag_name());
        if(visited->find(it.second) == visited->end()) {
            it.second->get_inlined_function(inlined_functions, visited);
        }
    }

    return inlined_functions;
}
