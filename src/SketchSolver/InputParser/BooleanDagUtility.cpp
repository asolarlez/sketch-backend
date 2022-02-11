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
    return inlining_tree != nullptr;
}

InliningTree *BooleanDagUtility::get_inlining_tree(bool assert_nonnull) {
    if(assert_nonnull)
    assert(inlining_tree != nullptr);
    return inlining_tree;
}

InliningTree::InliningTree(BooleanDagUtility *_sk_func, map<BooleanDagUtility*, InliningTree*>& visited): skfunc(_sk_func) {
    assert(visited.find(skfunc) == visited.end());
    visited[skfunc] = this;
    skfunc->increment_shared_ptr();

    string cmp_pred = "composite_predicate";
    string name = skfunc->get_dag_name();
    if(name.substr(0, cmp_pred.size()) == cmp_pred)
    {
        assert(skfunc->get_dag()->getNodesByType(bool_node::UFUN).size() == 3);
    }

    for(auto it: skfunc->get_dag()->getNodesByType(bool_node::UFUN))
    {
        string var_name = ((UFUN_node*)it)->get_original_ufname();
        string sub_dag_name = ((UFUN_node*)it)->get_ufname();
        assert(skfunc->get_env()->function_map.find(sub_dag_name) != skfunc->get_env()->function_map.end());
        BooleanDagUtility* sub_dag = skfunc->get_env()->function_map[sub_dag_name];
        if(visited.find(sub_dag) == visited.end()) {
            if(var_name_to_inlining_subtree.find(var_name) != var_name_to_inlining_subtree.end()) {
                assert(var_name_to_inlining_subtree[var_name]->skfunc == sub_dag);
            }
            else {
                var_name_to_inlining_subtree[var_name] = new InliningTree(sub_dag, visited);
            }
        } else {
            var_name_to_inlining_subtree[var_name] = visited[sub_dag];
        }
    }


    if(name.substr(0, cmp_pred.size()) == cmp_pred)
    {
        assert(var_name_to_inlining_subtree.size() == 3);
    }
}

void InliningTree::clear() {
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

InliningTree::InliningTree(BooleanDagUtility *_skfunc): skfunc(_skfunc)
{
    skfunc->increment_shared_ptr();
    map<BooleanDagUtility*, InliningTree*> visited;
    visited[skfunc] = this;
    string cmp_pred = "composite_predicate";
    string name = skfunc->get_dag_name();
    if(name.substr(0, cmp_pred.size()) == cmp_pred)
    {
        assert(skfunc->get_dag()->getNodesByType(bool_node::UFUN).size() == 3);
    }

    for(auto it: skfunc->get_dag()->getNodesByType(bool_node::UFUN))
    {
        string var_name = ((UFUN_node*)it)->get_original_ufname();
        string sub_dag_name = ((UFUN_node*)it)->get_ufname();
        assert(skfunc->get_env()->function_map.find(sub_dag_name) != skfunc->get_env()->function_map.end());
        BooleanDagUtility* sub_dag = skfunc->get_env()->function_map[sub_dag_name];
        if(visited.find(sub_dag) == visited.end()) {
            var_name_to_inlining_subtree[var_name] = new InliningTree(sub_dag, visited);
        }
        else
        {
            var_name_to_inlining_subtree[var_name] = visited[sub_dag];
        }
    }

    if(name.substr(0, cmp_pred.size()) == cmp_pred)
    {
        assert(var_name_to_inlining_subtree.size() == 3);
    }

    visited.clear();
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
    assert(visited->find(skfunc->get_dag_name()) == visited->end());
    visited->insert(skfunc->get_dag_name());
    assert(other_visited->find(other->skfunc->get_dag_name()) == other_visited->end());
    other_visited->insert(other->skfunc->get_dag_name());
    assert(visited->size() == other_visited->size());
    cout << "UNDER " << skfunc->get_dag_name() <<" ~~ " << other->skfunc->get_dag_name() << endl;
    for(const auto& it: var_name_to_inlining_subtree)
    {
        string name = it.first;
        cout << name << " -> " << it.second->skfunc->get_dag_name() <<" ~~ " << other->var_name_to_inlining_subtree.find(it.first)->second->skfunc->get_dag_name() << endl;
        assert(other->var_name_to_inlining_subtree.find(it.first) != other->var_name_to_inlining_subtree.end());
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

void InliningTree::concretize(VarStore var_store, bool is_root) {

    if(!is_root) {
        ((SketchFunction*)skfunc)->produce_concretization(var_store, bool_node::CTRL, false, false);
    }

    for(auto it: var_name_to_inlining_subtree)
    {
        VarStore new_var_store = var_store;
        new_var_store.descend_to_subname(it.first);

        it.second->concretize(new_var_store);
    }

}
