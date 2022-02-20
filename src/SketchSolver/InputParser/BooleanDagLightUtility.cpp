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

template void TemplateInliningTree<SkFuncSetter>::
_get_solution(VarStore* running_var_store, set<const TemplateInliningTree *> *visited) const;

template void TemplateInliningTree<LightSkFuncSetter>::
_get_solution(VarStore* running_var_store, set<const TemplateInliningTree *> *visited) const;

template<typename BaseClass>
void TemplateInliningTree<BaseClass>::
    _get_solution(VarStore* running_var_store, set<const TemplateInliningTree *> *visited) const {
    bool is_root = visited->empty();
    assert(visited->find(this) == visited->end());
    visited->insert(this);
    if(BaseClass::get_var_store_used_for_concretization() == nullptr){
        AssertDebug(BaseClass::get_num_unconcretized_holes() == 0, "TRYING TO GET THE SOLUTION OF A DAG WITH SOME HOLES LEFT UNCONCRETIZED.");
    }
    else {
        append_join(*running_var_store, *BaseClass::get_var_store_used_for_concretization());
    }
    for(const auto& it: var_name_to_inlining_subtree){
        if(visited->find(it.second) == visited->end()) {
            it.second->_get_solution(running_var_store, visited);
        }
    }
    if(is_root) delete visited;
}

//template<typename BaseClass>
//const SolverLanguagePrimitives::HoleAssignment *TemplateInliningTree<BaseClass>::
//get_solution(set<const TemplateInliningTree *> *visited) const{
//    bool is_root = visited->empty();
//    assert(visited->find(this) == visited->end());
//    visited->insert(this);
//    const VarStore* root_solution = BaseClass::get_var_store_used_for_concretization();
//
//    bool root_defied = false;
//    const SolverLanguagePrimitives::HoleAssignment* ret = nullptr;
//    if(root_solution != nullptr)
//    {
//        root_defied = true;
//        ret = new SolverLanguagePrimitives::HoleAssignment(root_solution);
//        assert(ret->get_assignment()->get_inlining_tree()->get_dag_id() == get_dag_id());
//    }
//
//    for(const auto& it: var_name_to_inlining_subtree)
//    {
//        if(visited->find(it.second) == visited->end()) {
//            if (ret == nullptr) {
//                assert(!root_defied);
//                ret = it.second->get_solution();
//                const LightInliningTree* local_inlining_tree = ret->get_assignment()->get_inlining_tree();
//                LightInliningTree* ret_inlining_tree = new LightInliningTree(this, false);
//                ret_inlining_tree->insert_into_var_name_to_inlining_subtree(it.first, local_inlining_tree);
//                ret->get_assignment()->update_inlining_tree(ret_inlining_tree);
//            } else {
//                auto sub_solution = it.second->get_solution();
//                auto assignment = sub_solution->get_assignment();
//                ret->get_assignment()->disjoint_join_with(assignment);
//
//                const LightInliningTree *local_inlining_tree = assignment->get_inlining_tree();
//                assignment->set_inlining_tree_to_nullptr();
//                sub_solution->clear_assert_num_shared_ptr_is_0();
//
//                LightInliningTree *ret_inlining_tree = ret->get_assignment()->get_inlining_tree_nonconst();
//
//                if(ret_inlining_tree->get_var_name_to_inlining_subtree().find(it.first) !=
//                   ret_inlining_tree->get_var_name_to_inlining_subtree().end()) {
//                    ret_inlining_tree->erase_from_var_name_to_inlining_subtree(it.first);
//                }
//
//                ret_inlining_tree->insert_into_var_name_to_inlining_subtree(it.first, local_inlining_tree);
//            }
//        }
//    }
//
//    assert(ret->get_assignment()->get_inlining_tree()->get_dag_id() == get_dag_id());
//    assert(BaseClass::assert_nonnull());
//
//    if(is_root) delete visited;
//
//    return ret;
//}
//

template TemplateInliningTree<SkFuncSetter>::TemplateInliningTree(const BooleanDagUtility *_skfunc, map<const BooleanDagUtility*, const TemplateInliningTree*>* visited);
template TemplateInliningTree<LightSkFuncSetter>::TemplateInliningTree(const BooleanDagUtility *_skfunc, map<const BooleanDagUtility*, const TemplateInliningTree*>* visited);

template<typename BaseClass>
TemplateInliningTree<BaseClass>::TemplateInliningTree(const BooleanDagUtility *_skfunc, map<const BooleanDagUtility*, const TemplateInliningTree*>* visited): BaseClass(_skfunc) {
    bool is_root = visited->empty();
    assert(visited->find(_skfunc) == visited->end());

    (*visited)[_skfunc] = this;

    auto ufun_nodes = _skfunc->get_dag()->getNodesByType(bool_node::UFUN);
    auto skfunc_inlining_tree = _skfunc->get_inlining_tree(false);

    if(!ufun_nodes.empty()) {
        assert(skfunc_inlining_tree == nullptr);
    }

    if(skfunc_inlining_tree != nullptr) {
        assert(ufun_nodes.empty());
    }

    if(ufun_nodes.empty()) {
        if(skfunc_inlining_tree != nullptr) {
            assert(skfunc_inlining_tree->get_skfunc() == _skfunc);
            for(const auto& it: _skfunc->get_inlining_tree()->get_var_name_to_inlining_subtree()) {
                if(visited->find(it.second->get_skfunc()) == visited->end()) {
                    var_name_to_inlining_subtree[it.first] = new TemplateInliningTree(it.second->get_skfunc(), visited);
                }
                else {
                    var_name_to_inlining_subtree[it.first] = (*visited)[it.second->get_skfunc()];
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
            if (visited->find(sub_dag) == visited->end()) {
                if (var_name_to_inlining_subtree.find(var_name) != var_name_to_inlining_subtree.end()) {
                    assert(get_var_name_to_inlining_subtree().at(var_name)->get_dag_id() == sub_dag->get_dag_id());
                    assert(sub_dag != nullptr);
                } else {
                    var_name_to_inlining_subtree[var_name] = new TemplateInliningTree(sub_dag, visited);
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


InliningTree::InliningTree(const BooleanDagUtility *_skfunc):
        TemplateInliningTree<SkFuncSetter>(_skfunc) {}

void InliningTree::concretize(const VarStore * const var_store, bool is_root, set<const BooleanDagUtility*>* visited) const {
    assert(is_root == visited->empty());
    assert(visited->find(get_skfunc()) == visited->end());
    visited->insert(get_skfunc());

    bool recurse = true;
    if(!is_root) {
        if(get_skfunc()->get_has_been_concretized()) {
            recurse = false;
        }
    }

    if(recurse) {
        for (const auto& it: var_name_to_inlining_subtree) {
            if (visited->find(it.second->get_skfunc()) == visited->end()) {
                if(var_store == nullptr) {
                    ((InliningTree*)it.second)->concretize(var_store, false, visited);
                }
                else {
                    VarStore* sub_var_store = var_store->get_sub_var_store(it.first);
                    ((InliningTree*)it.second)->concretize(sub_var_store, false, visited);
                    sub_var_store->clear();
                }
            }
        }
        if(!is_root && !get_skfunc()->get_has_been_concretized()) {
            ((SketchFunction *) get_skfunc())->produce_concretization(var_store, bool_node::CTRL, false, false);
        }
    }
    if(is_root) delete visited;
}

InliningTree::InliningTree(const BooleanDagUtility *to_replace_root, const InliningTree *to_copy, map<const BooleanDagUtility *, const InliningTree *> *visited):
        TemplateInliningTree<SkFuncSetter>(to_replace_root, to_copy) {}

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
        assert(it->get_skfunc()->get_dag_name() != get_skfunc()->get_dag_name());
    }

    visited->insert(this);

    cout << tabs(ntabs) << get_skfunc()->get_dag_name() << " { " << endl;
    for(const auto& it: var_name_to_inlining_subtree){
        cout << tabs(ntabs+1)<< it.first << " : " << endl;
        if(visited->find((const InliningTree*)it.second) == visited->end()) {
            ((const InliningTree*)it.second)->print(ntabs + 2, visited);
        }
        else
        {
            cout << tabs(ntabs+2) << "*" << it.second->get_skfunc()->get_dag_name() << endl;
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


    for (auto it:get_skfunc()->get_dag()->getNodesByType(bool_node::CTRL)) {
        string original_name = ((CTRL_node *) it)->get_original_name();
        if(original_name != "#PC") {
            AssertDebug(var_store.has_original_name(original_name), "NODE.original_name(): " + original_name + " DOESN'T EXIST.");
        }
    }

    for (auto it: get_skfunc()->get_dag()->getNodesByType(bool_node::CTRL)) {
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
        if(visited->find((InliningTree*)it.second) == visited->end()) {
            ((InliningTree*)it.second)->rename_var_store(var_store, visited, root);
        }
    }

    if(is_root) {
        var_store.set_inlining_tree(this);
    }

    if(is_root) delete visited;
}



template bool TemplateInliningTree<SkFuncSetter>::has_no_holes(set<string>* hole_names, set<const TemplateInliningTree*>* visited) const;
template bool TemplateInliningTree<LightSkFuncSetter>::has_no_holes(set<string>* hole_names, set<const TemplateInliningTree*>* visited) const;

template<typename BaseClass>
bool TemplateInliningTree<BaseClass>::has_no_holes(set<string>* hole_names, set<const TemplateInliningTree*>* visited) const {
    bool is_root = visited->empty();
    assert(visited->find(this) == visited->end());
    visited->insert(this);


//    for(auto it:get_skfunc()->get_dag()->getNodesByType(bool_node::CTRL))
//    {
//        hole_names->insert(((CTRL_node*)it)->get_name());
//    }


    bool ret = true;

    if(BaseClass::get_num_unconcretized_holes() >= 1)
    {
        ret = false;
    }
    else {
        for (const auto &it: var_name_to_inlining_subtree) {
            assert(ret);
            if (visited->find(it.second) == visited->end()) {
                if (!(((InliningTree *) it.second)->has_no_holes(hole_names, visited))) {
                    ret = false;
                    break;
                }
            }
        }
    }

//    bool ret = false;
//    if(hole_names->empty()) {
//        ret = true;
//    }
//    else {
//        if (hole_names->size() == 1) {
//            if (*hole_names->begin() == "#PC") {
//                ret = true;
//            }
//        }
//    }

    if(is_root)
    {
        delete visited;
        delete hole_names;
    }

    return ret;
}


long long SkFuncSetter::inlining_tree_global_id = 0;
set<const SkFuncSetter*> SkFuncSetter::all_inlining_trees = set<const SkFuncSetter*>();
map<string, int> SkFuncSetter::name_to_count = map<string, int>();
int SkFuncSetter::max_count = 0;


SkFuncSetter::SkFuncSetter(const BooleanDagUtility *_skfunc): LightSkFuncSetter(_skfunc), skfunc(_skfunc), inlining_tree_id(inlining_tree_global_id++) {
    init();
}

void SkFuncSetter::soft_clear(bool clear_dag, bool sub_clear) const {
    assert(get_num_shared_ptr() == 0);
    assert(get_skfunc() != nullptr);
    if(clear_dag)
    {
        if(sub_clear) {
            ((SketchFunction *) skfunc)->_clear();
        }
        else {
            ((SketchFunction *) skfunc)->clear();
        }
    }
    assert(name_to_count.find(get_dag_name()) != name_to_count.end());
    name_to_count[get_dag_name()]--;
    assert(all_inlining_trees.find(this) != all_inlining_trees.end());
    all_inlining_trees.erase(this);
//    cout << "AFTER CLEAR " << endl;
//    cout << "#trees " << all_inlining_trees.size() << ", dag_name: " << get_dag_name() << ", count: "<< name_to_count[get_skfunc()->get_dag_name()] << endl;


    LightSkFuncSetter::soft_clear(clear_dag, sub_clear);
}

long long int SkFuncSetter::get_tree_id() const {
    return inlining_tree_id;
}

void SkFuncSetter::init() {
    skfunc->increment_shared_ptr();
    assert(all_inlining_trees.find(this) == all_inlining_trees.end());
    all_inlining_trees.insert(this);
    if(name_to_count.find(get_dag_name()) == name_to_count.end()) {
        name_to_count[get_dag_name()] = 0;
    }
    name_to_count[get_dag_name()]+=1;
    max_count = max(max_count, name_to_count[get_dag_name()]);
//    cout << "#trees " << all_inlining_trees.size() <<", tree_id: " << inlining_tree_global_id << ", dag_name: " << get_dag_name() << ", count: "<< name_to_count[get_dag_name()] << ", skfunc.num_shared_ptr = " << skfunc->get_num_shared_ptr() << endl;
}

SkFuncSetter::SkFuncSetter(const SkFuncSetter *to_copy): LightSkFuncSetter(to_copy), skfunc(to_copy->skfunc), inlining_tree_id(inlining_tree_global_id++) {
    init();
}
LightSkFuncSetter::LightSkFuncSetter(const BooleanDagUtility *_skfunc): dag_name(_skfunc->get_dag_name()), dag_id(_skfunc->get_dag_id()), var_store(
        _skfunc->get_var_store_used_for_concretization()){
    num_unconcretized_holes = 0;
    for(auto it: _skfunc->get_dag()->getNodesByType(bool_node::CTRL)) {
        if(it->get_name() != "#PC") {
            num_unconcretized_holes += 1;
        }
    }
}