//
// Created by kliment on 2/8/22.
//

#include "VarStore.h"
#include "SketchFunction.h"
#include "BooleanDagUtility.h"

bool VarStore::has_original_name_and_source_dag(const string &original_name, const string &source_dag) const {
    bool ret = has_original_name(original_name);
    if(ret) {
        ret &= var_name_to_dag_name_to_name.at(original_name).find(source_dag) != var_name_to_dag_name_to_name.at(original_name).end();
    }
    return ret;
}


bool VarStore::has_original_name(const string &original_name) const {
    bool ret = var_name_to_dag_name_to_name.find(original_name) != var_name_to_dag_name_to_name.end();
    if(ret)
    {
        ret &= !var_name_to_dag_name_to_name.at(original_name).empty();
    }
    return ret;
}

void VarStore::rename(const string &original_name, const string &new_source_dag, const string &new_name, InliningTree* new_inlining_tree) {
    assert(var_name_to_dag_name_to_name.find(original_name) != var_name_to_dag_name_to_name.end());
    if(contains(new_name)){
        cout << "contains " << new_name << endl;
        //if new name already exists make sure that it's the same as the original name

        auto& obj = getObjConst(new_name);
        assert(obj.name == new_name);
        assert(obj.get_original_name() == original_name);
        assert(var_name_to_dag_name_to_name.find(original_name) != var_name_to_dag_name_to_name.end());
        if(obj.get_source_dag_name() == new_source_dag) {
            assert(var_name_to_dag_name_to_name[original_name].find(new_source_dag) !=
                   var_name_to_dag_name_to_name[original_name].end());
            assert(var_name_to_dag_name_to_name[original_name][new_source_dag] == new_name);
        }
        else {
            AssertDebug(false, "HOW CAN THE NAME BE CONTAINED BUT THE DAG BE DIFFERENT. EVERY DAG HAS UNIQUE HOLE NAMES.")
            string prev_dag = obj.get_source_dag_name();
            assert(var_name_to_dag_name_to_name[original_name].find(prev_dag) !=
                   var_name_to_dag_name_to_name[original_name].end());
            assert(var_name_to_dag_name_to_name[original_name][prev_dag] == new_name);
            var_name_to_dag_name_to_name[original_name].erase(prev_dag);
            if(var_name_to_dag_name_to_name[original_name].empty()) {
                var_name_to_dag_name_to_name.erase(original_name);
            }

            if(var_name_to_dag_name_to_name.find(original_name) == var_name_to_dag_name_to_name.end()) {
                var_name_to_dag_name_to_name[original_name] = map<string, string>();
            }
            assert(var_name_to_dag_name_to_name[original_name].find(new_source_dag) == var_name_to_dag_name_to_name[original_name].end());
            var_name_to_dag_name_to_name[original_name][new_source_dag] = new_name;
        }
    }
    else
    {
        cout << "not_contains " << new_name << endl;
        AssertDebug(var_name_to_dag_name_to_name[original_name].size() == 1, "Check why this fails and respond accordingly.");
        assert(inlining_tree != nullptr);
        vector<string>* prev_path = inlining_tree->find(var_name_to_dag_name_to_name[original_name].begin()->first);
        vector<string>* new_path = new_inlining_tree->find(new_source_dag);
        AssertDebug(*prev_path == *new_path, "You are replacing a hole name with another hole name that doesn't match the topological location between the prev and new inlining tree");
        string obj_name = var_name_to_dag_name_to_name[original_name].begin()->second;
        assert(index.find(obj_name) != index.end());
        auto& obj = _getObj(obj_name);
        assert(obj.get_original_name() == original_name);
        assert(obj_name == obj.name);
        string prev_source_dag_name = obj.get_source_dag_name();
        assert(var_name_to_dag_name_to_name[original_name].find(prev_source_dag_name) != var_name_to_dag_name_to_name[original_name].end());
        obj.rename(new_name, new_source_dag);

        assert(index.find(new_name) == index.end());
        int prev_index = index[obj_name];
        index.erase(obj_name);
        index[new_name] = prev_index;
        var_name_to_dag_name_to_name[original_name].erase(prev_source_dag_name);
        var_name_to_dag_name_to_name[original_name][new_source_dag] = new_name;
    }
}

const string &VarStore::get_name(const string& var_name, const string &source_dag_name) {
    assert(var_name_to_dag_name_to_name.find(var_name) != var_name_to_dag_name_to_name.end());
    assert(var_name_to_dag_name_to_name[var_name].find(source_dag_name) != var_name_to_dag_name_to_name[var_name].end());
    return var_name_to_dag_name_to_name[var_name][source_dag_name];
}

#include "BooleanDagUtility.h"

VarStore::VarStore(InliningTree *_inlining_tree){
    if(_inlining_tree != nullptr) {
        inlining_tree = (new InliningTree(_inlining_tree));
    }
}

VarStore::VarStore(const VarStore &to_copy)
{
    Assert(to_copy.synths.empty(), "TODO: implement copy logic for synths and synthouths.");
    Assert(to_copy.synthouts.empty(), "TODO: implement copy logic for synths and synthouths.");

    if(to_copy.inlining_tree != nullptr)
        inlining_tree = new InliningTree(to_copy.inlining_tree);

    bitsize = to_copy.bitsize;

    vector<pair<int, string> > index_as_vec;

    for(const auto& it : to_copy.index) {
        index_as_vec.emplace_back(it.second, it.first);
    }

    sort(index_as_vec.begin(), index_as_vec.end());


    for(int i = 0;i<index_as_vec.size(); i++)
    {
        pair<int, string> it = index_as_vec[i];
        assert(it.first == i);
        insertObj(it.second, it.first, objP(to_copy.objs[it.first]));
    }
}

void VarStore::rename(BooleanDagUtility *new_dag_util) {

    assert(inlining_tree != nullptr);

    InliningTree* new_inlining_tree = new_dag_util->get_inlining_tree();

    assert(new_inlining_tree->match_topology(inlining_tree));

    BooleanDAG* tmp_dag = new_dag_util->get_dag();

    for (auto it: tmp_dag->getNodesByType(bool_node::CTRL)) {
        string new_name =  ((CTRL_node*)it)->get_name();
        string original_name = ((CTRL_node*)it)->get_original_name();
        string subdag_name = ((CTRL_node*)it)->get_source_dag_name();
        cout << "RENAME " << original_name <<" -> " << new_name << " OF DAG " << ((CTRL_node*)it)->get_source_dag_name() << endl;
        rename(original_name, subdag_name, new_name, new_inlining_tree);
    }

    inlining_tree = new InliningTree(new_inlining_tree);

}

VarStore *VarStore::get_sub_var_store(const string& under_this_var) {
    VarStore* ret = new VarStore(*this);
    ret->descend_to_subname(under_this_var);
    return ret;
}

void VarStore::descend_to_subname(const string &under_this_name) {
    inlining_tree = inlining_tree->get_sub_inlining_tree(under_this_name);
}


