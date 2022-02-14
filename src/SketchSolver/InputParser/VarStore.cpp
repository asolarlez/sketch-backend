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

void VarStore::rename(const string &original_name, const string &new_source_dag, const string &new_name, const InliningTree *new_inlining_tree) {
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

        string matching_subdag_name;

        AssertDebug(!var_name_to_dag_name_to_name[original_name].empty(), "checkrep should have failed.");
        vector<string>* new_path = new_inlining_tree->find(new_source_dag);
        vector<string>* prev_path = nullptr;
        bool enter = false;

        for(const auto& it: var_name_to_dag_name_to_name[original_name])
        {
             prev_path = inlining_tree->find(it.first);
             if(prev_path != nullptr && *prev_path == *new_path)
             {
                 enter = true;
                 matching_subdag_name = it.first;
                 break;
             }
        }

//        AssertDebug(var_name_to_dag_name_to_name[original_name].size() == 1, "Check why this fails and respond accordingly.");
        assert(inlining_tree != nullptr);
//        vector<string>* prev_path = inlining_tree->find(var_name_to_dag_name_to_name[original_name].begin()->first);
//        vector<string>* new_path = new_inlining_tree->find(new_source_dag);
        if(!enter)
        {
            assert(prev_path == nullptr || *prev_path != *new_path);
            cout << "HERE" << endl;
            cout << "VARSTORE INLINING TREE" << endl;
            inlining_tree->print();
            cout << "NEW INLINING TREE" << endl;
            new_inlining_tree->print();
            cout << endl;

            InliningTree* subtree = inlining_tree;
            for(int i = new_path->size()-1; i>=0;i--) {
                subtree = subtree->get_sub_inlining_tree((*new_path)[i]);
            }

            string subdag_of_interest = subtree->get_skfunc()->get_dag_name();

            assert(var_name_to_dag_name_to_name[original_name].find(subdag_of_interest) == var_name_to_dag_name_to_name[original_name].end());

            VarStore* sub_var_store = ((SketchFunction*)subtree->get_skfunc())->get_solution()->to_var_store();
            auto sub_var_name_to_dag_name_to_name = sub_var_store->var_name_to_dag_name_to_name;
            auto sub_index = sub_var_store->index;
            if(sub_var_name_to_dag_name_to_name.find(original_name) != sub_var_name_to_dag_name_to_name.end())
            {
                assert(sub_var_name_to_dag_name_to_name[original_name].size() == 1);
                if(sub_var_name_to_dag_name_to_name[original_name].find(subdag_of_interest) != sub_var_name_to_dag_name_to_name[original_name].end())
                {
                    auto obj_name = sub_var_name_to_dag_name_to_name[original_name][subdag_of_interest];

                    assert(sub_index.find(obj_name) != sub_index.end());
                    auto &obj = sub_var_store->_getObj(obj_name);
                    assert(obj.get_original_name() == original_name);
                    assert(obj_name == obj.name);
                    string prev_source_dag_name = obj.get_source_dag_name();
                    assert(prev_source_dag_name == sub_var_name_to_dag_name_to_name[original_name].begin()->first);
                    assert(sub_var_name_to_dag_name_to_name[original_name].find(prev_source_dag_name) !=
                                   sub_var_name_to_dag_name_to_name[original_name].end());
//                    obj.rename(new_name, new_source_dag);

                    assert(sub_index.find(new_name) == sub_index.end());
//                    int prev_index = sub_index[obj_name];
                    assert(index.find(obj_name) == index.end());
//                    cout << "ERASING " << obj_name << endl;
//                    assert(false);
//                    index.erase(obj_name);
//                    index[new_name] = prev_index;
                    assert(var_name_to_dag_name_to_name.find(original_name) != var_name_to_dag_name_to_name.end());
                    assert(var_name_to_dag_name_to_name[original_name].find(prev_source_dag_name) == var_name_to_dag_name_to_name[original_name].end());
//                    var_name_to_dag_name_to_name[original_name].erase(prev_source_dag_name);
                    assert(var_name_to_dag_name_to_name[original_name].find(new_source_dag) == var_name_to_dag_name_to_name[original_name].end());

                    auto new_obj = objP(obj);
                    new_obj.rename(new_name, new_source_dag);
                    insertObj(new_name, objs.size(), new_obj);

                    assert(var_name_to_dag_name_to_name[original_name].find(new_source_dag) != var_name_to_dag_name_to_name[original_name].end());
                    assert(var_name_to_dag_name_to_name[original_name][new_source_dag] == new_name);
                }
                else {
                    assert(false);
                }
            }
            else
            {
                assert(false);
            }
        }
        else {
            AssertDebug(*prev_path == *new_path,
                        "You are replacing a hole name with another hole name that doesn't match the topological location between the prev and new inlining tree");
//            string obj_name = var_name_to_dag_name_to_name[original_name].begin()->second;

            assert(var_name_to_dag_name_to_name[original_name].find(matching_subdag_name) != var_name_to_dag_name_to_name[original_name].end());
            string obj_name = var_name_to_dag_name_to_name[original_name][matching_subdag_name];

            assert(index.find(obj_name) != index.end());
            auto &obj = _getObj(obj_name);
            assert(obj.get_original_name() == original_name);
            assert(obj_name == obj.name);
            string prev_source_dag_name = obj.get_source_dag_name();
            assert(prev_source_dag_name == matching_subdag_name);
            assert(var_name_to_dag_name_to_name[original_name].find(prev_source_dag_name) !=
                   var_name_to_dag_name_to_name[original_name].end());
            obj.rename(new_name, new_source_dag);

            assert(index.find(new_name) == index.end());
            int prev_index = index[obj_name];
            cout << "ERASING " << obj_name << endl;
            index.erase(obj_name);
            index[new_name] = prev_index;
            assert(objs[index[new_name]].name == new_name);
            var_name_to_dag_name_to_name[original_name].erase(prev_source_dag_name);
            var_name_to_dag_name_to_name[original_name][new_source_dag] = new_name;
        }
    }
}

const string &VarStore::get_name(const string& var_name, const string &source_dag_name) {
    assert(var_name_to_dag_name_to_name.find(var_name) != var_name_to_dag_name_to_name.end());
    assert(var_name_to_dag_name_to_name[var_name].find(source_dag_name) != var_name_to_dag_name_to_name[var_name].end());
    return var_name_to_dag_name_to_name[var_name][source_dag_name];
}

VarStore::VarStore(InliningTree *_inlining_tree){
    if(_inlining_tree != nullptr) {
        inlining_tree = (new InliningTree(_inlining_tree));
    }
}

VarStore::VarStore(const VarStore &to_copy, InliningTree* _inlining_tree)
{
    AssertDebug(to_copy.synths.empty(), "TODO: implement copy logic for synths and synthouths.");
    AssertDebug(to_copy.synthouts.empty(), "TODO: implement copy logic for synths and synthouths.");

    if(_inlining_tree != nullptr) {
        inlining_tree = new InliningTree(_inlining_tree);
    }

    *this = to_copy;

}

VarStore *VarStore::get_sub_var_store(const string& under_this_var) const {
    if(inlining_tree != nullptr) {
        return new VarStore(*this, inlining_tree->get_sub_inlining_tree(under_this_var));
    }
    else
    {
        assert(size() == 0);
        return new VarStore(*this);
    }
}

bool VarStore::check_rep() const {
    assert(inlining_tree != nullptr);
    for(const auto& it: index) {
        auto obj = getObjConst(it.first);
        string original_name = obj.get_original_name();
        string dag_name = obj.get_source_dag_name();
        assert(var_name_to_dag_name_to_name.find(original_name) != var_name_to_dag_name_to_name.end());
        assert(var_name_to_dag_name_to_name.at(original_name).find(dag_name) != var_name_to_dag_name_to_name.at(original_name).end());
        assert(var_name_to_dag_name_to_name.at(original_name).at(dag_name) == it.first);

        assert(inlining_tree->find(dag_name) != nullptr);

    }
    return true;
}

void VarStore::set_inlining_tree(const InliningTree *new_inlining_tree) {
    assert(inlining_tree != nullptr);
    inlining_tree->clear();
    inlining_tree = new InliningTree(new_inlining_tree);
}

void VarStore::operator=(const VarStore &to_copy){

    AssertDebug(to_copy.synths.empty(), "TODO: implement copy logic for synths and synthouths.");
    AssertDebug(to_copy.synthouts.empty(), "TODO: implement copy logic for synths and synthouths.");

    //local clear.
    objs.clear();
    index.clear();
    var_name_to_dag_name_to_name.clear();
    bitsize = 0;

    if(inlining_tree == nullptr) {
        if (to_copy.inlining_tree != nullptr) {
            inlining_tree = new InliningTree(to_copy.inlining_tree);
        }
    }


    bitsize = to_copy.bitsize;

    vector<pair<int, string> > index_as_vec;

    for(const auto& it : to_copy.index) {
        index_as_vec.emplace_back(it.second, it.first);
    }

    sort(index_as_vec.begin(), index_as_vec.end());

    int true_idx = 0;

    for(int i = 0;i<index_as_vec.size(); i++)
    {
        pair<int, string> it = index_as_vec[i];
        assert(it.first == i);
        if(inlining_tree == nullptr || inlining_tree->find(to_copy.objs[it.first].get_source_dag_name()) != nullptr) {
            insertObj(it.second, true_idx, objP(to_copy.objs[it.first]));
            true_idx++;
        }
    }
}