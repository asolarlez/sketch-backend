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

void VarStore::rename(const objP& _obj, const string &new_name, const string& new_source_dag)
{
    assert(inlining_tree == nullptr);

    const string& original_name = _obj.get_original_name();
    const string& _prev_source_dag_name = _obj.get_source_dag_name();
    assert(var_name_to_dag_name_to_name[original_name].find(_prev_source_dag_name) != var_name_to_dag_name_to_name[original_name].end());
    string obj_name = var_name_to_dag_name_to_name[original_name][_prev_source_dag_name];

    assert(index.find(obj_name) != index.end());
    auto &obj = _getObj(obj_name);
    assert(obj.get_original_name() == original_name);
    assert(obj_name == obj.name);
    string prev_source_dag_name = obj.get_source_dag_name();
    assert(prev_source_dag_name == _prev_source_dag_name);
    assert(var_name_to_dag_name_to_name[original_name].find(prev_source_dag_name) !=
           var_name_to_dag_name_to_name[original_name].end());

    obj.rename(new_name, new_source_dag);

    assert(index.find(new_name) == index.end());
    int prev_index = index[obj_name];
    index.erase(obj_name);
    index[new_name] = prev_index;
    assert(objs[index[new_name]].name == new_name);
    var_name_to_dag_name_to_name[original_name].erase(prev_source_dag_name);
    var_name_to_dag_name_to_name[original_name][new_source_dag] = new_name;

}

void VarStore::rename(const string &original_name, const string &new_source_dag, const string &new_name, const LightInliningTree *new_inlining_tree, string& _ret_prev_source_dag_name) {
    assert(_ret_prev_source_dag_name == "");
    assert(inlining_tree != nullptr);
    assert(var_name_to_dag_name_to_name.find(original_name) != var_name_to_dag_name_to_name.end());

    if(contains(new_name)){
        const objP& obj = getObjConst(new_name);
        assert(obj.name == new_name);
        assert(obj.get_original_name() == original_name);
        assert(var_name_to_dag_name_to_name.find(original_name) != var_name_to_dag_name_to_name.end());
        if(obj.get_source_dag_name() == new_source_dag) {
            assert(var_name_to_dag_name_to_name[original_name].find(new_source_dag) !=
                   var_name_to_dag_name_to_name[original_name].end());
            assert(var_name_to_dag_name_to_name[original_name][new_source_dag] == new_name);
            assert(inlining_tree->contains_var(obj));
        }
        else {
            AssertDebug(false, "HOW CAN THE NAME BE CONTAINED BUT THE DAG BE DIFFERENT. EVERY DAG HAS UNIQUE HOLE NAMES.")
            {//dead code
//                string prev_dag = obj.get_source_dag_name();
//                assert(var_name_to_dag_name_to_name[original_name].find(prev_dag) !=
//                       var_name_to_dag_name_to_name[original_name].end());
//                assert(var_name_to_dag_name_to_name[original_name][prev_dag] == new_name);
//                var_name_to_dag_name_to_name[original_name].erase(prev_dag);
//                if (var_name_to_dag_name_to_name[original_name].empty()) {
//                    var_name_to_dag_name_to_name.erase(original_name);
//                }
//
//                if (var_name_to_dag_name_to_name.find(original_name) == var_name_to_dag_name_to_name.end()) {
//                    var_name_to_dag_name_to_name[original_name] = map<string, string>();
//                }
//                assert(var_name_to_dag_name_to_name[original_name].find(new_source_dag) ==
//                       var_name_to_dag_name_to_name[original_name].end());
//                var_name_to_dag_name_to_name[original_name][new_source_dag] = new_name;
            }
        }
    }
    else
    {
        string matching_subdag_name;

        AssertDebug(!var_name_to_dag_name_to_name[original_name].empty(), "checkrep should have failed.");
        const vector<string>* new_path = new_inlining_tree->find(new_source_dag);
        const vector<string>* prev_path = nullptr;
        bool enter = false;

//        cout << "var_val's inlining_tree" << endl;
//        inlining_tree->print();
//
//        cout << "original_name:" << original_name << endl;
//        for(const auto& it: var_name_to_dag_name_to_name[original_name])
//        {
//            cout << "it.first " << it.first << " -> " << it.second << endl;
//        }

//        cout << "paths tried:" << endl;
        for(const auto& it: var_name_to_dag_name_to_name[original_name]){
            assert(enter == (prev_path != nullptr));
            auto tmp_path = inlining_tree->find(it.first);
//            if(tmp_path != nullptr) {
//                cout << "tmp_path.size() " << tmp_path->size() << endl;
//                cout << "BOTTOM <- ";
//                for (const auto &link: *tmp_path) {
//                    cout << link << " <- ";
//                }
//                cout << "TOP" << endl;
//                cout << endl;
//            }
            if(tmp_path != nullptr && *tmp_path == *new_path) {
                AssertDebug(prev_path == nullptr && !enter, "MULTIPLE DAGS HAVING THE SAME ORIGINAL HOLE NAME! MOST PROBABLY THE DAGS WERE CLONES OF THE SAME ORIGINAL DAG. MULTIPLE WAYS TO GET TO THE SAME DAG. THINK ABOUT WHAT THIS CASE MEANS AND WHAT ADDITIONAL ASSERTS HAVE TO BE ADDED. CONNECTED WITH A SIMILAR ASSERT IN LightInliningTree._find.");
                enter = true;
                prev_path = tmp_path;
                matching_subdag_name = it.first;
                //break;
            }
            else if (tmp_path != nullptr) {
                delete tmp_path;
            }
        }

        assert(inlining_tree != nullptr);
        inlining_tree->check_rep(this);
        if(!enter)
        {
            AssertDebug(false, "HOLE IS MISSING; BEST IF YOU MAKE SURE THAT THIS DOESN'T HAPPEN");
            #define DONT_ALLOW_RENAMING_MISSING_HOLES
            #ifndef DONT_ALLOW_RENAMING_MISSING_HOLES
            {
                assert(prev_path == nullptr || *prev_path != *new_path);
                const LightInliningTree *subtree = inlining_tree;
                for (int i = new_path->size() - 1; i >= 0; i--) {
                    subtree = subtree->get_sub_inlining_tree((*new_path)[i]);
                }

                string subdag_of_interest = subtree->get_dag_name();

                assert(var_name_to_dag_name_to_name[original_name].find(subdag_of_interest) ==
                       var_name_to_dag_name_to_name[original_name].end());

                VarStore *sub_var_store = ((SketchFunction *) subtree->get_skfunc())->get_solution_var_store();
                auto sub_var_name_to_dag_name_to_name = sub_var_store->var_name_to_dag_name_to_name;
                auto sub_index = sub_var_store->index;
                if (sub_var_name_to_dag_name_to_name.find(original_name) != sub_var_name_to_dag_name_to_name.end()) {
                    assert(sub_var_name_to_dag_name_to_name[original_name].size() == 1);
                    if (sub_var_name_to_dag_name_to_name[original_name].find(subdag_of_interest) !=
                        sub_var_name_to_dag_name_to_name[original_name].end()) {
                        auto obj_name = sub_var_name_to_dag_name_to_name[original_name][subdag_of_interest];

                        assert(sub_index.find(obj_name) != sub_index.end());
                        auto &obj = sub_var_store->getObjConst(obj_name);
                        assert(obj.get_original_name() == original_name);
                        assert(obj_name == obj.name);
                        string prev_source_dag_name = obj.get_source_dag_name();
                        assert(prev_source_dag_name == sub_var_name_to_dag_name_to_name[original_name].begin()->first);
                        assert(sub_var_name_to_dag_name_to_name[original_name].find(prev_source_dag_name) !=
                               sub_var_name_to_dag_name_to_name[original_name].end());
                        assert(sub_index.find(new_name) == sub_index.end());
                        assert(index.find(obj_name) == index.end());
                        assert(var_name_to_dag_name_to_name.find(original_name) != var_name_to_dag_name_to_name.end());
                        assert(var_name_to_dag_name_to_name[original_name].find(prev_source_dag_name) ==
                               var_name_to_dag_name_to_name[original_name].end());
                        assert(var_name_to_dag_name_to_name[original_name].find(new_source_dag) ==
                               var_name_to_dag_name_to_name[original_name].end());

                        auto new_obj = objP(obj);
                        new_obj.rename(new_name, new_source_dag);
                        insertObj(new_name, objs.size(), new_obj);

                        assert(var_name_to_dag_name_to_name[original_name].find(new_source_dag) !=
                               var_name_to_dag_name_to_name[original_name].end());
                        assert(var_name_to_dag_name_to_name[original_name][new_source_dag] == new_name);
                    } else {
                        assert(false);
                    }
                } else {
                    assert(false);
                }
                sub_var_store->clear();
            }
            #endif
        }
        else {
            AssertDebug(*prev_path == *new_path,
                        "You are replacing a hole name with another hole name that doesn't match the topological location between the prev and new inlining tree");

            assert(var_name_to_dag_name_to_name[original_name].find(matching_subdag_name) != var_name_to_dag_name_to_name[original_name].end());
            string obj_name = var_name_to_dag_name_to_name[original_name][matching_subdag_name];

            assert(index.find(obj_name) != index.end());
            objP& obj = _getObj(obj_name);

//            rename(obj, new_name, new_source_dag);

            assert(obj.get_original_name() == original_name);
            assert(obj_name == obj.name);
            string prev_source_dag_name = obj.get_source_dag_name();
            assert(prev_source_dag_name == matching_subdag_name);
            assert(var_name_to_dag_name_to_name[original_name].find(prev_source_dag_name) !=
                   var_name_to_dag_name_to_name[original_name].end());

            _ret_prev_source_dag_name = prev_source_dag_name;

            {//renaming unit

                inlining_tree->rename_var(obj, new_name, new_source_dag);
                obj.rename(new_name, new_source_dag);

                assert(index.find(new_name) == index.end());
                int prev_index = index[obj_name];
                index.erase(obj_name);
                index[new_name] = prev_index;
                assert(objs[index[new_name]].name == new_name);
                var_name_to_dag_name_to_name[original_name].erase(prev_source_dag_name);
                var_name_to_dag_name_to_name[original_name][new_source_dag] = new_name;

            }

//            check_rep();
            //insert in inline
        }

        delete new_path;
        if(prev_path != nullptr) {
            delete prev_path;
        }
    }
}

const string &VarStore::get_name(const string& var_name, const string &source_dag_name) {
    assert(var_name_to_dag_name_to_name.find(var_name) != var_name_to_dag_name_to_name.end());
    assert(var_name_to_dag_name_to_name[var_name].find(source_dag_name) != var_name_to_dag_name_to_name[var_name].end());
    return var_name_to_dag_name_to_name[var_name][source_dag_name];
}

#ifndef NO_CLONE_INLINING_TREE
VarStore::VarStore(const LightInliningTree *_inlining_tree){
    if(_inlining_tree != nullptr) {
        inlining_tree = new LightInliningTree(_inlining_tree);
    }
}
#else
VarStore::VarStore(const LightInliningTree *_inlining_tree){
    assert(_inlining_tree == nullptr);
}
#endif


//VarStore::VarStore(const LightInliningTree *_inlining_tree){
//    if(_inlining_tree != nullptr) {
//#ifndef NO_CLONE_INLINING_TREE
//        inlining_tree = new LightInliningTree(_inlining_tree);
//#else
//        inlining_tree = _inlining_tree;
//#endif
//    }
//}

#ifndef NO_CLONE_INLINING_TREE
VarStore::VarStore(const VarStore &to_copy, const LightInliningTree* _inlining_tree)
{
    AssertDebug(to_copy.synths.empty(), "TODO: implement copy logic for synths and synthouths.");
    AssertDebug(to_copy.synthouts.empty(), "TODO: implement copy logic for synths and synthouths.");

    if(_inlining_tree != nullptr) {
        inlining_tree = new LightInliningTree(_inlining_tree);
    }

    *this = to_copy;
}
#else
VarStore::VarStore(const VarStore &to_copy, const LightInliningTree* _inlining_tree)
{
    assert(_inlining_tree == nullptr);
    AssertDebug(to_copy.synths.empty(), "TODO: implement copy logic for synths and synthouths.");
    AssertDebug(to_copy.synthouts.empty(), "TODO: implement copy logic for synths and synthouths.");

//    if(_inlining_tree != nullptr) {
//        inlining_tree = new LightInliningTree(_inlining_tree);
//    }

    *this = to_copy;
}
#endif

//VarStore::VarStore(const VarStore &to_copy, const LightInliningTree* _inlining_tree)
//{
//    AssertDebug(to_copy.synths.empty(), "TODO: implement copy logic for synths and synthouths.");
//    AssertDebug(to_copy.synthouts.empty(), "TODO: implement copy logic for synths and synthouths.");
//
//    if(_inlining_tree != nullptr) {
//#ifndef NO_CLONE_INLINING_TREE
//        inlining_tree = new LightInliningTree(_inlining_tree);
//#else
//        inlining_tree = _inlining_tree;
//#endif
//    }
//
//    *this = to_copy;
//
//}

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

        assert(inlining_tree->contains(dag_name));
        assert(inlining_tree->contains_var(obj.name, obj.element_size(), obj.otype, obj.get_type(), obj.get_original_name(), obj.get_source_dag_name()));

    }
    inlining_tree->check_rep(this);
    return true;
}

#ifndef NO_CLONE_INLINING_TREE
void VarStore::set_inlining_tree(const LightInliningTree *new_inlining_tree) {
    if(inlining_tree != nullptr) {
        inlining_tree->clear();
    }
    inlining_tree = new LightInliningTree(new_inlining_tree);
}
#else
void VarStore::set_inlining_tree(const LightInliningTree *new_inlining_tree) {
    assert(new_inlining_tree == nullptr);
}
#endif

//void VarStore::set_inlining_tree(const LightInliningTree *new_inlining_tree) {
//    if(inlining_tree != nullptr) {
//        inlining_tree->clear();
//    }
//#ifndef NO_CLONE_INLINING_TREE
//    inlining_tree = new LightInliningTree(new_inlining_tree);
//#else
//    inlining_tree = new_inlining_tree;
//#endif
//}

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

#ifndef NO_CLONE_INLINING_TREE
            inlining_tree = new LightInliningTree(to_copy.inlining_tree);
#else
            inlining_tree = to_copy.inlining_tree;
#endif

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
        if(inlining_tree == nullptr || inlining_tree->contains(to_copy.objs[it.first].get_source_dag_name())) {
            insertObj(it.second, true_idx, objP(to_copy.objs[it.first]));
            true_idx++;
        }
    }
}

void VarStore::clear()
{
//		for(auto it:objs)
//		{
//            it.clear();
//		}

    for(auto it:var_name_to_dag_name_to_name)
    {
        it.second.clear();
    }

    var_name_to_dag_name_to_name.clear();


    if(inlining_tree != nullptr) {
        inlining_tree->clear();
    }

    objs.clear();
    index.clear();

    Assert(synths.size() == 0, "TODO: implement copy logic for synths and synthouths.");
    Assert(synthouts.size() == 0, "TODO: implement copy logic for synths and synthouths.");

    delete this;
}

bool VarStore::check_rep_and_clear() {
    bool ret = check_rep();
    clear();
    return ret;
}

void VarStore::newVar(const string &name, int nbits, OutType *otype, bool_node::Type type, string original_name,
                      string source_dag_name) {
    if(contains(name)) {
        auto obj = getObjConst(name);
        assert(obj.getName() == name);
        assert(obj.get_size() == nbits && obj.element_size() == nbits);
        assert(obj.getOtype() == otype);
        assert(obj.get_original_name() == original_name);
        assert(obj.get_source_dag_name() == source_dag_name);
        if(inlining_tree != nullptr)
        assert(inlining_tree->contains_var(name, nbits, otype, type, original_name, source_dag_name));
    }
    else {
        Assert(index.count(name) == 0, name << ": This variable already existed!!");
        insert_name_in_original_name_to_dag_name_to_name(name, original_name, source_dag_name);
        int begidx = objs.size();
        objs.emplace_back(objP(name, nbits, otype, type, original_name, source_dag_name));
        index[name] = begidx;
        bitsize += nbits;
        if(inlining_tree != nullptr)
        inlining_tree->insert_var(objs[objs.size()-1]);
    }
    assert(!objs[index[name]].get_is_array());
}

void VarStore::setVarVal(const string &name, int val, OutType *otype, bool_node::Type type) {
    AssertDebug(contains(name), "IF THIS FAILS, REWRITE THIS FUNCTION TO USE newVar first.");
    if(inlining_tree != nullptr) {
        auto obj = getObjConst(name);
        assert(!obj.get_is_array());
        assert(inlining_tree->contains_var(name, obj.element_size(), obj.otype, obj.get_type(), obj.get_original_name(), obj.get_source_dag_name()));
    }
    int idx;
    if(index.count(name)!=0){
        idx = getId(name);
        auto obj = getObjConst(name);
        assert(obj.otype == otype);
        assert(obj.get_type() == type);
    }else{
        AssertDebug(false, "check previous assert.");
        objs.emplace_back(objP(name, 5, otype, type));
        idx = objs.size()-1;
        index[name] = idx;
    }
    if(otype == OutType::BOOL)
    {
        assert(val == 0 || val == 1);
    }
    objs[idx].setVal(val);
    assert(!objs[idx].get_is_array());
    if(inlining_tree != nullptr) {
        auto obj = getObjConst(name);
        assert(!obj.get_is_array());
        inlining_tree->set_var_val(name, val, obj.element_size(), obj.otype, obj.get_type(), obj.get_original_name(), obj.get_source_dag_name());
    }
}

void VarStore::rename_subdag(const string &prev_name, const string &new_name) {
    assert(inlining_tree != nullptr);
    inlining_tree->rename_subdag(prev_name, new_name);
}

void VarStore::change_id(const string &prev_name, int new_id) {
    assert(inlining_tree != nullptr);
    inlining_tree->change_id(prev_name, new_id);
}

bool VarStore::contains(const VarStore::objP &obj, vector<string> *path) const
{

    assert(var_name_to_dag_name_to_name.find(obj.get_original_name()) != var_name_to_dag_name_to_name.end());
    assert(var_name_to_dag_name_to_name.at(obj.get_original_name()).find(obj.get_source_dag_name()) != var_name_to_dag_name_to_name.at(obj.get_original_name()).end());
    assert(var_name_to_dag_name_to_name.at(obj.get_original_name()).at(obj.get_source_dag_name()) == obj.name);

    const LightInliningTree* subtree = inlining_tree;
    for (int i = 0; i < path->size(); i++) {
        subtree = subtree->get_sub_inlining_tree((*path)[i]);
    }

//    assert(subtree->get_dag_name() == obj.get_source_dag_name());

    assert(contains(obj.name));

    return true;
}
