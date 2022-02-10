//
// Created by kliment on 2/8/22.
//

#include "VarStore.h"

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
        ret &= var_name_to_dag_name_to_name.at(original_name).size() >= 1;
    }
    return ret;
}

void VarStore::rename(const string &original_name, const string &new_source_dag, const string &new_name) {
    assert(var_name_to_dag_name_to_name.find(original_name) != var_name_to_dag_name_to_name.end());
    if(contains(new_name)){
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
        AssertDebug(var_name_to_dag_name_to_name[original_name].size() == 1, "Check why this fails and respond accordingly.")
        string obj_name = var_name_to_dag_name_to_name[original_name].begin()->second;
        assert(index.find(obj_name) != index.end());
        auto& obj = _getObj(obj_name);
        assert(obj.get_original_name() == original_name);
        assert(obj_name == obj.name);
        string prev_source_dag_name = obj.get_source_dag_name();
        assert(var_name_to_dag_name_to_name[original_name].find(prev_source_dag_name) != var_name_to_dag_name_to_name[original_name].end());
        obj.rename(new_name, new_source_dag);

        assert(index.find(new_name) == index.end());
        index[new_name] = index[obj_name];
        index.erase(obj_name);
        var_name_to_dag_name_to_name[original_name].erase(prev_source_dag_name);
        var_name_to_dag_name_to_name[original_name][new_source_dag] = new_name;
    }
}

const string &VarStore::get_name(const string& var_name, const string &source_dag_name) {
    assert(var_name_to_dag_name_to_name.find(var_name) != var_name_to_dag_name_to_name.end());
    assert(var_name_to_dag_name_to_name[var_name].find(source_dag_name) != var_name_to_dag_name_to_name[var_name].end());
    return var_name_to_dag_name_to_name[var_name][source_dag_name];
}
