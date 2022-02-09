//
// Created by kliment on 2/8/22.
//

#include "VarStore.h"

bool VarStore::has_original_name(const string &original_name) const {
    return original_name_to_name.find(original_name) != original_name_to_name.end();
}

void VarStore::rename(const string &original_name, const string &new_name) {
    assert(original_name_to_name.find(original_name) != original_name_to_name.end());
    if(contains(new_name)){
        //if new name already exists make sure that it's the same as the original name
        assert(original_name == new_name);
        assert(getObjConst(new_name).get_original_name() == original_name);
    }
    else
    {
        string obj_name = original_name_to_name[original_name];
        assert(index.find(obj_name) != index.end());
        auto& obj = _getObj(obj_name);
        assert(obj.get_original_name() == original_name);
        assert(obj_name == obj.name);
        obj.rename(new_name);

        assert(index.find(new_name) == index.end());
        index[new_name] = index[obj_name];
        index.erase(obj_name);

        original_name_to_name[original_name] = new_name;
    }
}
