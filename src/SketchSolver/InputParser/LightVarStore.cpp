//
// Created by Kliment Serafimov on 7/17/22.
//

#include "LightVarStore.h"

LightVarStore::LightVarStore(const LightVarStore &to_copy, bool deep_clone)
{
    AssertDebug(to_copy.synths.empty(), "TODO: implement copy logic for synths and synthouths.");
    AssertDebug(to_copy.synthouts.empty(), "TODO: implement copy logic for synths and synthouths.");

    *this = to_copy;
}


void LightVarStore::operator=(const LightVarStore &to_copy){

    AssertDebug(to_copy.synths.empty(), "TODO: implement copy logic for synths and synthouths.");
    AssertDebug(to_copy.synthouts.empty(), "TODO: implement copy logic for synths and synthouths.");

    //local clear.
    objs.clear();
    index.clear();
    bitsize = 0;

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
        insertObj(it.second, true_idx, objP(to_copy.objs[it.first]));
        true_idx++;
    }
}