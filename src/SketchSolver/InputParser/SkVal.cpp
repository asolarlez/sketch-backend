//
// Created by kliment on 6/28/21.
//

#include "SkVal.h"


int local_get_nbits(vector<pair<int, int> >* vec)
{
    assert(vec->size() >= 1);
    int ret = vec->at(0).second;
    for(int i= 1;i<vec->size();i++)
    {
        assert(ret == vec->at(i).second);
    }
    return ret;
}


vector<int> local_get_first(vector<pair<int, int> >* vec)
{
    vector<int> ret = vector<int>();
    for(int i= 0;i<vec->size();i++)
    {
        ret.push_back(vec->at(i).first);
    }
    return ret;
}

#include "BooleanDagUtility.h"

Assignment_SkVal::Assignment_SkVal(Assignment_SkVal *to_copy):
        Mapping<SkVal>(),
        type(to_copy->type),
        name_to_original_name(to_copy->name_to_original_name),
        name_to_dag_name(to_copy->name_to_dag_name),
        var_name_to_dag_name_to_name(to_copy->var_name_to_dag_name_to_name),
        inlining_tree(new InliningTree(to_copy->inlining_tree)){

    for(auto it: to_copy->assignment)
    {
        SkValType sk_val_type = it.second->get_type();
        assert(
                sk_val_type == sk_type_int ||
                sk_val_type == sk_type_bool ||
                sk_val_type == sk_type_boolarr);
        switch (sk_val_type) {
            case sk_type_int: {
                SkValInt *new_val = new SkValInt((SkValInt *) it.second);
                set(it.first, new_val);
                break;
            }
            case sk_type_intarr:
                assert(false);
                set(it.first, new SkValIntArr(*(SkValIntArr*)it.second));
                break;
            case sk_type_bool: {
                SkValBool *new_val = new SkValBool((SkValBool *) it.second);
                set(it.first, new_val);
                break;
            }
            case sk_type_boolarr: {
                SkValBoolArr *new_val = new SkValBoolArr((SkValBoolArr *) it.second);
                set(it.first, new_val);
                break;
            }
            default:
                assert(false);
        }
    }
}

bool Assignment_SkVal::operator==(const Assignment_SkVal& other)  const
{
    if(!Mapping<SkVal>::operator==(other)) {
        return false;
    }
    else
    {
        if(!inlining_tree->match_topology(other.inlining_tree)) {
            return false;
        }
    }

    //TODO: add logic to compare the maps as well.
    //TODO: implement checkrep.

    return true;
}
