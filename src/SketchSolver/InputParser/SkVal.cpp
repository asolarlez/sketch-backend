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

Assignment_SkVal::Assignment_SkVal(const InliningTree *_inlining_tree, FloatManager &floats) :
Mapping<SkVal>(), inlining_tree(new InliningTree(_inlining_tree)) {}

Assignment_SkVal::Assignment_SkVal(const VarStore *var_store, FloatManager &floats): Mapping<SkVal>() {
    assert(var_store != nullptr);
    if(var_store->get_inlining_tree() != nullptr) {
        inlining_tree = new InliningTree(var_store->get_inlining_tree());
    }

    for(auto it = var_store->begin(); it != var_store->end(); it++)
    {
        if(type != bool_node::NO_TYPE){
            assert((*it).get_type() == type);
        }
        else {
            type = (*it).get_type();
        }
        OutType* out_type = (*it).getOtype();
        string name = (*it).getName();
        string original_name = (*it).get_original_name();
        string source_dag_name = (*it).get_source_dag_name();
        if(out_type == OutType::INT) {
            set(name, new SkValInt((*it).getInt(), (*it).get_size()));
        }
        else if (out_type == OutType::FLOAT)
        {
            set(name, new SkValFloat(floats.getFloat((*it).getInt()), (*it).get_size()));
        }
        else if (out_type == OutType::BOOL)
        {
            set(name, new SkValBool((*it).getInt()));
        }
        else if(out_type == OutType::BOOL_ARR)
        {
            set(name, new SkValBoolArr((*it).getArr()));
        }
        else if(out_type == OutType::INT_ARR)
        {
            set(name, new SkValIntArr((*it).getArr()));
        }
        else
        {
            AssertDebug(false, "need to add more OutType to SkVal conversions.");
        }

        assert(name_to_original_name.find(name) == name_to_original_name.end());
        name_to_original_name[name] = original_name;
        assert(name_to_dag_name.find(name) == name_to_dag_name.end());
        name_to_dag_name[name] = source_dag_name;

        if(type == bool_node::CTRL) {
            set_var_name_to_dag_name_to_name(name);
        }
        else {
            assert(type == bool_node::SRC);
        }
    }

    VarStore* test_var_store = to_var_store(false);


    assert(test_var_store->size() == var_store->size());

    for(auto it = var_store->begin(); it !=var_store->end(); ++it) {
        assert(test_var_store->getObjConst(it->getName()) == var_store->getObjConst(it->getName()));
    }
    for(auto it = (*test_var_store).begin(); it !=(*test_var_store).end(); ++it) {
        assert(test_var_store->getObjConst(it->getName()) == var_store->getObjConst(it->getName()));
    }

    test_var_store->clear();

}

void Assignment_SkVal::set_inlining_tree(const InliningTree *_inlining_tree)
{
    assert(inlining_tree == nullptr);
    inlining_tree = new InliningTree(_inlining_tree);
}

void Assignment_SkVal::clear(bool clear_root, bool sub_clear) {
    type = bool_node::NO_TYPE;
    name_to_original_name.clear();
    name_to_dag_name.clear();
    var_name_to_dag_name_to_name.clear();
    if(inlining_tree != nullptr) {
        inlining_tree->clear(clear_root, sub_clear);
    }
    Mapping<SkVal>::clear();
}
