//
// Created by kliment on 6/28/21.
//

#ifndef SKETCH_SOURCE_SKVAL_H
#define SKETCH_SOURCE_SKVAL_H

#include <string>
#include <cassert>
#include <system_error>
#include <map>
#include <iostream>
#include "VarStore.h"

using namespace std;

template<typename T>
class PolyVal
{
    T val;
public:
    explicit PolyVal(T _val): val(_val){}
    T get() {
        return val;
    }
};

enum SkValType {sk_type_int, sk_type_float, sk_type_bool, sk_type_boolarr, sk_type_intarr};
static string sk_val_type_name[5] = {"sk_type_int", "sk_type_float", "sk_type_bool", "sk_type_boolarr", "sk_type_intarr"};

class SkVal
{
    SkValType sketch_val_type;
    bool size_defined = false;
    int nbits;
public:
//    explicit SkVal(SkValType _sketch_val_type):
//            sketch_val_type(_sketch_val_type) {}
    SkVal(SkValType _sketch_val_type, int _nbits):
            sketch_val_type(_sketch_val_type), nbits(_nbits), size_defined(true) {}
    int get_nbits()
    {
        Assert(size_defined, "SkVal size not defined");
        return nbits;
    }
    SkValType get_type() {
        return sketch_val_type;
    }
    virtual string to_string()
    {
        assert(false);
    }

    string get_type_as_string()
    {
        return sk_val_type_name[get_type()];
    }
};

class SkValBool: public SkVal, public PolyVal<bool>
{
public:
    explicit SkValBool(int _val): PolyVal<bool>(_val), SkVal(sk_type_bool, 1) {assert(_val == 0 ||  _val == 1);}
    string to_string() override {return std::to_string(get());}
};

vector<int>* local_get_first(vector<pair<int, int> >* vec);

class SkValBoolArr: public SkVal, public PolyVal<vector<int>*>
{
public:
    explicit SkValBoolArr(vector<pair<int, int>>* _val): PolyVal<vector<int>*>(local_get_first(_val)), SkVal(sk_type_boolarr, 1) {
        for(int i = 0;i<_val->size();i++)
        {
            assert(_val->at(i).second == 1);
        }
    }
    string to_string() override {
        string ret;
        vector<int>* val = get();
        for(int i = 0;i < val->size();i++)
        {
            ret += std::to_string(val->at(i));
        }
        return ret;
    }
};

int local_get_nbits(vector<pair<int, int> >* vec);
class SkValIntArr: public SkVal, public PolyVal<vector<int>*>
{

public:
    explicit SkValIntArr(vector<pair<int, int> >* _val): PolyVal<vector<int>*>(local_get_first(_val)), SkVal(sk_type_intarr, local_get_nbits(_val)) {}
    string to_string() override {
        string ret;
        vector<int>* val = get();
        for(int i = 0;i < val->size();i++)
        {
            ret += std::to_string(val->at(i));
        }
        return ret;
    }
};

class SkValInt: public SkVal, public PolyVal<int>
{
public:
//    explicit SkValInt(int _val): PolyVal<int>(_val), SkVal(sk_type_int){}
    explicit SkValInt(int _val, int _size): PolyVal<int>(_val), SkVal(sk_type_int, _size){
        assert(_val <= (1<<_size)-1);
    }
    string to_string() override {return std::to_string(get());}
};
class SkValFloat: public SkVal, public PolyVal<float>
{
public:
//    explicit SkValFloat(float _val): PolyVal<float>(_val), SkVal(sk_type_float){}
    explicit SkValFloat(float _val, int _size): PolyVal<float>(_val), SkVal(sk_type_float, _size){}
    string to_string() override {return std::to_string(get());}
};

template<typename ValType>
class Mapping
{
protected:
    bool null = true;
    map<string, ValType*> assignment;
public:
    Mapping() = default;
    map<string, ValType*>& get_assignment()
    {
        return assignment;
    }
    bool has(const string& name)
    {
        return assignment.find(name) != assignment.end();
    }
    ValType* get(const string& name)
    {
        assert(has(name));
        return assignment[name];
    }
    void set(const string& name, ValType* val)
    {
        null = false;
        assignment[name] = val;
    }
    bool is_null()
    {
        return null;
    }
    string to_string()
    {
        if(null)
        {
            return "null";
        }
        else {
            string ret = "{";
            for (auto p : assignment) {
                ret += p.first + " <- " + p.second->to_string() + "; ";
            }
            ret += "}";
            return ret;
        }
    }
};

class Assignment_SkVal: public Mapping<SkVal> {
public:
    Assignment_SkVal(): Mapping<SkVal>() {}
    explicit Assignment_SkVal(Assignment_SkVal* to_copy): Mapping<SkVal>() {
        for(auto it: to_copy->assignment)
        {
            SkValType sk_val_type = it.second->get_type();
            switch (sk_val_type) {
                case sk_type_int:
                    set(it.first, new SkValInt(*(SkValInt*)it.second));
                    break;
                case sk_type_intarr:
                    set(it.first, new SkValIntArr(*(SkValIntArr*)it.second));
                    break;
                case sk_type_bool:
                    set(it.first, new SkValBool(*(SkValBool*)it.second));
                    break;
                case sk_type_boolarr:
                    set(it.first, new SkValBoolArr(*(SkValBoolArr*)it.second));
                    break;
                default:
                    assert(false);
            }
        }
    }
    Assignment_SkVal(VarStore* var_store, FloatManager& floats): Mapping<SkVal>() {
        for(auto it = var_store->begin(); it != var_store->end(); it++)
        {
            OutType* out_type = (*it).getOtype();
            if(out_type == OutType::INT) {
                set((*it).getName(), new SkValInt((*it).getInt(), (*it).get_size()));
            }
            else if (out_type == OutType::FLOAT)
            {
                set((*it).getName(), new SkValFloat(floats.getFloat((*it).getInt()), (*it).get_size()));
            }
            else if (out_type == OutType::BOOL)
            {
                set((*it).getName(), new SkValBool((*it).getInt()));
            }
            else if(out_type == OutType::BOOL_ARR)
            {
                set((*it).getName(), new SkValBoolArr((*it).getArr()));
            }
            else if(out_type == OutType::INT_ARR)
            {
                set((*it).getName(), new SkValIntArr((*it).getArr()));
            }
            else
            {
                assert(false);
                Assert(false, "need to add more OutType to SkVal conversions.");
            }
        }

        VarStore* test_var_store = to_var_store();


        assert(test_var_store->size() == var_store->size());

        for(VarStore::iterator it = var_store->begin(); it !=var_store->end(); ++it) {
            assert(test_var_store->getObj(it->getName()) == var_store->getObj(it->getName()));
        }
        for(VarStore::iterator it = (*test_var_store).begin(); it !=(*test_var_store).end(); ++it) {
            assert(test_var_store->getObj(it->getName()) == var_store->getObj(it->getName()));
        }

    }
    static OutType* sk_val_type_to_bool_node_out_type(SkValType sk_val_type)
    {
        switch (sk_val_type) {
            case sk_type_int:
                return OutType::INT;
                break;
            case sk_type_float:
                return OutType::FLOAT;
                break;
            case sk_type_bool:
                return OutType::BOOL;
                break;
            case sk_type_boolarr:
                return OutType::BOOL_ARR;
                break;
            default:
                assert(false);
        }
        assert(false);
    }
    VarStore* to_var_store()
    {
        auto* ret = new VarStore();
        for(auto item : assignment)
        {
            if(item.second->get_type() == sk_type_int)
            {
                ret->newVar(item.first, item.second->get_nbits(), sk_val_type_to_bool_node_out_type(item.second->get_type()));
                ret->setVarVal(item.first, ((SkValInt*) item.second)->get(), sk_val_type_to_bool_node_out_type(item.second->get_type()));
//                cout << item.first <<" (varstore) "<< (*ret)[item.first] << " (SkValBool) val "<< ((SkValInt*) item.second)->get() << " nbits " << item.second->get_nbits()<< endl;
            }
            else if(item.second->get_type() == sk_type_bool)
            {
                ret->newVar(item.first, item.second->get_nbits(), sk_val_type_to_bool_node_out_type(item.second->get_type()));
                ret->setVarVal(item.first, ((SkValBool*) item.second)->get(), sk_val_type_to_bool_node_out_type(item.second->get_type()));
//                cout << item.first <<" (varstore) "<< (*ret)[item.first] << " (SkValBool) val "<< ((SkValBool*) item.second)->get() << " nbits " << item.second->get_nbits()<< endl;

            }
            else if(item.second->get_type() == sk_type_boolarr)
            {
                assert(item.second->get_nbits() == 1);
                ret->newArr(item.first, 1,  ((SkValBoolArr*)item.second)->get()->size(), OutType::BOOL_ARR);
                ret->setArr(item.first, ((SkValBoolArr*)item.second)->get());
            }
            else if(item.second->get_type() == sk_type_intarr)
            {
                ret->newArr(item.first, item.second->get_nbits(),  ((SkValIntArr*)item.second)->get()->size(), OutType::INT_ARR);
                ret->setArr(item.first, ((SkValIntArr*)item.second)->get());
            }
            else
            {
                Assert(false, "TODO: cast SkFloat back into an entry in VarStore.");
            }
        }

        return ret;
    }

    void update(Assignment_SkVal *updated_assignment) {
        for(const auto& it : updated_assignment->get_assignment())
        {
            set(it.first, it.second);
        }
    }
};


#endif //SKETCH_SOURCE_SKVAL_H
