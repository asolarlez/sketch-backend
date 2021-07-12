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

enum SkValType {sk_type_int, sk_type_float, sk_type_bool};
static string sk_val_type_name[3] = {"sk_type_int", "sk_type_float", "sk_type_bool"};

class SkVal
{
    SkValType sketch_val_type;
//    int size;
public:
    explicit SkVal(SkValType _sketch_val_type):
            sketch_val_type(_sketch_val_type) {}
    int get_size()
    {
        cout << "size is not initialized" << endl;
        assert(false);
//        return size;
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
    explicit SkValBool(int _val): PolyVal<bool>(_val), SkVal(sk_type_bool) {}
    string to_string() override {return std::to_string(get());}
};
class SkValInt: public SkVal, public PolyVal<int>
{
public:
    explicit SkValInt(int _val): PolyVal<int>(_val), SkVal(sk_type_int){}
    string to_string() override {return std::to_string(get());}
};
class SkValFloat: public SkVal, public PolyVal<float>
{
public:
    explicit SkValFloat(float _val): PolyVal<float>(_val), SkVal(sk_type_float){}
    string to_string() override {return std::to_string(get());}
};

template<typename ValType>
class Assignment
{
protected:
    bool null = true;
    map<string, ValType*> assignment;
public:
    Assignment() = default;
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

class Assignment_SkVal: public Assignment<SkVal> {
public:
    Assignment_SkVal(): Assignment<SkVal>() {}
    Assignment_SkVal(VarStore* var_store, FloatManager& floats): Assignment<SkVal>() {
        for(auto it = var_store->begin(); it != var_store->end(); it++)
        {
            auto star_it = *it;
            OutType* out_type = (*it).getOtype();
            if(out_type == OutType::INT) {
                set((*it).getName(), new SkValInt((*it).getInt()));
            }
            else if (out_type == OutType::FLOAT)
            {
                set((*it).getName(), new SkValFloat(floats.getFloat((*it).getInt())));
            }
            else if (out_type == OutType::BOOL)
            {
                set((*it).getName(), new SkValBool((*it).getInt()));
            }
            else
            {
                assert(false);
                Assert(false, "need to add more OutType to SkVal conversions.");
            }
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
                assert(false);
        }
        assert(false);
    }
    VarStore* to_var_store()
    {
        auto* ret = new VarStore();
        for(auto item : assignment)
        {
            ret->newVar(item.first, item.second->get_size(), sk_val_type_to_bool_node_out_type(item.second->get_type()));
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
