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
#include <utility>
#include "VarStore.h"

using namespace std;

template<typename T>
class PolyVal
{
    T val;
public:

    explicit PolyVal(PolyVal<T>* to_copy){
        if(std::is_pointer<T>::value)
        {
            //POINTERS NOT ALLOWED BECAUSE THEY ARE DIFFICULT TO HANDLE BC THIS IS DEEPCOPY.
            assert(false);
        }
        else if(std::is_class<T>::value)
        {
            val = to_copy->val;
        }
        else
        {
            assert((std::is_same<int,T>::value) || (std::is_same<bool,T>::value));
            val = to_copy->val;
        }
    };
    explicit PolyVal(T _val): val(std::move(_val)){
        if(std::is_pointer<T>::value)
        {
            //POINTERS NOT ALLOWED BECAUSE THEY ARE DIFFICULT TO HANDLE
            assert(false);
        }
    }
    T& get() {
        return val;
    }
    bool operator == (const PolyVal<T>& other) const
    {
        return val == other.val;
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
    explicit SkVal(SkVal* to_copy):
        sketch_val_type(to_copy->sketch_val_type), size_defined(to_copy->size_defined), nbits(to_copy->nbits) {}
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

    bool operator == (const SkVal & other) const
    {
        return (sketch_val_type == other.sketch_val_type &&
            size_defined == other.size_defined &&
            nbits == other.nbits);
    }

    virtual SkVal* clone()
    {
        assert(false);
        return nullptr;
    }
};

class SkValBool: public SkVal, public PolyVal<bool>
{
public:
    explicit SkValBool(int _val): PolyVal<bool>(_val), SkVal(sk_type_bool, 1) {assert(_val == 0 ||  _val == 1);}
    string to_string() override {return std::to_string(get());}
    explicit SkValBool(SkValBool* to_copy): SkVal(to_copy), PolyVal<bool>(to_copy){}
    bool operator == (const SkValBool& other)
    {
        return SkVal::operator==(other) && PolyVal<bool>::operator==(other);
    }
    SkValBool* clone() override {
        return new SkValBool(this);
    }
};

vector<int> local_get_first(vector<pair<int, int> >* vec);

class SkValBoolArr: public SkVal, public PolyVal<vector<int>>
{
public:
    explicit SkValBoolArr(vector<pair<int, int>>* _val): PolyVal<vector<int>>(local_get_first(_val)), SkVal(sk_type_boolarr, 1) {
        for(int i = 0;i<_val->size();i++)
        {
            assert(_val->at(i).second == 1);
        }
    }

    explicit SkValBoolArr(SkValBoolArr* to_copy): SkVal(to_copy), PolyVal<vector<int>>((PolyVal<vector<int>>*)to_copy){}

    string to_string() override {
        string ret;
        vector<int>& val = get();
        for(int i = 0;i < val.size();i++)
        {
            ret += std::to_string(val.at(i));
        }
        return ret;
    }
    bool operator == (const SkValBoolArr& other)
    {
        return SkVal::operator==(other) && PolyVal<vector<int>>::operator==(other);
    }
    SkValBoolArr* clone() override {
        return new SkValBoolArr(this);
    }
};

int local_get_nbits(vector<pair<int, int> >* vec);
class SkValIntArr: public SkVal, public PolyVal<vector<int>>
{

public:
    explicit SkValIntArr(vector<pair<int, int> >* _val):
    PolyVal<vector<int>>(local_get_first(_val)), SkVal(sk_type_intarr, local_get_nbits(_val)) {}
    string to_string() override {
        string ret;
        vector<int>& val = get();
        for(int i = 0;i < val.size();i++)
        {
            ret += std::to_string(val.at(i));
        }
        return ret;
    }
    bool operator == (const SkValIntArr& other)
    {
        return SkVal::operator==(other) && PolyVal<vector<int>>::operator==(other);
    }
    SkValIntArr* clone() override {
        assert(false);
//        return new SkValIntArr(this);
    }
};

class SkValInt: public SkVal, public PolyVal<int>
{
public:
    explicit SkValInt(int _val, int _size): PolyVal<int>(_val), SkVal(sk_type_int, _size){
        assert(_val <= (1<<_size)-1);
    }
    explicit SkValInt(SkValInt* to_copy): SkVal(to_copy), PolyVal<int>(to_copy){}
    string to_string() override {return std::to_string(get());}

    bool operator == (const SkValInt& other)
    {
        return SkVal::operator==(other) && PolyVal<int>::operator==(other);
    }
    SkValInt* clone() override {
        return new SkValInt(this);
    }
};
class SkValFloat: public SkVal, public PolyVal<float>
{
public:
//    explicit SkValFloat(float _val): PolyVal<float>(_val), SkVal(sk_type_float){}
    explicit SkValFloat(float _val, int _size): PolyVal<float>(_val), SkVal(sk_type_float, _size){}
    string to_string() override {return std::to_string(get());}
    bool operator == (const SkValFloat& other)
    {
        return SkVal::operator==(other) && PolyVal<float>::operator==(other);
    }
    SkValFloat* clone() override {
        assert(false);
//        return new SkValFloat(this);
    }
};

template<typename ValType>
class Mapping
{
protected:
    bool null = true;
    map<string, ValType*> assignment;
public:
    void clear()
    {
        for(auto& it:assignment)
        {
            delete it.second;
            it.second = nullptr;
        }
        assignment.clear();
    }
    Mapping() = default;
    explicit Mapping(bool is_null): null(is_null) {};
    
    bool operator < (const Mapping& other) const
    {
        if(null > other.null)
        {
            return true;
        }
        else if(null < other.null)
        {
            return false;
        }
        return assignment < other.assignment;
    }

    bool operator == (const Mapping& other) const
    {
        if(null != other.null)
        {
            return false;
        }

        if(assignment.size() != other.assignment.size())
        {
            return false;
        }

        for(const auto & it: assignment) {
            const auto & other_it = other.assignment.find(it.first);
            if(other_it == other.assignment.end())
            {
                return false;
            }
            else
            {
                if(!(*it.second == *other_it->second))
                {
                    return false;
                }
            }
        }


        for(const auto & other_it : other.assignment) {
            const auto & it = assignment.find(other_it.first);
            if(it == assignment.end())
            {
                return false;
            }
            else
            {
                if(!(*it->second == *other_it.second))
                {
                    return false;
                }
            }
        }

        return true;
    }
    
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

    bool_node::Type type = bool_node::NO_TYPE;

    map<string, string> name_to_original_name;
    map<string, string> name_to_dag_name;

    void set_it(Assignment_SkVal *updated_assignment, string name, SkVal* val)
    {
        set(name, val->clone());
        assert(updated_assignment->name_to_original_name.find(name) != updated_assignment->name_to_original_name.end());
        name_to_original_name[name] = updated_assignment->name_to_original_name[name];
        assert(updated_assignment->name_to_dag_name.find(name) != updated_assignment->name_to_dag_name.end());
        name_to_dag_name[name] = updated_assignment->name_to_dag_name[name];
    }

    static OutType* sk_val_type_to_bool_node_out_type(SkValType sk_val_type) {
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

public:
    Assignment_SkVal(bool_node::Type _type): type(_type), Mapping<SkVal>() {}

    template<typename T>
    explicit Assignment_SkVal(bool_node::Type _type, T is_null): type(_type), Mapping<SkVal>(is_null) {assert((std::is_same<T, bool>::value));}

    explicit Assignment_SkVal(Assignment_SkVal* to_copy):
        Mapping<SkVal>(),
        type(to_copy->type),
        name_to_original_name(to_copy->name_to_original_name),
        name_to_dag_name(to_copy->name_to_dag_name){

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

    Assignment_SkVal(const VarStore* var_store, FloatManager& floats): Mapping<SkVal>() {
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
                assert(false);
                Assert(false, "need to add more OutType to SkVal conversions.");
            }

            assert(name_to_original_name.find(name) == name_to_original_name.end());
            name_to_original_name[name] = original_name;
            assert(name_to_dag_name.find(name) == name_to_dag_name.end());
            name_to_dag_name[name] = source_dag_name;

        }

        VarStore* test_var_store = to_var_store();


        assert(test_var_store->size() == var_store->size());

        for(auto it = var_store->begin(); it !=var_store->end(); ++it) {
            assert(test_var_store->getObjConst(it->getName()) == var_store->getObjConst(it->getName()));
        }
        for(auto it = (*test_var_store).begin(); it !=(*test_var_store).end(); ++it) {
            assert(test_var_store->getObjConst(it->getName()) == var_store->getObjConst(it->getName()));
        }

        delete test_var_store;

    }

    VarStore* to_var_store()
    {
        auto* ret = new VarStore();
        for(auto item : assignment)
        {
            if(item.second->get_type() == sk_type_int)
            {
                ret->newVar(item.first, item.second->get_nbits(), sk_val_type_to_bool_node_out_type(item.second->get_type()), type, name_to_original_name[item.first], name_to_dag_name[item.first]);
                ret->setVarVal(item.first, ((SkValInt*) item.second)->get(), sk_val_type_to_bool_node_out_type(item.second->get_type()), type);
//                cout << item.first <<" (varstore) "<< (*ret)[item.first] << " (SkValBool) val "<< ((SkValInt*) item.second)->get() << " nbits " << item.second->get_nbits()<< endl;
            }
            else if(item.second->get_type() == sk_type_bool)
            {
                ret->newVar(item.first, item.second->get_nbits(), sk_val_type_to_bool_node_out_type(item.second->get_type()), type, name_to_original_name[item.first], name_to_dag_name[item.first]);
                ret->setVarVal(item.first, ((SkValBool*) item.second)->get(), sk_val_type_to_bool_node_out_type(item.second->get_type()), type);
//                cout << item.first <<" (varstore) "<< (*ret)[item.first] << " (SkValBool) val "<< ((SkValBool*) item.second)->get() << " nbits " << item.second->get_nbits()<< endl;

            }
            else if(item.second->get_type() == sk_type_boolarr)
            {
                assert(item.second->get_nbits() == 1);
                ret->newArr(item.first, 1,  (int)((SkValBoolArr*)item.second)->get().size(), OutType::BOOL_ARR, type);
                ret->setArr(item.first, ((SkValBoolArr*)item.second)->get());
            }
            else if(item.second->get_type() == sk_type_intarr)
            {
                ret->newArr(item.first, item.second->get_nbits(),  (int)((SkValIntArr*)item.second)->get().size(), OutType::INT_ARR, type);
                ret->setArr(item.first, ((SkValIntArr*)item.second)->get());
            }
            else
            {
                AssertDebug(false, "TODO: cast SkFloat back into an entry in VarStore.");
            }
        }

        return ret;
    }

    void update(Assignment_SkVal *updated_assignment) {
        assert(!null);
        assert(!updated_assignment->null);
        for(const auto& it : updated_assignment->get_assignment()) {
            set_it(updated_assignment, it.first, it.second->clone());
        }
    }
    void disjoint_join_with(Assignment_SkVal *assignment_to_join_with) {
        assert(!null);
        assert(!assignment_to_join_with->null);
        for(const auto& it : assignment_to_join_with->get_assignment()) {
            assert(!has(it.first));
            set_it(assignment_to_join_with, it.first, it.second->clone());
        }
    }
};

class SkHoleSpec
{
    string name;
    SkValType type;
public:
    SkHoleSpec(string _name, SkValType _type): name(std::move(_name)), type(_type) {}
    string get_name() const
    {
        return name;
    }
    SkValType get_type() const
    {
        return type;
    }
};

template<typename T>
bool lt_compare_pointers(T* left, T* right)
{
    if(left == nullptr && right == nullptr)
    {
        return false;
    }
    else if(left == nullptr && right != nullptr)
    {
        return true;
    }
    else if(left != nullptr && right == nullptr)
    {
        return false;
    }
    assert(left != nullptr && right != nullptr);
    return *left < *right;
}

template<typename T>
bool eq_compare_pointers(T* left, T* right)
{
    if(left == nullptr && right != nullptr)
    {
        return false;
    }
    else if(left != nullptr && right == nullptr)
    {
        return false;
    }
    assert((left == nullptr) == (right == nullptr));
    return *left == *right;
}

namespace SolverLanguagePrimitives {

    class ProblemAE;

    class HoleAssignment {
        Assignment_SkVal *assignment_skval = nullptr;
        SATSolverResult sat_solver_result = SAT_UNDETERMINED;
    public :

        void clear() {
            assignment_skval->clear();
            delete assignment_skval;
            assignment_skval = nullptr;
        }

        explicit HoleAssignment(SATSolverResult _sat_solver_result) :
                sat_solver_result(_sat_solver_result) {
        }

        bool operator < (const HoleAssignment& other) const
        {
            if(sat_solver_result < other.sat_solver_result){
                return true;
            }
            else if(sat_solver_result > other.sat_solver_result){
                return false;
            }
            return lt_compare_pointers(assignment_skval, other.assignment_skval);
        }

        bool operator == (const HoleAssignment& other) const
        {
            if(sat_solver_result != other.sat_solver_result){
                return false;
            }
            return eq_compare_pointers(assignment_skval, other.assignment_skval);
        }



        HoleAssignment(SATSolverResult _sat_solver_result, Assignment_SkVal *_assignment_skval) :
                sat_solver_result(_sat_solver_result), assignment_skval(_assignment_skval) {}

        HoleAssignment(SATSolverResult _sat_solver_result, const VarStore *ctrl_store, FloatManager &floats) :
                sat_solver_result(_sat_solver_result), assignment_skval(new Assignment_SkVal(ctrl_store, floats)) {}

        explicit HoleAssignment(HoleAssignment *to_copy) : sat_solver_result(to_copy->sat_solver_result), assignment_skval(
                new Assignment_SkVal(to_copy->assignment_skval)) {}

        HoleAssignment() = default;
        explicit HoleAssignment(bool is_null): assignment_skval(new Assignment_SkVal(bool_node::CTRL, is_null)) {};

        explicit HoleAssignment(ProblemAE* problem) {
            cout << "TODO: HoleAssignment::HoleAssignment" << endl;
            assert(false);
        }

        SATSolverResult get_sat_solver_result() {
            return sat_solver_result;
        }

        void set_sat_solver_result(SATSolverResult _rez) {
            sat_solver_result = _rez;
        }

        string to_string() {
            string ret = SATSolverResultNames[sat_solver_result] + assignment_skval->to_string();
            return ret;
        }

        VarStore *to_var_store() {
            return assignment_skval->to_var_store();
        }

        void get_control_map(map<string, string> &map) {
            if (assignment_skval != NULL) {
                for (auto it: assignment_skval->get_assignment()) {
                    map[it.first] = it.second->to_string();
                }
            }
        }

        bool has_assignment_skval() {
            if (assignment_skval == NULL) {
                return false;
            } else {
                return !assignment_skval->is_null();
            }
        }

        Assignment_SkVal *get_assignment() {
            return assignment_skval;
        }

        void update(HoleAssignment *updated_solution_holder) {
            sat_solver_result = updated_solution_holder->get_sat_solver_result();
            if (updated_solution_holder->get_assignment()->is_null()) {
                assignment_skval = new Assignment_SkVal(bool_node::CTRL);
            } else {
                assignment_skval->update(updated_solution_holder->get_assignment());
            }
        }

        void join_with(HoleAssignment* other)
        {
            if(other->sat_solver_result == SAT_UNSATISFIABLE) {
                sat_solver_result = SAT_UNSATISFIABLE;
            }
            else{
                assert(other->sat_solver_result == SAT_SATISFIABLE);
                if(sat_solver_result == SAT_UNDETERMINED)
                {
                    sat_solver_result = other->sat_solver_result;
                }
            }
            if (other->get_assignment()->is_null()) {
                //do nothing;
            } else {
                assignment_skval->disjoint_join_with(other->get_assignment());
            }
        }

        HoleAssignment *clone() {
            return new HoleAssignment(this);
        }
    };

    class InputAssignment : public Assignment_SkVal {
    public :
        InputAssignment() : Assignment_SkVal(bool_node::SRC) {}

        InputAssignment(VarStore *input, FloatManager &floats) : Assignment_SkVal(input, floats) {}

        explicit InputAssignment(InputAssignment *to_copy) : Assignment_SkVal(new Assignment_SkVal((Assignment_SkVal*)to_copy)) {}

    };

}

#endif //SKETCH_SOURCE_SKVAL_H
