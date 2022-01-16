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
    explicit PolyVal(PolyVal<T>* to_copy){
        if(std::is_pointer<T>::value)
        {
            assert(false);
            val = T(to_copy->val);
        }
        else if(std::is_class<T>::value)
        {
            assert(false);
            val = T(to_copy->val);
        }
        else
        {
            assert((std::is_same<int,T>::value));
            val = to_copy->val;
        }
    };
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
    explicit SkValIntArr(vector<pair<int, int> >* _val):
    PolyVal<vector<int>*>(local_get_first(_val)), SkVal(sk_type_intarr, local_get_nbits(_val)) {}
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
    explicit SkValInt(int _val, int _size): PolyVal<int>(_val), SkVal(sk_type_int, _size){
        assert(_val <= (1<<_size)-1);
    }
    explicit SkValInt(SkValInt* to_copy): SkVal(to_copy), PolyVal<int>(to_copy){}
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
    void clear()
    {
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
    explicit Assignment_SkVal(bool is_null): Mapping<SkVal>(is_null) {}
    explicit Assignment_SkVal(Assignment_SkVal* to_copy): Mapping<SkVal>() {
        for(auto it: to_copy->assignment)
        {
            SkValType sk_val_type = it.second->get_type();
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
                case sk_type_bool:
                    assert(false);
                    set(it.first, new SkValBool(*(SkValBool*)it.second));
                    break;
                case sk_type_boolarr:
                    assert(false);
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

        delete test_var_store;

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
        assert(!null);
        assert(!updated_assignment->null);
        for(const auto& it : updated_assignment->get_assignment())
        {
            set(it.first, it.second);
        }
    }
    void join_with(Assignment_SkVal *assignment_to_join_with) {
        assert(!null);
        assert(!assignment_to_join_with->null);
        for(const auto& it : assignment_to_join_with->get_assignment())
        {
//            assert(!has(it.first));
            set(it.first, it.second);
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

namespace SolverLanguagePrimitives {

    class ProblemAE;

    class SolutionHolder {
        Assignment_SkVal *assignment_skval = nullptr;
        SATSolver::SATSolverResult sat_solver_result = SATSolver::UNDETERMINED;
    public :

        void clear()
        {
            assignment_skval->clear();
        }

        explicit SolutionHolder(SATSolver::SATSolverResult _sat_solver_result) :
                sat_solver_result(_sat_solver_result) {
        }

        bool operator < (const SolutionHolder& other) const
        {
            if(sat_solver_result < other.sat_solver_result){
                return true;
            }
            else if(sat_solver_result > other.sat_solver_result){
                return false;
            }
            return lt_compare_pointers(assignment_skval, other.assignment_skval);
        }

        SolutionHolder(SATSolver::SATSolverResult _sat_solver_result, Assignment_SkVal *_assignment_skval) :
                sat_solver_result(_sat_solver_result), assignment_skval(_assignment_skval) {}

        SolutionHolder(SATSolver::SATSolverResult _sat_solver_result, VarStore *ctrl_store, FloatManager &floats) :
                sat_solver_result(_sat_solver_result), assignment_skval(new Assignment_SkVal(ctrl_store, floats)) {}

        explicit SolutionHolder(SolutionHolder *to_copy) : sat_solver_result(to_copy->sat_solver_result), assignment_skval(
                new Assignment_SkVal(to_copy->assignment_skval)) {}

        SolutionHolder() = default;;
        explicit SolutionHolder(bool is_null): assignment_skval(new Assignment_SkVal(is_null)) {};

        explicit SolutionHolder(ProblemAE* problem) {
            cout << "TODO: SolutionHolder::SolutionHolder" << endl;
            assert(false);
        }
        VarStore *get_controls(FloatManager &floats) {
            VarStore *ret = new VarStore();
            for (auto it: assignment_skval->get_assignment()) {
                switch (it.second->get_type()) {

                    case sk_type_int:
                        ret->setVarVal(it.first, ((SkValInt *) it.second)->get(), OutType::INT);
                        break;
                    case sk_type_float:
                        ret->setVarVal(it.first, floats.getIdx(((SkValFloat *) it.second)->get()), OutType::FLOAT);
                        break;
                    case sk_type_bool:
                        ret->setVarVal(it.first, ((SkValBool *) it.second)->get(), OutType::BOOL);
                        break;
                    default:
                        AssertDebug(false, "missing skval cases.")

                }
            }
            return ret;
        }

        SATSolver::SATSolverResult get_sat_solver_result() {
            return sat_solver_result;
        }

        void set_sat_solver_result(SATSolver::SATSolverResult _rez) {
            sat_solver_result = _rez;
        }

        string to_string() {
            cout << "SolutionHolder::to_string" << endl;
            assert(false);
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

        void update(SolutionHolder *updated_solution_holder) {
            sat_solver_result = updated_solution_holder->get_sat_solver_result();
            if (updated_solution_holder->get_assignment()->is_null()) {
                assignment_skval = new Assignment_SkVal();
            } else {
                assignment_skval->update(updated_solution_holder->get_assignment());
            }
        }

        void join_with(SolutionHolder* other)
        {
            if(other->sat_solver_result == SATSolver::UNSATISFIABLE) {
                sat_solver_result = SATSolver::UNSATISFIABLE;
            }
            else{
                assert(other->sat_solver_result == SATSolver::SATISFIABLE);
                if(sat_solver_result == SATSolver::UNDETERMINED)
                {
                    sat_solver_result = other->sat_solver_result;
                }
            }
            if (other->get_assignment()->is_null()) {
                //do nothing;
            } else {
                assignment_skval->join_with(other->get_assignment());
            }
        }

        SolutionHolder *clone() {
            return new SolutionHolder(this);
        }
    };

    class InputHolder : public Assignment_SkVal {
    public :
        InputHolder() : Assignment_SkVal() {}

        InputHolder(VarStore *input, FloatManager &floats) : Assignment_SkVal(input, floats) {}

        explicit InputHolder(InputHolder *to_copy) : Assignment_SkVal(new Assignment_SkVal(to_copy)) {}

        explicit InputHolder(ProblemAE *problem) : Assignment_SkVal() {
            cout << "TODO: SolutionHolder::SolutionHolder" << endl;
            assert(false);
        }
    };

}

#endif //SKETCH_SOURCE_SKVAL_H
