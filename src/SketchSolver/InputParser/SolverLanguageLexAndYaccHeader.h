//
// Created by kliment on 12/17/21.
//

#ifndef SOLVERLANGUAGEPARSER_LEXANDYACCHEADER_H
#define SOLVERLANGUAGEPARSER_LEXANDYACCHEADER_H

#include <iostream>
#include <cassert>
#include <string>
#include <utility>
#include <map>
#include <vector>
#include <fstream>
#include "SketchFunction.h"

using namespace std;

class SolverProgramState;
class Frame;

class SketchFunction;
class FloatManager;
class CommandLineArgs;
class HoleHardcoder;

class File;
class SketchFunction;

namespace SolverLanguagePrimitives{
    class SolutionHolder;
    class InputHolder;
};

//using namespace SolverLanguagePrimitives;

namespace SL {

    class VarVal;

    class Name {
        bool defined = false;
        string name;
    public:
        explicit Name(string _name) : name(_name), defined(true) {};
        explicit Name(Name* to_copy) : name(to_copy->to_string()), defined(to_copy->defined) { assert(defined); };

        string to_string() const {
            assert(defined);
            return name;
        }

        void clear()
        {
            assert(defined == 1);
            name.clear();
            assert(defined == 1);
            delete this;
        }

        ~Name()
        {
            assert(defined == 1);
            defined = false;
        }

        SL::VarVal* eval(SolverProgramState *state);

        bool operator<(const Name &other) const {
            assert(defined);
            return name < other.name;
        }

        bool operator==(const Name &other) const {
            assert(defined);
            return name == other.name;
        }

        bool is_defined();
    };

    class SLType;

    template<typename Head>
    class LinkedList
    {
        Head* head = nullptr;
        LinkedList* rest = nullptr;
    public:
        explicit LinkedList(Head* _head): head(_head) {}
        LinkedList(Head* _head, LinkedList* _rest): head(_head), rest(_rest) {}

        void populate_vector(vector<Head*>& ret)
        {
            LinkedList<Head>* at = this;
            while(at != nullptr)
            {
                ret.push_back(at->head);
                at = at->rest;
            }
        }

        bool operator < (const LinkedList& other) const
        {
            assert(head != nullptr && other.head != nullptr);
            if(*head < *other.head)
            {
                return true;
            }
            else if(*other.head < *head)
            {
                return false;
            }
            if(rest == nullptr && other.rest == nullptr)
            {
                return false;
            }
            else if(rest != nullptr && other.rest == nullptr){
                return true;
            }
            else if(rest == nullptr && other.rest != nullptr)
            {
                return false;
            }
            else
            {
                return *rest < *other.rest;
            }
        }

        bool operator == (const LinkedList& other) const
        {
            assert(head != nullptr && other.head != nullptr);
            if(!(*head == *other.head))
            {
                return false;
            }
            if(rest == nullptr && other.rest == nullptr)
            {
                return true;
            }
            else if(rest != nullptr && other.rest == nullptr){
                return false;
            }
            else if(rest == nullptr && other.rest != nullptr)
            {
                return false;
            }
            else {
                return *rest == *other.rest;
            }
        }
        string to_string()
        {
            return head->to_string() + (rest == nullptr ? "" : ", " + rest->to_string());
        }
    };

    class TypeParams: public LinkedList<SLType>
    {
    public:
        explicit TypeParams(SLType* _head): LinkedList<SLType>(_head) {}
        TypeParams(SLType* _head, TypeParams* _rest): LinkedList<SLType>(_head, _rest) {}
    };

    const bool is_strongly_typed = false;

    class PolyType;

    class SLType
    {
        Name* name = nullptr;
        PolyType* type_params = nullptr;
    public:
        explicit SLType(Name* _name): name(_name) {assert(name != nullptr);}
        SLType(Name* _name, TypeParams* _type_params);
        explicit SLType(SLType* to_copy);
//        {
//            if(to_copy->type_params != nullptr) {
//                type_params = new vector<SLType *>();
//                for (auto & type_param : *to_copy->type_params) {
//                    type_params->push_back(new SL::SLType(type_param));
//                }
//            }
//        }

        void clear();

        string to_string();

        bool operator < (const SLType& other) const;

        bool operator == (const SLType& other) const;

        Name *get_head() {
            assert(name != nullptr);
            return name;
        }

        PolyType* get_type_params() {
            assert(name != nullptr);
            return type_params;
        }

        bool is_simple_type();

        bool is_defined() {
            return name != nullptr;
        }

        bool is_any() {
            assert(is_defined());
            return name->to_string() == "any";
        }
    };

    class Var {
        SLType *type = nullptr;
        Name *name = nullptr;
    public:

        Var(SLType *_type, Name *_name) : type(_type), name(_name) {}
        Var(Name *_type, Name *_name) : type(new SL::SLType(_type)), name(_name) {}
        explicit Var(Var* to_copy): type(new SL::SLType(to_copy->type)), name(new SL::Name(to_copy->name)) {}

        Name *get_name() {
            return name;
        }

        void clear()
        {
            type->clear();
            name->clear();
            delete this;
        }

        string to_string() const {
            return name->to_string() + " : " + type->to_string();
        }

        void run(SolverProgramState *state);

        bool defined() const
        {
            return type != nullptr && name != nullptr;
        }

        bool operator == (const Var& other) const
        {
            if(is_strongly_typed) {
                return *type == *other.type && *name == *other.name;
            }
            else
            {
                return *name == *other.name;
            }
        }

        bool operator < (const Var& other) const
        {
            assert(defined());
            assert(other.defined());


            if(is_strongly_typed) {
                if (*type < *other.type) {
                    return true;
                } else if (*other.type < *type) {
                    return false;
                }
            }

            return *name < *other.name;
        }

        SL::VarVal*eval(SolverProgramState *pState);

        SLType * get_type();

        bool has_type()
        {
            return type->is_defined();
        }

        bool has_any_type()
        {
            return type->is_any();
        }

    };

    vector<SL::SLType*>* copy_type_params(vector<SLType*>* to_copy);

    class PolyType
    {
        vector<SL::SLType*>* type_params = nullptr;
    public:
        explicit PolyType(TypeParams* _type_params){
            type_params = new vector<SL::SLType*>();
            _type_params->populate_vector(*type_params);
        }
        explicit PolyType(vector<SLType*>* _type_params): type_params(_type_params) {}
        explicit PolyType(PolyType* to_copy): type_params(copy_type_params(to_copy->type_params)) {};

        SL::SLType* at(int idx)
        {
            assert(type_params != nullptr);
            assert(0 <= idx && idx < size());
            return type_params->at(idx);
        }

        int size()
        {
            assert(type_params != nullptr);
            if(type_params == nullptr)
            {
                return 0;
            }
            return type_params->size();
        }

        const vector<SLType*>* get_type_params()
        {
            return type_params;
        }

        string to_string()
        {
            string str;
            for(int i = 0;i<type_params->size();i++)
            {
                if(i >= 1)
                {
                    str+= ", ";
                }
                str += type_params->at(i)->to_string();
            }
            return str;
        }

        virtual void clear()
        {
            for(auto & it: *type_params)
            {
                it->clear();
                it = nullptr;
            }
            type_params->clear();
            delete type_params;
            type_params = nullptr;
        }

        bool operator == (const PolyType& other) const
        {
            if(type_params->size() != other.type_params->size())
            {
                return false;
            }
            assert(type_params->size() == other.type_params->size());
            for(int i = 0;i<type_params->size();i++)
            {
                if(!(*type_params->at(i) == *other.type_params->at(i)))
                {
                    return false;
                }
            }
            return true;
        }

        bool operator < (const PolyType& other) const
        {
            assert(is_strongly_typed);
            if(type_params->size() < other.type_params->size())
            {
                return true;
            }
            else if(type_params->size() > other.type_params->size())
            {
                return false;
            }
            assert(type_params->size() == other.type_params->size());
            for(int i = 0;i<type_params->size();i++)
            {
                if(*type_params->at(i) < *other.type_params->at(i))
                {
                    return true;
                }
                else if( *other.type_params->at(i) < *type_params->at(i))
                {
                    return false;
                }
            }
            return false;
        }
    };

    class PolyVec: public PolyType, private vector<SL::VarVal*>
    {
    public:
        explicit PolyVec(PolyType* _type_params): PolyType(_type_params){
            assert(get_type_params()->size() == 1);
        }
        explicit PolyVec(PolyVec* to_copy);

        void emplace_back(SL::VarVal* new_element);

        void sort();

        void clear() override;

        size_t size(){
            return vector<SL::VarVal*>::size();
        }

        SL::VarVal* at(int idx)
        {
            assert(idx >= 0 && idx < size());
            return vector<SL::VarVal*>::at(idx);
        }

        void reverse();
    };

    class PolyPair: public PolyType, private pair<SL::VarVal*, SL::VarVal*>
    {
    public:
        explicit PolyPair(PolyType* _type_params, SL::VarVal* left, SL::VarVal* right);

        explicit PolyPair(PolyPair* to_copy);

        SL::VarVal* first() const {
            return pair<SL::VarVal*, SL::VarVal*>::first;
        }
        SL::VarVal* second() const {
            return pair<SL::VarVal*, SL::VarVal*>::second;
        }

        void clear() override;

        bool operator < (const PolyPair& other) const;
    };

    class Method;

    enum VarValType {
        string_val_type, int_val_type, file_val_type,
        method_val_type, skfunc_val_type, solution_val_type,
        input_val_type, bool_val_type, void_val_type,
        float_val_type, poly_vec_type,
        poly_pair_type,
        no_type};

    template <typename T>
    VarValType get_var_val_type(T val)
    {
        if(std::is_same<bool,T>::value)
        {
            return bool_val_type;
        }
        else if(std::is_same<int,T>::value)
        {
            return int_val_type;
        }
        else {
            assert(false);
        }
    }

    class VarVal{
        union {
            Name* s;
            int i;
            bool b;
            float float_val;
            File* file;
            Method* method;
            SketchFunction* skfunc;
            SolverLanguagePrimitives::SolutionHolder* solution;
            SolverLanguagePrimitives::InputHolder* input_holder;
            PolyVec* poly_vec;
            PolyPair* poly_pair;
        };
        const VarValType var_val_type;
        int num_shared_ptr = 0;
    public:
        explicit VarVal(string  _s);
        template <typename T>
        explicit VarVal(T val);
        explicit VarVal(float _float_val);
        explicit VarVal(File* _file);
        explicit VarVal(Method* _method);
        explicit VarVal(SketchFunction* _harness);
        explicit VarVal(PolyVec* _poly_vec);
        explicit VarVal(PolyPair* _poly_pair);
        explicit VarVal(SolverLanguagePrimitives::SolutionHolder* _solution);
        explicit VarVal(SolverLanguagePrimitives::InputHolder* _input_holder);
        explicit VarVal(VarVal* _to_copy);

        VarVal(): var_val_type(void_val_type) {}

        int get_num_shared_ptr()
        {
            return num_shared_ptr;
        }

        string get_type_string()
        {
            switch (var_val_type) {

                case string_val_type:
                    return "string";
                    break;
                case int_val_type:
                    return "int";
                    break;
                case file_val_type:
                    return "File";
                    break;
                case method_val_type:
                    assert(false);
                    break;
                case skfunc_val_type:
                    return "SketchFunction";
                    break;
                case solution_val_type:
                    return "Solution";
                    break;
                case input_val_type:
                    return "InputHolder";
                    break;
                case bool_val_type:
                    return "bool";
                    break;
                case void_val_type:
                    return "void";
                    break;
                case float_val_type:
                    return "float";
                    break;
                case poly_vec_type:
                    return "vector";
                    break;
                case poly_pair_type:
                    return "pair";
                    break;
                case no_type:
                    assert(false);
                    break;
                default:
                    assert(false);
            }
            assert(false);
        }

        bool operator < (const VarVal& other) const
        {
            switch (var_val_type) {

                case string_val_type:
                    return s < other.s;
                    break;
                case int_val_type:
                    return i < other.i;
                    break;
                case file_val_type:
                    return *file < *other.file;
                    break;
                case method_val_type:
                    assert(false);
//                    return *method < *other.method;
                    break;
                case skfunc_val_type:
                    assert(false);
//                    return *skfunc < *other.skfunc;
                    break;
                case solution_val_type:
                    return *solution < *other.solution;
                    break;
                case input_val_type:
                    assert(false);
//                    return *input_holder < *other.input_holder;
                    break;
                case bool_val_type:
                    return b < other.b;
                    break;
                case void_val_type:
                    return false;
                    break;
                case float_val_type:
                    return float_val < other.float_val;
                    break;
                case poly_vec_type:
                    assert(false);
//                    return *poly_vec < *other.poly_vec;
                    break;
                case poly_pair_type:
                    return *poly_pair < *other.poly_pair;
                    break;
                case no_type:
                    assert(false);
                    break;
                default:
                    assert(false);
            }
            assert(false);
        }

        VarValType get_type()
        {
            return var_val_type;
        }

        template<typename T>
        void assert_type_invariant()
        {
            if(std::is_same<bool,T>::value){
                assert(var_val_type == bool_val_type);
            }
            else if(std::is_same<int,T>::value){
                assert(var_val_type == int_val_type);
            }
            else if(std::is_same<Method*,T>::value){
                assert(var_val_type == method_val_type);
            }
            else if(std::is_same<string,T>::value){
                assert(var_val_type == string_val_type);
            }
            else if(std::is_same<Name*,T>::value){
                assert(var_val_type == string_val_type);
            }
            else if(std::is_same<SketchFunction*,T>::value){
                assert(is_sketch_function());
            }
            else if(std::is_same<File*,T>::value){
                assert(var_val_type == file_val_type);
            }
            else if(std::is_same<SolverLanguagePrimitives::SolutionHolder *,T>::value){
                assert(var_val_type == solution_val_type);
            }
            else if(std::is_same<SolverLanguagePrimitives::InputHolder *,T>::value){
                assert(var_val_type == input_val_type);
            }
            else if(std::is_same<float,T>::value){
                assert(var_val_type == float_val_type);
            }
            else if(std::is_same<PolyPair*,T>::value){
                assert(var_val_type == poly_pair_type);
            }
            else if(std::is_same<PolyVec*,T>::value){
                assert(var_val_type == poly_vec_type);
            }
            else {
                assert(false);
            }
        }

        template<typename T>
        T get(T ret, bool do_count, bool do_assert)
        {
            assert_type_invariant<T>();

            if(do_count) {
                if(do_assert) {
                    assert(get_num_shared_ptr() >= 1);
                }
                increment_shared_ptr();
                decrement_shared_ptr();
            }
            return ret;
        }

        bool get_bool(bool do_count = true, bool do_assert = true)
        {
            assert(var_val_type == bool_val_type);
            return get<bool>(b, do_count, do_assert);
        }

        int get_int(bool do_count = true, bool do_assert = true) {
            if(var_val_type == int_val_type) {
                return get<int>(i, do_count, do_assert);
            }
            else if(var_val_type == bool_val_type)
            {
                return get<bool>(b, do_count, do_assert);
            }
            else
            {
                assert(false);
            }
        }

        Method *get_method(bool do_count = true, bool do_assert = true) {
            assert(is_method());
            assert(do_assert);
            return get<Method *>(method, do_count, do_assert);
        }

        bool is_sketch_function()
        {
            return var_val_type == skfunc_val_type;
        }

        bool is_method() {
            return var_val_type == method_val_type;
        }

        string get_string(bool do_count = true, bool do_assert = true) {
            assert(var_val_type == string_val_type);
            return get<string>(s->to_string(), do_count, do_assert);
        }

        SketchFunction *get_harness(bool do_count = true, bool do_assert = true) {
            return get_function(do_count, do_assert);
        }

        File *get_file(bool do_count = true, bool do_assert = true) {
            assert(var_val_type == file_val_type);
            assert(do_assert);
            return get<File *>(file, do_count, do_assert);
        }

        SolverLanguagePrimitives::SolutionHolder *get_solution(bool do_count = true, bool do_assert = true) {
            assert(var_val_type == solution_val_type);
            assert(do_assert);
            return get<SolverLanguagePrimitives::SolutionHolder *>(solution, do_count, do_assert);
        }

        SketchFunction *get_function(bool do_count = true, bool do_assert = true) {
            assert(is_sketch_function());
            assert(do_assert);
            return get<SketchFunction *>(skfunc, do_count, do_assert);
        }

        SolverLanguagePrimitives::InputHolder *get_input_holder(bool do_count = true, bool do_assert = true) {
            assert(var_val_type == input_val_type);
            assert(do_assert);
            return get<SolverLanguagePrimitives::InputHolder *>(input_holder, do_count, do_assert);
        }

        bool is_void(){
            return var_val_type == void_val_type;
        }

        float get_float(bool do_count = true, bool do_assert = true) {
            assert(var_val_type == float_val_type);
            return get<float>(float_val, do_count, do_assert);
        }


        PolyVec *get_poly_vec(bool do_count = true, bool do_assert = true) {
            assert(var_val_type == poly_vec_type);
            assert(do_assert);
            return get<PolyVec *>(poly_vec, do_count, do_assert);
        }

        PolyPair *get_poly_pair(bool do_count = true, bool do_assert = true) {
            assert(var_val_type == poly_pair_type);
            assert(do_assert);
            return get<PolyPair *>(poly_pair, do_count, do_assert);
        }

        string to_string(bool do_count, bool do_assert)
        {
            switch (var_val_type) {

                case string_val_type:
                    return get_string(do_count, do_assert);
                    break;
                case int_val_type:
                    return std::to_string(get_int(do_count, do_assert));
                    break;
                case file_val_type:
                    assert(false);
                    break;
                case method_val_type:
                    assert(false);
                    break;
                case skfunc_val_type:
                    assert(false);
                    break;
                case solution_val_type:
                    assert(false);
                    break;
                case input_val_type:
                    assert(false);
                    break;
                case bool_val_type:
                    assert(false);
                    break;
                case void_val_type:
                    assert(false);
                    break;
                case no_type:
                    assert(false);
                    break;
                case float_val_type:
                    return std::to_string(get_float(do_count, do_assert));
                    break;
                case poly_pair_type:
                    assert(false);
                    break;
                case poly_vec_type:
                    assert(false);
                    break;
                default:
                    assert(false);
            }
        }

        void decrement_shared_ptr()
        {
            assert(num_shared_ptr >= 1);
            num_shared_ptr --;

            if (num_shared_ptr == 0) {
                dealloc();
            }
        }



        void force_clear()
        {
            clear();
        }
    private:
        template<typename T>
        void clear(T& val, bool do_delete = true)
        {
            assert_type_invariant<T>();
            if(val != nullptr) {
                val->clear();
                if(do_delete) {
                    delete val;
                }
                val = nullptr;
            }
        }
        void dealloc()
        {
            assert(num_shared_ptr == 0);
            clear();
            delete this;
        }
        void clear() {

            switch (var_val_type) {
                case string_val_type:
                    clear<Name*>(s, false);
                    break;
                case int_val_type:
                    //do nothing
                    break;
                case file_val_type:
                    clear<File*>(file);
                    break;
                case method_val_type:
                    clear<Method*>(method);
                    break;
                case skfunc_val_type:
                    clear<SketchFunction*>(skfunc, false);
                    break;
                case solution_val_type:
                    clear<SolverLanguagePrimitives::SolutionHolder*>(solution);
                    break;
                case input_val_type:
                    clear<SolverLanguagePrimitives::InputHolder*>(input_holder);
                    break;
                case bool_val_type:
                    //do nothing
                    break;
                case void_val_type:
                    //do nothing
                    break;
                    break;
                case no_type:
                    assert(false);
                    break;
                case float_val_type:
                    //do nothing
                    break;
                case poly_pair_type:
                    clear<SL::PolyPair*>(poly_pair);
                    break;
                case poly_vec_type:
                    clear<SL::PolyVec*>(poly_vec);
                    break;
                default:
                    assert(false);
            }
        }
    public:
        void increment_shared_ptr();

        void complete_return()
        {
            assert(is_return);
            num_shared_ptr--;
            assert(num_shared_ptr >= 0);
            is_return = false;
        }


    private:
            bool is_return = false;
    public:
        void set_return();

        bool get_is_return() const;
    };

    class Param;

    class Params {
        Param *head = nullptr;
        Params *rest = nullptr;
    public:
        Params() = default;
        Params(Param *_head, Params *_rest = nullptr) : head(_head), rest(_rest) {};
        Params(Param *_head, Param *_next) : head(_head), rest(new Params(_next)) {};
        void populate_vector(vector<Param*>& params)
        {
            Params* at = this;
            while(at != nullptr)
            {
                if(at->head != nullptr) {
                    params.emplace_back(at->head);
                }
                at = at->rest;
            }
        }
    };

    class Expression;

    class FuncCall {
        Expression *expression = nullptr;
        union {
            Name *method_name;
            SLType *type_constructor;
        };
        vector<Param*> params;

        enum MethodMetaType {name_meta_type, type_constructor_meta_type};

        MethodMetaType method_meta_type;

    public:
        FuncCall(
                Expression *_expression, Name *_method_name, Params *_params) :
                expression(_expression), method_name(_method_name), method_meta_type(name_meta_type) {
            _params->populate_vector(params);
        };
        FuncCall(
                Name *_method_name, Params *_params) :
                method_name(_method_name), method_meta_type(name_meta_type) {
            _params->populate_vector(params);
        };
        FuncCall(
                SLType *_type_constructor, Params *_params){
            if(_type_constructor->is_simple_type())
            {
                method_name = _type_constructor->get_head();
                method_meta_type = name_meta_type;
            }
            else
            {
                type_constructor = _type_constructor;
                method_meta_type = type_constructor_meta_type;
            }
            _params->populate_vector(params);
        };
        explicit FuncCall(FuncCall* to_copy);

        pair<Var*, SL::VarVal*> get_var_assert_type(SolverProgramState* state, const string& type_name);

        SL::VarVal* eval(SolverProgramState* state);

        void run(SolverProgramState *pState);

        SL::VarVal*eval_type_constructor(SolverProgramState* state);

        void clear();
    };

    class Assignment
    {
        union {
            Var* dest_var;
            Name* dest_name;
        };

        Expression* expression = nullptr;

        enum DestMetaType {var_dest_type, name_dest_type, no_dest_type};

        DestMetaType dest_type = no_dest_type;

    public:

        explicit Assignment(Var* _var): dest_var(_var), dest_type(var_dest_type){}
        Assignment(Var* _var, Expression* _expression): dest_var(_var), expression(_expression), dest_type(var_dest_type) {}
        Assignment(Name* _name, Expression* _expression): dest_name(_name), expression(_expression), dest_type(name_dest_type) {}
        explicit Assignment(Assignment* to_copy);

        Var* get_var() const
        {
            assert(dest_type == var_dest_type);
            return dest_var;
        }
        void run(SolverProgramState* state);

        bool has_assignment();

        void clear();
    };

    class Param{
        union {
            Expression *expression;
            Var* var;
        };
        enum ParamMetaType {is_var, is_expression};
        ParamMetaType meta_type;
    public:
        explicit Param(Expression *_expression) : expression(_expression), meta_type(is_expression) {};
        explicit Param(Var *_var) : var(_var), meta_type(is_var) {};
        explicit Param(Assignment *assignment) : var(assignment->get_var()), meta_type(is_var) {
            assert(!assignment->has_assignment());
        };
        explicit Param(Param* to_copy);

        SL::VarVal*eval(SolverProgramState *state);

        Var *get_var() {
            switch (meta_type) {
                case is_expression:
                    assert(false);
//                    return state->get_var_val(state->name_to_var(name));
                    break;
                case is_var:
                    return var;
                    break;
                default:
                    assert(false);
            }
        }

        void clear();

    };

    class Predicate;

    class Expression
    {
        union
        {
            Predicate* predicate;
            FuncCall* func_call;
            Name* identifier;
            SL::VarVal* var_val;
        };
        enum ExpressionMetaType {predicate_meta_type, func_call_meta_type, identifier_meta_type, var_val_meta_type, no_meta_type};
        ExpressionMetaType expression_meta_type = no_meta_type;
    public:
        explicit Expression(Predicate* _predicate): predicate(_predicate), expression_meta_type(predicate_meta_type){}
        explicit Expression(FuncCall* _func_call): func_call(_func_call), expression_meta_type(func_call_meta_type){}
        explicit Expression(Name* _identifier): identifier(_identifier), expression_meta_type(identifier_meta_type){}
        explicit Expression(SL::VarVal* _var_val): var_val(_var_val), expression_meta_type(var_val_meta_type){};
        explicit Expression(Expression* to_copy);

        SL::VarVal* eval(SolverProgramState* state);

        void run(SolverProgramState* state) {
            SL::VarVal* ret = eval(state);
            if(!ret->is_void())
            {
                cout << "ERROR: " << "Expression returns but result not stored." << endl;
                assert(false);
            }
            ret->increment_shared_ptr();
            ret->decrement_shared_ptr();
        }

        Name* get_var_name()
        {
            switch (expression_meta_type) {
                case identifier_meta_type:
                    return identifier;
                    break;
                case func_call_meta_type:
                    return nullptr;
                    break;
                default:
                    assert(false);
            }
        }

        void clear();

    };

    enum MyOperator {lt, gt, eq};

    class Predicate
    {
        Expression* left_operand = nullptr;
        Expression* right_operand = nullptr;
    private:
        MyOperator op;
    public:
        Predicate(MyOperator _op,
                           Expression* _left,
                           Expression* _right): op(_op), left_operand(_left), right_operand(_right) {};
        explicit Predicate(SL::Predicate* to_copy);

        bool eval(SolverProgramState* state)
        {
            int left_val = left_operand->eval(state)->get_int();
            int right_val = right_operand->eval(state)->get_int();
            switch (op) {
                case lt:
                    return left_val < right_val;
                    break;
                case gt:
                    return left_val > right_val;
                    break;
                case eq:
                    return left_val == right_val;
                    break;
                default:
                    assert(false);
            }
        }

        void clear()
        {
            left_operand->clear();
            right_operand->clear();
            delete this;
        }
    };
    
    class CodeBlock;

    class While
    {
        Expression* expression = nullptr;
        CodeBlock* body = nullptr;
    public:
        While(Expression* _expression, CodeBlock* _body): expression(_expression), body(_body) {}
        explicit While(While* to_copy);

        void run(SolverProgramState* state);

        void clear();
    };

    class UnitLine;

    class For
    {
        UnitLine* def = nullptr;
        Expression* expression = nullptr;
        UnitLine* plus_plus = nullptr;
        CodeBlock* body = nullptr;
    public:
        For(UnitLine* _def, Expression* _expression, UnitLine* _plus_plus, CodeBlock* _body):
            def(_def), expression(_expression), plus_plus(_plus_plus), body(_body) {}
        explicit For(For* to_copy);

        void run(SolverProgramState* state);

        void clear();
    };


    class If
    {
        Expression* expression = nullptr;
        CodeBlock* body = nullptr;
    public:
        If(Expression* _expression, CodeBlock* _body): expression(_expression), body(_body) {}
        explicit If(If* to_copy);

        void run(SolverProgramState* state);

        void clear();
    };

    class Return
    {
        Expression* expression = nullptr;
    public:
        explicit Return(Expression* _expression) : expression(_expression) {}
        explicit Return(Return* to_copy): expression(new Expression(to_copy->expression)){};

        void run(SolverProgramState* state);

        void clear();
    };

    class UnitLine
    {
        enum LineType {var_line, assign_line, while_line, if_line, return_line, expression_line, for_line, code_block_line};
        union {
            Var* var;
            Assignment* assignment;
            While* while_loop;
            For* for_loop;
            If* if_block;
            Return* return_stmt;
            Expression* expression;
            CodeBlock* code_block;
        };
        LineType line_type;
    public:
        explicit UnitLine(Var* _var): var(_var), line_type(var_line){}
        explicit UnitLine(Assignment* _assignment): assignment(_assignment), line_type(assign_line){}
        explicit UnitLine(While* _while_loop): while_loop(_while_loop), line_type(while_line){}
        explicit UnitLine(If* _if_block): if_block(_if_block), line_type(if_line){}
        explicit UnitLine(Return* _return_stmt): return_stmt(_return_stmt), line_type(return_line){}
        explicit UnitLine(Expression* _expression): expression(_expression), line_type(expression_line){}
        explicit UnitLine(For* _for): for_loop(_for), line_type(for_line){}
        explicit UnitLine(CodeBlock* _code_block): code_block(_code_block), line_type(code_block_line){}
        explicit UnitLine(UnitLine* to_copy);

        void run(SolverProgramState *state);

        void clear();
    };

    class CodeBlock
    {
        UnitLine* head = nullptr;
        CodeBlock* rest = nullptr;
    public:
        explicit CodeBlock(UnitLine* _head): head(_head) {}
        CodeBlock(UnitLine* _head, CodeBlock* _rest): head(_head), rest(_rest) {}
        explicit CodeBlock(CodeBlock* to_copy): head(new UnitLine(to_copy->head))
        {
            if(to_copy->rest != nullptr){
                rest = new CodeBlock(to_copy->rest);
            }
        }

        void run(SolverProgramState *state);

        void clear()
        {
            head->clear();
            head = nullptr;
            if(rest != nullptr) {
                rest->clear();
                rest = nullptr;
            }
            delete this;
        }
    };

    class Method
    {
        CodeBlock* body = nullptr;
        Var* var = nullptr;
        vector<Param*> params;

    public:
        Method(Var* _var, Params* _params, CodeBlock* _body): var(_var), body(_body)
        {
            _params->populate_vector(params);
        }
        explicit Method(Method* to_copy): var(new Var(to_copy->var)), body(new CodeBlock(to_copy->body))
        {
            for(auto it: to_copy->params)
            {
                params.push_back(new Param(it));
            }
        }

        void clear();

        void run(SolverProgramState* state, vector<Param*>& input_params);

        SL::VarVal*eval(SolverProgramState* state, vector<Param*>& params);

        Var* get_var()
        {
            return var;
        }
    };

    bool var_val_invariant(SL::SLType *var_type, SL::VarVal*var_val);
    bool var_val_invariant(SL::Var *var, SL::VarVal*var_val);

    class Methods
    {
        Method* head = nullptr;
        Methods* rest = nullptr;
    public:
        explicit Methods(Method* _head): head(_head) {}
        Methods(Method* _head, Methods* _rest): head(_head), rest(_rest) {}

        void populate_state(Frame& frame);
    };
};


typedef void* yyscan_t;
void yyerror(yyscan_t scanner, SolverProgramState* state, string s);


//using namespace SL;

#endif //SOLVERLANGUAGEPARSER_LEXANDYACCHEADER_H
