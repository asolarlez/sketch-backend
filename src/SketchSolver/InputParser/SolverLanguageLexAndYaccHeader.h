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
    class HoleAssignment;
    class InputAssignment;
};

//using namespace SolverLanguagePrimitives;

namespace SL {

    class VarVal;

    class Identifier {
        bool defined = false;
        string identifier;
    public:
        explicit Identifier(string _name) : identifier(std::move(_name)), defined(true) {};
        explicit Identifier(Identifier* to_copy) : identifier(to_copy->to_string()), defined(to_copy->defined) { assert(defined); };

        string to_string() const {
            assert(defined);
            return identifier;
        }

        void clear()
        {
            assert(defined == 1);
            identifier.clear();
            assert(defined == 1);
            delete this;
        }

        ~Identifier()
        {
            assert(defined == 1);
            defined = false;
        }

        SL::VarVal* eval(SolverProgramState *state);

        bool operator<(const Identifier &other) const {
            assert(defined);
            return identifier < other.identifier;
        }

        bool operator==(const Identifier &other) const {
            assert(defined);
            return identifier == other.identifier;
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
            assert(ret.size() == 0);
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
        Identifier* name = nullptr;
        PolyType* type_params = nullptr;
    public:
        explicit SLType(Identifier* _name): name(_name) {assert(name != nullptr);}
        SLType(Identifier* _name, TypeParams* _type_params);
        explicit SLType(SLType* to_copy);

        void clear();

        string to_string();

        bool operator < (const SLType& other) const;

        bool operator == (const SLType& other) const;

        Identifier *get_head() {
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
        Identifier *name = nullptr;
    public:

        Var(SLType *_type, Identifier *_name) : type(_type), name(_name) {}
        Var(Identifier *_type, Identifier *_name) : type(new SL::SLType(_type)), name(_name) {}
        explicit Var(Var* to_copy): type(new SL::SLType(to_copy->type)), name(new SL::Identifier(to_copy->name)) {}

        Identifier *get_name() {
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

        bool accepts_type(string type_name)
        {
            if(!has_type() || has_any_type()) {
                return true;
            }
            assert(get_type()->is_simple_type());
            return get_type()->get_head()->to_string() == type_name;
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

    class FunctionCall;

    class VarVal{

        union {
            Identifier* s;
            int i;
            bool b;
            float float_val;
            File* file;
            Method* method;
            SketchFunction* skfunc;
            SolverLanguagePrimitives::HoleAssignment* solution;
            SolverLanguagePrimitives::InputAssignment* input_holder;
            PolyVec* poly_vec;
            PolyPair* poly_pair;
        };
        const VarValType var_val_type;
        int num_shared_ptr = 0;
        map<string, VarVal*> is_responsible_for;
    public:
        explicit VarVal(string  _s);
//        explicit VarVal(int val);
        template <typename T>
        explicit VarVal(T val);
        explicit VarVal(float _float_val);
        explicit VarVal(File* _file);
        explicit VarVal(Method* _method);
        explicit VarVal(SketchFunction* _harness);
        explicit VarVal(PolyVec* _poly_vec);
        explicit VarVal(PolyPair* _poly_pair);
        explicit VarVal(SolverLanguagePrimitives::HoleAssignment* _solution);
        explicit VarVal(SolverLanguagePrimitives::InputAssignment* _input_holder);
        explicit VarVal(VarVal* _to_copy);

        VarVal(): var_val_type(void_val_type) {}

        VarVal* eq_op (VarVal* other)
        {
            assert(var_val_type == other->var_val_type);
            switch (var_val_type) {
                case int_val_type:
                    return new VarVal((bool)(get_int(true, false) == other->get_int(true, false)));
                    break;
                case bool_val_type:
                    return new VarVal((bool)(get_bool(true, false) == other->get_bool(true, false)));
                    break;
                default:
                    assert(false);
            }
            assert(false);
        }


        VarVal* lt_op (VarVal* other)
        {
            assert(var_val_type == other->var_val_type);
            switch (var_val_type) {
                case int_val_type:
                    return new VarVal((bool)(get_int(true, false) < other->get_int(true, false)));
                    break;
                case float_val_type:
                    return new VarVal((bool)(get_float(true, false) < other->get_float(true, false)));
                    break;
                default:
                    assert(false);
            }
            assert(false);
        }

        VarVal* gt_op (VarVal* other)
        {
            assert(var_val_type == other->var_val_type);
            switch (var_val_type) {
                case int_val_type:
                    return new VarVal((bool)(get_int(true, false) > other->get_int(true, false)));
                    break;
                case float_val_type:
                    return new VarVal((bool)(get_float(true, false) > other->get_float(true, false)));
                    break;
                default:
                    assert(false);
            }
            assert(false);
        }

        VarVal* geq_op (VarVal* other)
        {
            assert(var_val_type == other->var_val_type);
            switch (var_val_type) {
                case int_val_type:
                    return new VarVal((bool)(get_int(true, false) >= other->get_int(true, false)));
                    break;
                case float_val_type:
                    return new VarVal((bool)(get_float(true, false) >= other->get_float(true, false)));
                    break;
                default:
                    assert(false);
            }
            assert(false);
        }

        VarVal* plus_op(VarVal* other)
        {
            assert(var_val_type == other->var_val_type);
            switch (var_val_type) {
                case int_val_type:
                    return new VarVal((int)(get_int(true, false) + other->get_int(true, false)));
                    break;
                case float_val_type:
                    return new VarVal((float)(get_float(true, false) + other->get_float(true, false)));
                    break;
                default:
                    assert(false);
            }
            assert(false);
        }

        VarVal* minus_op(VarVal* other)
        {
            assert(var_val_type == other->var_val_type);
            switch (var_val_type) {
                case int_val_type:
                    return new VarVal((int)(get_int(true, false) - other->get_int(true, false)));
                    break;
                case float_val_type:
                    return new VarVal((float)(get_float(true, false) - other->get_float(true, false)));
                    break;
                default:
                    assert(false);
            }
            assert(false);
        }

        VarVal* mult_op(VarVal* other)
        {
            assert(var_val_type == other->var_val_type);
            switch (var_val_type) {
                case int_val_type:
                    return new VarVal((int)(get_int(true, false) * other->get_int(true, false)));
                    break;
                case float_val_type:
                    return new VarVal((float)(get_float(true, false) * other->get_float(true, false)));
                    break;
                default:
                    assert(false);
            }
            assert(false);
        }

        VarVal* div_op(VarVal* other)
        {
            assert(var_val_type == other->var_val_type);
            switch (var_val_type) {
                case int_val_type:
                    return new VarVal((int)(get_int(true, false) / other->get_int(true, false)));
                    break;
                case float_val_type:
                    return new VarVal((float)(get_float(true, false) / other->get_float(true, false)));
                    break;
                default:
                    assert(false);
            }
            assert(false);
        }

        int get_num_shared_ptr() const
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
                    return "InputAssignment";
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
            assert(var_val_type == other.var_val_type);
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
                    //WARNING: all skfuncs are essentially equal.
                    //TODO: should implement a meaningful definition of < for skfunc/boolean dag
                    return false;
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
            else if(std::is_same<Identifier*,T>::value){
                assert(var_val_type == string_val_type);
            }
            else if(std::is_same<SketchFunction*,T>::value){
                assert(is_sketch_function());
            }
            else if(std::is_same<File*,T>::value){
                assert(var_val_type == file_val_type);
            }
            else if(std::is_same<SolverLanguagePrimitives::HoleAssignment *,T>::value){
                assert(var_val_type == solution_val_type);
            }
            else if(std::is_same<SolverLanguagePrimitives::InputAssignment *,T>::value){
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

        File *get_file(bool do_count = true, bool do_assert = true) {
            assert(var_val_type == file_val_type);
            assert(do_assert);
            return get<File *>(file, do_count, do_assert);
        }

        SolverLanguagePrimitives::HoleAssignment *get_solution(bool do_count = true, bool do_assert = true) {
            assert(var_val_type == solution_val_type);
            if(do_count) {
                assert(do_assert);
            }
            return get<SolverLanguagePrimitives::HoleAssignment *>(solution, do_count, do_assert);
        }

        SketchFunction *get_function(bool do_count = true, bool do_assert = true) {
            assert(is_sketch_function());
            assert(do_assert);
            return get<SketchFunction *>(skfunc, do_count, do_assert);
        }

        SolverLanguagePrimitives::InputAssignment *get_input_holder(bool do_count = true, bool do_assert = true) {
            assert(var_val_type == input_val_type);
            assert(do_assert);
            return get<SolverLanguagePrimitives::InputAssignment *>(input_holder, do_count, do_assert);
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
                    return std::to_string(get_bool(do_count, do_assert));
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

        bool decrement_shared_ptr()
        {
            assert(num_shared_ptr >= 1);
            num_shared_ptr --;

            if (num_shared_ptr == 0) {
                _clear();
                return true;
            }
            else
            {
                return false;
            }
        }

    private:
        template<typename T>
        void clear(T & val, bool do_delete = true)
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
        template<typename T>
        VarVal* eval(T& val, SolverProgramState* state, FunctionCall* function_call);

        void _clear() {
            assert(num_shared_ptr == 0);
            switch (var_val_type) {
                case string_val_type:
                    clear<Identifier*>(s, false);
                    break;
                case int_val_type:
                    //do nothing
                    break;
                case file_val_type:
                    clear<File*>(file, false);
                    break;
                case method_val_type:
                    clear<Method*>(method);
                    break;
                case skfunc_val_type:
                    clear<SketchFunction*>(skfunc, false);
                    break;
                case solution_val_type:
                    clear<SolverLanguagePrimitives::HoleAssignment*>(solution);
                    break;
                case input_val_type:
                    clear<SolverLanguagePrimitives::InputAssignment*>(input_holder);
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
            for(auto it : is_responsible_for) {
                it.second->decrement_shared_ptr();
            }
            is_responsible_for.clear();
            delete this;
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

        VarVal *eval(SolverProgramState *pState, SL::FunctionCall *pCall);

        VarVal *clone();

        void add_responsibility(const string &var_name, SL::VarVal *new_child);

        bool is_input_holder();

        bool is_solution_holder();

        void clear_assert_0_shared_ptrs();

        void remove_responsibility(const string& key);
    };

    class Param;

    class Params {
        Param *head = nullptr;
        Params *rest = nullptr;
    public:
        Params() = default;
        Params(Param *_head, Params *_rest = nullptr) : head(_head), rest(_rest) {};
        Params(Param *_head, Param *_next) : head(_head), rest(new Params(_next)) {};
        void populate_vector(vector<Param*>* params)
        {
            assert(params != nullptr);
            assert(params->empty());
            Params* at = this;
            while(at != nullptr)
            {
                if(at->head != nullptr) {
                    params->emplace_back(at->head);
                }
                at = at->rest;
            }
        }
    };

    class Expression;

    enum MethodId {
        _unknown_method,
        _file, _produce_subset_file,
        _sat_solver, _produce_concretization,
        _concretize, _size, _get,
        _passes, _clear, _Solution, _join,
        _print, _num_holes, _append,
        _first, _second,
        _to_float, _sort_vec,
        _clone, _assert, _reverse,
        _produce_replace,
        _replace, _get_solution,
        _produce_filter,
        _not, _relabel, _reset};

    static bool method_str_to_method_id_map_is_defined = false;
    static map<string, MethodId> method_str_to_method_id_map;
    static map<MethodId, vector<string> > method_id_to_type_str;

    static void add_to_method_str_to_method_id_map(
            const string& method_str, MethodId method_enum, string type_str_1, string type_str_2 = "", string type_str_3 = "");

    static void init_method_str_to_method_id_map();

    class FunctionCall {
        Expression *expression = nullptr;
        union {
            Identifier *method_name;
            SLType *type_constructor;
        };
        vector<Param*> params;

        enum MethodMetaType {name_meta_type, type_constructor_meta_type};

        MethodMetaType method_meta_type;

        MethodId method_id = _unknown_method;

        MethodId get_method_id();

        SL::VarVal* eval_global(SolverProgramState *state);

        pair<Var*, SL::VarVal*> get_var_and_var_val_and_assert_type(SolverProgramState* state, vector<string> type_names);

        SL::VarVal* eval_type_constructor(SolverProgramState* state);

    public:
        FunctionCall(
                Expression *_expression, Identifier *_method_name, Params *_params) :
                expression(_expression), method_name(_method_name), method_meta_type(name_meta_type) {
            _params->populate_vector(&params);
            method_id = get_method_id();

        };
        FunctionCall(
                Identifier *_method_name, Params *_params) :
                method_name(_method_name), method_meta_type(name_meta_type) {
            _params->populate_vector(&params);
            method_id = get_method_id();
        };
        FunctionCall(SLType *_type_constructor, Params *_params){
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
            _params->populate_vector(&params);
            method_id = get_method_id();
        };
        explicit FunctionCall(FunctionCall* to_copy);

        SL::VarVal* eval(SolverProgramState* state);

        template<typename VarType>
        SL::VarVal* eval(VarType& var, SolverProgramState* state, VarVal* the_var_val);

        void run(SolverProgramState *pState);

        void clear();
    };

    class Assignment
    {
        union {
            Var* dest_var;
            Identifier* dest_name;
        };

        Expression* expression = nullptr;

        enum DestMetaType {var_dest_type, name_dest_type, no_dest_type};

        DestMetaType dest_type = no_dest_type;

    public:

        explicit Assignment(Var* _var): dest_var(_var), dest_type(var_dest_type){}
        Assignment(Var* _var, Expression* _expression): dest_var(_var), expression(_expression), dest_type(var_dest_type) {}
        Assignment(Identifier* _name, Expression* _expression): dest_name(_name), expression(_expression), dest_type(name_dest_type) {}
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

        SL::VarVal* eval(SolverProgramState *state);

        Var *get_var() {
            switch (meta_type) {
                case is_var:
                    return var;
                    break;
                default:
                    assert(false);
            }
        }

        void clear();

    };

    class BinaryExpression;

    class LambdaExpression;

    class Expression
    {
        union
        {
            BinaryExpression* binary_expression;
            LambdaExpression* lambda_expression;
            FunctionCall* function_call;
            Identifier* identifier;
            SL::VarVal* var_val;
        };
        enum ExpressionMetaType {
            binary_expr_meta_type, func_call_meta_type, identifier_meta_type,
            var_val_meta_type,  lambda_expr_meta_type,  no_meta_type};
        ExpressionMetaType expression_meta_type = no_meta_type;
    public:
        explicit Expression(BinaryExpression* _binary_expression): binary_expression(_binary_expression), expression_meta_type(binary_expr_meta_type){}
        explicit Expression(LambdaExpression* _lambda_expression): lambda_expression(_lambda_expression), expression_meta_type(lambda_expr_meta_type){}
        explicit Expression(FunctionCall* _func_call): function_call(_func_call), expression_meta_type(func_call_meta_type){}
        explicit Expression(Identifier* _identifier): identifier(_identifier), expression_meta_type(identifier_meta_type){}
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

        Identifier* get_var_name()
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

    enum BinaryOp {_lt, _gt, _eq, _geq, _plus, _minus, _mult, _div};

    class BinaryExpression
    {
    protected:
        Expression* left_operand = nullptr;
        Expression* right_operand = nullptr;
        BinaryOp op;
    public:
        BinaryExpression(BinaryOp _op,
                         Expression* _left,
                         Expression* _right): op(_op), left_operand(_left), right_operand(_right) {};

        explicit BinaryExpression(BinaryExpression* to_copy);

        VarVal* eval(SolverProgramState* state)
        {
            VarVal* left_var_val = left_operand->eval(state);
            VarVal* right_var_val = right_operand->eval(state);

            assert(left_var_val->get_type() == right_var_val->get_type());

            switch (op) {
                case _lt:
                    return left_var_val->lt_op(right_var_val);
                    break;
                case _gt:
                    return left_var_val->gt_op(right_var_val);
                    break;
                case _eq:
                    return left_var_val->eq_op(right_var_val);
                    break;
                case _geq:
                    return left_var_val->geq_op(right_var_val);
                    break;
                case _plus:
                    return left_var_val->plus_op(right_var_val);
                    break;
                case _minus:
                    return left_var_val->minus_op(right_var_val);
                    break;
                case _mult:
                    return left_var_val->mult_op(right_var_val);
                    break;
                case _div:
                    return left_var_val->div_op(right_var_val);
                    break;
                default:
                    assert(false);
            }
            assert(false);
        }

        void clear()
        {
            left_operand->clear();
            right_operand->clear();
            delete this;
        }
    };

    class CodeBlock;

    class LambdaExpression
    {
        vector<Param*>* meta_params;
        vector<Param*>* params;
        SL::CodeBlock* code_block;
    public:
        LambdaExpression(SL::Params* _meta_params, SL::Params* _params, SL::CodeBlock* _code_block): code_block(_code_block) {
            params = new vector<Param*>();
            _params->populate_vector(params);
            meta_params = new vector<Param*>();
            _meta_params->populate_vector(meta_params);
        }
        LambdaExpression(LambdaExpression* to_copy);

        void clear();

        VarVal *eval(SolverProgramState *state);
    };

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
        CodeBlock* else_body = nullptr;
    public:
        If(Expression* _expression, CodeBlock* _body): expression(_expression), body(_body) {}
        If(Expression* _expression, CodeBlock* _body, CodeBlock* _else_body): expression(_expression), body(_body), else_body(_else_body) {}
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
        vector<Param*>* params = nullptr;
        vector<Param*>* meta_params = nullptr;

    public:
        Method(Var* _var, Params* _params, CodeBlock* _body): var(_var), body(_body)
        {
            params = new vector<Param*>();
            _params->populate_vector(params);
        }

        Method(Var* _var, vector<Param*>* _params, CodeBlock* _body, vector<Param*>* _meta_params = nullptr):
            var(_var), body(_body), params(_params), meta_params(_meta_params){}

        explicit Method(Method* to_copy);

        void clear();

        void run(SolverProgramState* state, vector<Param*>& input_params);

        template<typename T>
        SL::VarVal* eval(SolverProgramState* state, vector<T>& params);

        void run(SolverProgramState* state, vector<VarVal*>& input_params);

        Var* get_var()
        {
            return var;
        }

        const vector<SL::Param*>* get_params();
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
