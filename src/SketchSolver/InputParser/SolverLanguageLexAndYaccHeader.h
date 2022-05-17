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
#include "FunctionMap.h"

using namespace std;

class SketchFunction;
class GenericFile;
class File;

class ProgramState;

namespace SL
{

    class VarVal;

    class Identifier {

        static int global_identifier_id;

        int identifier_id = 0;

        bool defined = false;
        string identifier;
    public:
        explicit Identifier(string _name) : identifier(std::move(_name)), defined(true), identifier_id(Identifier::global_identifier_id++) {};
        explicit Identifier(Identifier* to_copy) : identifier(to_copy->to_string()), defined(to_copy->defined), identifier_id(Identifier::global_identifier_id++) {assert(defined); };

        string to_string() const {
            assert(defined);
            return identifier;
        }

        void clear()
        {
//            cout << "CLEAR IDENTIFIER ID = " <<identifier_id << " name: " << identifier << endl;
            //clears everything
            assert(defined);
            identifier.clear();
            defined = false;
            delete this;
        }

        template<typename StateType> VarVal* eval(StateType* state);

        bool operator<(const Identifier &other) const {
            assert(defined);
            return identifier < other.identifier;
        }

        bool operator==(const Identifier &other) const {
            assert(defined);
            return identifier == other.identifier;
        }

        bool is_defined() const;
    };

    class Method;
    class PolyPair;
    class PolyVec;
    class PolyMap;
    class FunctionCall;
    class Param;

    enum VarValType {
        void_val_type, string_val_type, int_val_type, bool_val_type, float_val_type,
        generic_file_val_type, file_val_type,
        method_val_type, skfunc_val_type,
        solution_val_type, input_val_type,
        poly_vec_type, poly_pair_type, poly_map_type,
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
            Identifier* s;
            int i;
            bool b;
            float float_val;
            GenericFile* generic_file;
            File* file;
            Method* method;
            SketchFunction* skfunc;
            HoleVarStore* solution;
            InputVarStore* input_holder;
            PolyVec* poly_vec;
            PolyPair* poly_pair;
            PolyMap* poly_map;
        };
        const VarValType var_val_type;
        mutable int num_shared_ptr = 0;
    public:
        explicit VarVal(string  _s);
        template <typename T>
        VarVal(T val): var_val_type(get_var_val_type(val)){

            if(std::is_same<bool,T>::value)
            {
                b = val;
            }
            else if(std::is_same<int,T>::value)
            {
                i = val;
            }
            else {
                assert(false);
            }
        }
        explicit VarVal(float _float_val);
        explicit VarVal(GenericFile* _generic_file);
        explicit VarVal(File* _file);
        explicit VarVal(Method* _method);
        explicit VarVal(SketchFunction* _harness);
        explicit VarVal(PolyPair* _poly_pair);
        explicit VarVal(PolyVec* _poly_vec);
        explicit VarVal(PolyMap* _poly_map);
        explicit VarVal(HoleVarStore* _solution);
        explicit VarVal(InputVarStore* _input_holder);
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
                case generic_file_val_type:
                    return "File";
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
                case poly_pair_type:
                    return "pair";
                    break;
                case poly_vec_type:
                    return "vector";
                    break;
                case poly_map_type:
                    return "map";
                    break;
                case no_type:
                    assert(false);
                    break;
                default:
                    assert(false);
            }
            assert(false);
        }

        bool operator < (const VarVal& other) const;

        VarValType get_type()
        {
            return var_val_type;
        }

        template<typename T>
        void assert_type_invariant() const
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
            else if(std::is_same<GenericFile*,T>::value){
                assert(var_val_type == generic_file_val_type);
            }
            else if(std::is_same<HoleVarStore *,T>::value){
                assert(var_val_type == solution_val_type);
            }
            else if(std::is_same<InputVarStore *,T>::value){
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
            else if(std::is_same<PolyMap*,T>::value){
                assert(var_val_type == poly_map_type);
            }
            else if(std::is_same<File*,T>::value){
                assert(var_val_type == file_val_type);
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
                if(is_pointer<T>::value) {
                    assert(do_assert);
                }
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

        bool is_sketch_function() const
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

        GenericFile *get_generic_file(bool do_count = true, bool do_assert = true) {
            assert(var_val_type == generic_file_val_type);
            assert(do_assert);
            return get<GenericFile *>(generic_file, do_count, do_assert);
        }

        GenericFile *get_generic_file_const(bool do_count) const {
            assert(!do_count);
            assert(var_val_type == generic_file_val_type);
            return generic_file;
        }

        File *get_file(bool do_count = true, bool do_assert = true) {
            assert(var_val_type == file_val_type);
            assert(do_assert);
            return get<File *>(file, do_count, do_assert);
        }

        File *get_file_const(bool do_count) const {
            assert(!do_count);
            assert(var_val_type == file_val_type);
            return file;
        }

        HoleVarStore *get_hole_var_store(bool do_count = true, bool do_assert = true) {
            assert(var_val_type == solution_val_type);
            if(do_count) {
                assert(do_assert);
            }
            return get<HoleVarStore *>(solution, do_count, do_assert);
        }
        HoleVarStore *get_solution_const(bool do_count = true) const {
            assert(var_val_type == solution_val_type);
            assert(!do_count);
            return solution;
        }
        InputVarStore* get_input_holder(bool do_count = true, bool do_assert = true) {
            assert(var_val_type == input_val_type);
            assert(do_assert);
            return get<InputVarStore *>(input_holder, do_count, do_assert);
        }
        SketchFunction *get_skfunc(bool do_count = true, bool do_assert = true) {
            assert(is_sketch_function());
            assert(do_assert);
            return get<SketchFunction *>(skfunc, do_count, do_assert);
        }

        SketchFunction *get_function_const(bool do_count) const {
            assert(!do_count);
            assert(is_sketch_function());
            return skfunc;
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

        PolyVec *get_poly_vec_const(bool do_count) const {
            assert(!do_count);
            assert(var_val_type == poly_vec_type);
            return poly_vec;
        }

        PolyMap *get_poly_map(bool do_count = true, bool do_assert = true) {
            assert(var_val_type == poly_map_type);
            assert(do_assert);
            return get<PolyMap *>(poly_map, do_count, do_assert);
        }

        PolyMap *get_poly_map_const(bool do_count) const {
            assert(!do_count);
            assert(var_val_type == poly_map_type);
            return poly_map;
        }

        PolyPair *get_poly_pair(bool do_count = true, bool do_assert = true) {
            assert(var_val_type == poly_pair_type);
            assert(do_assert);
            return get<PolyPair *>(poly_pair, do_count, do_assert);
        }

        PolyPair *get_poly_pair_const(bool do_count) const {
            assert(!do_count);
            assert(var_val_type == poly_pair_type);
            return poly_pair;
        }

        string to_string(bool do_count = true, bool do_assert = true)
        {
            switch (var_val_type) {

                case string_val_type:
                    return "\"" + get_string(do_count, do_assert) + "\"";
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
                case poly_map_type:
                    assert(false);
                    break;
                default:
                    AssertDebug(false, "MISSING CASE");
            }
            AssertDebug(false, "MISSING CASE");
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
        void clear(T& val, bool do_delete = true);

        template<typename StateType, typename T>
        VarVal* eval(T& val, StateType* state, FunctionCall* function_call);

        void _clear() {
            assert(num_shared_ptr == 0);
            switch (var_val_type) {
                case string_val_type:
                    clear<Identifier*>(s, false);
                    break;
                case int_val_type:
                    //do nothing
                    break;
                case generic_file_val_type:
                    clear<GenericFile*>(generic_file, false);
                    break;
                case file_val_type:
                    clear<File*>(file, false);
                    break;
                case method_val_type:
                    clear<Method*>(method, false);
                    break;
                case skfunc_val_type:
                    clear<SketchFunction*>(skfunc, false);
                    break;
                case solution_val_type:
                    clear<HoleVarStore*>(solution, false);
                    break;
                case input_val_type:
                    clear<InputVarStore*>(input_holder, false);
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
                    clear<SL::PolyPair*>(poly_pair, false);
                    break;
                case poly_vec_type:
                    clear<SL::PolyVec*>(poly_vec, false);
                    break;
                case poly_map_type:
                    clear<SL::PolyMap*>(poly_map, false);
                    break;
                default:
                    AssertDebug(false, "MISSING CASE");
            }
            delete this;
        }

    public:
        void increment_shared_ptr();

        void complete_return();


    private:
        bool is_return = false;
    public:
        void set_return();

        bool get_is_return() const;

        template<typename StateType> VarVal* eval(StateType* state, SL::FunctionCall *pCall);

        bool is_input_holder() const;

        bool is_string() const;

        bool is_solution_holder() const;

        void clear_assert_0_shared_ptrs() ;

    };

    class SLType;

    template<typename Head>
    class LinkedList
    {
        Head* head = nullptr;
        LinkedList* rest = nullptr;
    public:
        LinkedList() {}
        explicit LinkedList(Head* _head): head(_head) {}
        LinkedList(Head* _head, LinkedList* _rest): head(_head), rest(_rest) {}

        void populate_vector(vector<Head*>& ret)
        {
            assert(ret.size() == 0);
            LinkedList<Head>* at = this;
            while(at != nullptr) {
                if(at->head != nullptr) {
                    ret.push_back(new Head(at->head));
                }
                else {
                    assert(at->rest == nullptr);
                }
                at = at->rest;
            }
        }

        void clear()
        {
            if(head != nullptr) {
                head->clear();
                head = nullptr;
            }
            else
            {
                assert(rest == nullptr);
            }
            if(rest != nullptr)
            {
                rest->clear();
                rest = nullptr;
            }
            delete this;
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
            //clears everything
            type->clear();
            name->clear();
            delete this;
        }

        string to_string() const {
            return name->to_string() + " : " + type->to_string();
        }

        template<typename StateType>
void run(StateType* state);

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

        template<typename StateType> VarVal* eval(StateType* state);

        SLType * get_type();

        bool has_type()
        {
            return type->is_defined();
        }

        bool accepts_type(const string& type_name)
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

    bool var_val_invariant(SL::SLType *var_type, SL::VarVal*var_val);
    bool var_val_invariant(SL::Var *var, SL::VarVal*var_val);


    vector<SL::SLType*>* copy_type_params(vector<SLType*>* to_copy);

    class PolyType
    {
        vector<SL::SLType*>* type_params = nullptr;

        static vector<SL::SLType*>* one_type_param(string type_name)
        {
            vector<SL::SLType*>* ret = new vector<SL::SLType*>();
            ret->push_back(new SL::SLType(new Identifier(type_name)));
            return ret;
        }

    public:
        explicit PolyType(TypeParams* _type_params){
            assert(_type_params != nullptr);
            type_params = new vector<SL::SLType*>();
            _type_params->populate_vector(*type_params);
            _type_params->clear();
        }
        explicit PolyType(vector<SLType*>* _type_params): type_params(_type_params) {
            assert(_type_params != nullptr);}
        explicit PolyType(string any_str): type_params(one_type_param(any_str)) {assert(any_str == "any");}
        explicit PolyType(PolyType* to_copy): type_params(copy_type_params(to_copy->type_params)) {
            assert(to_copy != nullptr);
        };

        SL::SLType* at(int idx)
        {
            assert(type_params != nullptr);
            assert(0 <= idx && idx < size());
            return type_params->at(idx);
        }

        virtual size_t size()
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
            soft_clear();
            delete this;
        }

        void soft_clear()
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
        explicit PolyVec(PolyType* _type_params, int size = 0): vector<SL::VarVal*>(size, nullptr), PolyType(_type_params){
            assert(get_type_params()->size() == 1);
            _type_params->clear();
        }
        explicit PolyVec(PolyVec* to_copy);

        void push_back(SL::VarVal* new_element);

        void sort();

        void clear() override;

        auto begin(){
            return vector<SL::VarVal*>::begin();
        }

        auto end(){
            return vector<SL::VarVal*>::end();
        }

        size_t size(){
            return vector<SL::VarVal*>::size();
        }

        SL::VarVal* at(int idx)
        {
            assert(idx >= 0 && idx < size());
            return vector<SL::VarVal*>::at(idx);
        }

        void set(size_t idx, SL::VarVal* new_element)
        {
            assert(0 <= idx && idx < size());
            if(at(idx) != nullptr) {
                at(idx)->decrement_shared_ptr();
            }
            if(new_element != nullptr) {
                new_element->increment_shared_ptr();
            }
            (*this)[idx] = new_element;
        }

        vector<bool> to_vector_bool();

        void reverse();
    };
}

static exception NameNotFound;

class Frame {
    map<SL::Var, SL::VarVal* > vars_map;
    map<SL::Identifier, SL::Var *> name_to_var_map;
    vector<SL::Var*> all_new_vars;

public:

    bool is_empty() {
        return vars_map.empty() && name_to_var_map.empty() && all_new_vars.empty();
    }

    Frame() = default;

    void clear(bool is_global = false)
    {
        for (auto it: vars_map) {
            if (it.second != nullptr) {
                if (is_global){
                    if (it.second->is_sketch_function()) {
                        assert(it.second->get_num_shared_ptr() == 1);
                        delete it.second;
                        continue;
                    }
                }
//                string skfunc_name = "not_skfunc";
//                bool is_skfunc = false;
//                if(it.second->is_sketch_function())
//                {
//                    is_skfunc = true;
//                    SketchFunction* skfunc = it.second->get_skfunc(false);
//                    if(skfunc != nullptr) {
//                        skfunc_name = skfunc->get_dag()->get_name();
//                    }
//                    else
//                    {
//                        skfunc_name = "nullptr";
//                    }
//                }
                bool cleared = it.second->decrement_shared_ptr();
//                if(is_skfunc) {
//                    if (cleared) {
//                        cout << "CLEARED";
//                    }
//                    else
//                    {
//                        cout << "NOT CLEARED";
//                    }
//                    cout << " var " << it.first.to_string() << " holding skfunc " << skfunc_name << endl;
//                }
            }
        }

        vars_map.clear();
        name_to_var_map.clear();
        for(auto it:all_new_vars){
            it->clear();
        }
        all_new_vars.clear();
    }

    void add_var_name(SL::Var *_var, bool assert_new = true)
    {
        SL::Identifier name = *_var->get_name();
        if(name_to_var_map.find(name) == name_to_var_map.end()) {
            SL::Var* var = new SL::Var(_var);
            all_new_vars.push_back(var);
            //new variable
            name_to_var_map[name] = var;
        }
        else
        {
            SL::Var* var = _var;
            assert(!assert_new);
            //existing variable.
            assert(*name_to_var_map[name] == *var);
        }
    }

    virtual void add_var(SL::Var *_var) {
        SL::Var* var = new SL::Var(_var);
        all_new_vars.push_back(var);
        assert(vars_map.find(*var) == vars_map.end());
        vars_map[*var] = nullptr;
        add_var_name(var);
    }

    virtual SL::Var *name_to_var_throws(SL::Identifier *name) {
        if(name_to_var_map.find(*name) == name_to_var_map.end())
        {
            throw NameNotFound;
        }
        return name_to_var_map[*name];
    }

    SL::Var *name_to_var(SL::Identifier *name) {
        try{
            return name_to_var_throws(name);
        } catch(exception& e){
            AssertDebug(false, "ERROR:: IDENTIFIER NOT FOUND: " + name->to_string());
        }
    }

    exception VarNotFound;

    virtual SL::VarVal* get_var_val_throws(SL::Var *var) {
        if(vars_map.find(*var) == vars_map.end()){
            throw VarNotFound;
        }
        return vars_map[*var];
    }

    SL::VarVal* get_var_val(SL::Var *var) {
        try {
            return get_var_val_throws(var);
        } catch (exception& e){
            assert(false);
        }
    }

    virtual void set_var_val_throws(SL::Var *_var, SL::VarVal* var_val) {

        if(vars_map.find(*_var) == vars_map.end())
        {
            //not yet declared.
            throw VarNotFound;
        }
        else
        {
            //overwrite.
        }

        SL::Var* var = new SL::Var(_var);
        all_new_vars.push_back(var);

        if(var_val != nullptr) {
            var_val->increment_shared_ptr();
        }

        if(vars_map[*var] != nullptr){
            bool is_deleted = vars_map[*var]->decrement_shared_ptr();
            if(is_deleted)
            {
                vars_map[*var] = nullptr;
                vars_map.erase(*var);
            }
        }

        vars_map[*var] = var_val;
        add_var_name(var, false);
    }

    void set_var_val(SL::Var *var, SL::VarVal* var_val) {
        try {
            set_var_val_throws(var, var_val);
        }
        catch (exception& e) {
            AssertDebug(false, "NOT YET DECLARED: " + var->to_string());
        }
    }

    void add_var_and_set_var_val_and_clear_var(SL::Var *var, SL::VarVal* var_val) {
        add_var(var);
        set_var_val(var, var_val);
        var->clear();
    }
};

class NestedFrame: private Frame
{
    vector<Frame> frames;

public:

    bool is_empty()
    {
        return frames.empty();
    }

    explicit NestedFrame() {
        assert(frames.empty());
    }

    void open_subframe()
    {
        frames.push_back(Frame());
    }

    void close_subframe()
    {
        assert(!frames.empty());
        frames.rbegin()->clear();
        frames.pop_back();
    }

    void add_var(SL::Var *var) override {
        assert(!frames.empty());
        frames.rbegin()->add_var(var);
    }

    SL::VarVal* get_var_val_throws(SL::Var *var) override {
        assert(!frames.empty());
        int at_id = frames.size()-1;
        while(at_id >= 0){
            try {
                return frames[at_id].get_var_val_throws(var);
            }
            catch (exception e){
                at_id--;
            }
        }
        throw VarNotFound;
    }

    void set_var_val_throws(SL::Var *var, SL::VarVal* var_val) override{
        assert(!frames.empty());
        int at_id = frames.size()-1;
        while(at_id >= 0){
            try {
                frames[at_id].set_var_val_throws(var, var_val);
                return ;
            }
            catch (exception e){
                at_id--;
            }
        }
        throw VarNotFound;
    }

    SL::Var* name_to_var_throws(SL::Identifier* name) override{
        assert(!frames.empty());
        int at_id = frames.size()-1;
        while(at_id >= 0){
            try {
                return frames[at_id].name_to_var_throws(name);
            }
            catch (exception e){
                at_id--;
            }
        }
        throw NameNotFound;
    }

    void clear()
    {
        assert(frames.size() == 1);
        frames[0].clear();
    }

};

class ProgramState
{

protected:
    vector<NestedFrame> frames;
    Frame global;

public:

    FunctionMap& function_map;

    explicit ProgramState(FunctionMap& _function_map):
        function_map(_function_map), global() {}

    explicit ProgramState(ProgramEnvironment* program_environment);

    virtual void clear(bool conscious_call = false)
    {
        AssertDebug(conscious_call, "METHOD NOT IMPLEMENTED IN DERIVED CLASS.");

        assert(global.is_empty());
        for(auto it: frames) {
            assert(it.is_empty());
        }
        frames.clear();
    }

    void add_var(SL::Var *var) {
        assert(!frames.empty());
        frames.rbegin()->add_var(var);
    }

    SL::VarVal* get_var_val(SL::Var *var) {
        assert(!frames.empty());
        try {
            return frames.rbegin()->get_var_val_throws(var);
        }
        catch (exception& e) {
            try {
                return global.get_var_val_throws(var);
            }
            catch (exception& e2){
                assert(false);
            }
        }
    }

    void set_var_val(SL::Var *var, SL::VarVal* var_val);

    void add_and_set_var_val(SL::Var *var, SL::VarVal* var_val) {
        add_var(var);
        set_var_val(var, var_val);
    }

    SL::Var* name_to_var_throws(SL::Identifier* name)
    {
        assert(!frames.empty());
        try {
            return frames.rbegin()->name_to_var_throws(name);
        }
        catch (exception& e1) {
            try {
                return global.name_to_var_throws(name);
            }
            catch (exception& e2){
                if(!SL::is_strongly_typed) {
                    throw NameNotFound;
                }
                else {
                    assert(false);
                }
            }
        }
    }

    SL::Var* name_to_var(SL::Identifier* name)
    {
        assert(!frames.empty());
        try {
            return name_to_var_throws(name);
        }
        catch (exception& e1) {
            cout << "ERROR: NAME NOT FOUND: " << name->to_string() << endl;
            assert(false);
        }
    }

    SL::VarVal* return_var_val = nullptr;

    bool has_return()
    {
        return return_var_val != nullptr;
    }

    void set_return_var_val(SL::VarVal* var_val) {
        assert(return_var_val == nullptr);
        var_val->set_return();
        return_var_val = var_val;
    }

    SL::VarVal* get_return_var_val() {
        if(return_var_val != nullptr) {
            assert(return_var_val->get_is_return());
            return_var_val->complete_return();
        }
        SL::VarVal* ret = return_var_val;
        return_var_val = nullptr;
        if(ret == nullptr)
        {
            ret = new SL::VarVal();
        }
        return ret;
    }

    void new_stack_frame(vector<SL::Param *> &vars, vector<SL::Param *> &vals, vector<SL::Param *>* meta_vals = nullptr);
    void new_stack_frame(vector<SL::Param *> &vars, vector<SL::VarVal *> &vals, vector<SL::Param *>* meta_vals = nullptr);

    void new_stack_frame() {
        frames.emplace_back(NestedFrame());
        open_subframe();
    }

    void pop_stack_frame() {
        assert(!frames.empty());
        close_subframe();
        assert(frames.rbegin()->is_empty());
        frames.pop_back();
    }

    SL::Var *method_name_to_var(SL::Identifier *name) {
        SL::Var* ret = global.name_to_var(name);
        assert(global.get_var_val(ret)->is_method());
        return ret;
    }

    void open_subframe() {
        AssertDebug(!frames.empty(), "A FRAME IS REQUIRED TO BE ABLE TO OPEN A SUBFRAME.");
        frames.rbegin()->open_subframe();
    }

    void close_subframe(){
        assert(!frames.empty());
        frames.rbegin()->close_subframe();
    }

    void add_to_function_map(const string &skfunc_name, SketchFunction *skfunc);

};

class FloatManager;
class CommandLineArgs;
class HoleHardcoder;

namespace SL {

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

    class PolyMap: public PolyType, private map<string, SL::VarVal*>
    {
    public:
        explicit PolyMap(PolyType* _type_params): PolyType(_type_params){
            assert(get_type_params()->size() == 2);
            assert(*get_type_params()->at(0) == SL::SLType(new SL::Identifier("string")));
            _type_params->clear();
        }

        explicit PolyMap(PolyMap* to_copy);

        void clear() override;

        void insert(const string& key, VarVal* element)
        {
            assert(find(key) == end());
            element->increment_shared_ptr();
            (*this)[key] = element;
        }

        bool has(string key) {
            return find(key) != end();
        }

        size_t size(){
            return map<string, SL::VarVal*>::size();
        }

        SL::VarVal* at(const string& idx)
        {
            assert(has(idx));
            return map<string, SL::VarVal*>::at(idx);
        }

        template<typename OutType>
        map<string, OutType> get_cpp_map();
    };

    class Method;

    class Param;

    class Params: public LinkedList<Param>
    {
    public:
        Params(): LinkedList<Param>() {};
        explicit Params(Param* _head): LinkedList<Param>(_head) {}
        Params(Param* _head, Params* _rest): LinkedList<Param>(_head, _rest) {}
    };

//    class Params {
//        Param *head = nullptr;
//        Params *rest = nullptr;
//    public:
//        Params() = default;
//        Params(Param *_head, Params *_rest = nullptr) : head(_head), rest(_rest) {};
//        Params(Param *_head, Param *_next) : head(_head), rest(new Params(_next)) {}; //TODO: seems like _next should be deleted?
//
//        void populate_vector(vector<Param*>* params);
//
//        void clear();
//    };

    class Expression;

    //sketch function types
        //sketch function with holes
        //concretized unit sketch function
        //concretized inlined sketch function (executable SkFunc)

    enum MethodId {
        _unknown_method,
        _file, _produce_subset_file,
        _sat_solver, _size, _get,
        _passes, _clear,
//        _Solution,
        _join,
        _print, _num_holes, _append,
        _first, _second,
        _to_float, _sort_vec,
        _assert, _reverse,
        _produce_filter,
        _not,
        _relabel,
        _reset,
        _produce_deep_concretize,
//        TODO: produce_unit_concretize
        _inplace_deep_concretize,
        _inplace_unit_concretize,
        _produce_executable,
        _make_executable,
        _clone,
        _deep_clone,
        _unit_clone,
        //_inline_deep_replace,
        _inplace_unit_replace,
        //_produce_deep_replace,
        _produce_unit_replace,
        _get_solution,

        //FMTL primitives:
        _declare,
//        _vector
        _vectorized_count_passing_inputs,
        _evaluate_inputs
    };

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

        template<typename StateType>
        SL::VarVal* eval_global(StateType *state);

        pair<Var*, SL::VarVal*> get_var_and_var_val_and_assert_type(ProgramState* state, vector<string> type_names);

        SL::VarVal* eval_type_constructor(ProgramState* state);

    public:
        FunctionCall(
                Expression *_expression, Identifier *_method_name, Params *_params) :
                expression(_expression), method_name(_method_name), method_meta_type(name_meta_type) {
            _params->populate_vector(params);
            _params->clear();
            method_id = get_method_id();

        };
        FunctionCall(
                Identifier *_method_name, Params *_params) :
                method_name(_method_name), method_meta_type(name_meta_type) {
            _params->populate_vector(params);
            _params->clear();
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
            _params->populate_vector(params);
            _params->clear();
            method_id = get_method_id();
        };
        explicit FunctionCall(FunctionCall* to_copy);

        template<typename StateType>
        SL::VarVal* eval(StateType* state);

        template<typename StateType>
        void run(StateType* state);

        void clear();

        string to_string();

        template<typename StateType>
        VarVal *eval(PolyVec *&poly_vec, StateType *state, const VarVal *const the_var_val);

        template<typename StateType>
        VarVal *eval(PolyPair *&poly_pair, StateType *state, const VarVal *const the_var_val);

        template<typename StateType>
        VarVal *eval(InputVarStore *&input_var_store, StateType *state, const VarVal *const the_var_val);

        template<typename StateType>
        VarVal *eval(HoleVarStore *&hole_var_store, StateType *state, const VarVal *const the_var_val);

        template<typename StateType, typename FileType>
        VarVal *eval(FileType *&generic_file, StateType *state, const VarVal *const the_var_val);

        template<typename StateType>
        VarVal *eval(SketchFunction *&skfunc, StateType *state, const VarVal *const the_var_val);
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
        template<typename StateType>
        void run(StateType* state);

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
        explicit Param(Assignment *assignment) : var(new Var(assignment->get_var())), meta_type(is_var) {
            assert(!assignment->has_assignment());
            assignment->clear();
        };
        explicit Param(Param* to_copy);

        template<typename StateType> VarVal* eval(StateType* state);

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

        string to_string();
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

        template<typename StateType> VarVal* eval(StateType* state);

        template<typename StateType>
        void run(StateType* state) {
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

        string to_string();
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

        template<typename StateType> VarVal* eval(StateType* state);

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
            _params->populate_vector(*params);
            _params->clear();
            meta_params = new vector<Param*>();
            _meta_params->populate_vector(*meta_params);
            _meta_params->clear();
        }
        LambdaExpression(LambdaExpression* to_copy);

        void clear();

        template<typename StateType> VarVal* eval(StateType* state);
    };

    class While
    {
        Expression* expression = nullptr;
        CodeBlock* body = nullptr;
    public:
        While(Expression* _expression, CodeBlock* _body): expression(_expression), body(_body) {}
        explicit While(While* to_copy);

        template<typename StateType>
        void run(StateType* state);

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

        template<typename StateType> void run(StateType* state);

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

        template<typename StateType> void run(StateType* state);

        void clear();
    };

    class Return
    {
        Expression* expression = nullptr;
    public:
        explicit Return(Expression* _expression) : expression(_expression) {}
        explicit Return(Return* to_copy): expression(new Expression(to_copy->expression)){};

        template<typename StateType>
        void run(StateType* state);

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

        template<typename StateType>
        void run(StateType *state);

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

        template<typename StateType>
        void run(StateType *state);

        void clear()
        {
            //clears everything
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
            _params->populate_vector(*params);
            _params->clear();
        }

        Method(Var* _var, vector<Param*>* _params, CodeBlock* _body, vector<Param*>* _meta_params = nullptr):
            var(_var), body(_body), params(_params), meta_params(_meta_params){}

        explicit Method(Method* to_copy);

        void clear();

        template<typename StateType>
        void run(StateType* state, vector<Param*>& input_params);
        
        template<typename StateType>
        void run(StateType* state, vector<VarVal*>& input_params);

        template<typename StateType, typename T> VarVal* eval(StateType* state, vector<T>& params);


        Var* get_var()
        {
            return var;
        }

        const vector<SL::Param*>* get_params();
    };

    class Methods
    {
        Method* head = nullptr;
        Methods* rest = nullptr;
    public:
        explicit Methods(Method* _head): head(_head) {}
        Methods(Method* _head, Methods* _rest): head(_head), rest(_rest) {}

        void populate_state(Frame& frame);

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
};

typedef void* yyscan_t;
class SolverProgramState;
void yyerror(yyscan_t scanner, SolverProgramState* state, string s);

#endif //SOLVERLANGUAGEPARSER_LEXANDYACCHEADER_H
