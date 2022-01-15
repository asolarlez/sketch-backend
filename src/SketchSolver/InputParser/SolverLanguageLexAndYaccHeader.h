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
        explicit Name(string _name) : name(std::move(_name)), defined(true) {};
        explicit Name(void* _nullptr) : defined(false) {assert (_nullptr == nullptr);};

        bool is_defined()
        {
            return defined;
        }

        string to_string() const {
            assert(defined);
            return name;
        }

        void clear()
        {
            name.clear();
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

    };

    class SLType;

    template<typename Head>
    class LinkedList
    {
        Head* head = nullptr;
        LinkedList* rest = nullptr;
    public:
        LinkedList(Head* _head): head(_head) {}
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
        TypeParams(SLType* _head): LinkedList<SLType>(_head) {}
        TypeParams(SLType* _head, TypeParams* _rest): LinkedList<SLType>(_head, _rest) {}

    };

    class SLType
    {
        Name* name = nullptr;
        vector<SLType*>* type_params = nullptr;
    public:
        explicit SLType(Name* _name): name(_name){}
        SLType(Name* _name, TypeParams* _type_params): name(_name){
            type_params = new vector<SLType*>();
            _type_params->populate_vector(*type_params);
        }

        string to_string()
        {
            if(type_params == nullptr) {
                if(name != nullptr) {
                    return name->to_string();
                }
                else
                {
                    return "any";
                }
            }
            else
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
                return name->to_string() + "<" + str + " >";
            }
        }

        bool operator < (const SLType& other) const
        {
            assert(name != nullptr && other.name != nullptr);
            if(*name < *other.name)
            {
                return true;
            }
            else if(*other.name < *name)
            {
                return false;
            }
            if(type_params == nullptr && other.type_params == nullptr)
            {
                return false;
            }
            else if(type_params == nullptr && other.type_params != nullptr)
            {
                return false;
            }
            else if(type_params != nullptr && other.type_params == nullptr)
            {
                return true;
            }
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

        bool operator == (const SLType& other) const
        {
            assert(name != nullptr && other.name != nullptr);
            if(!(*name == *other.name))
            {
                return false;
            }
            if((type_params == nullptr) && (other.type_params == nullptr))
            {
                return true;
            }
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

        Name *get_head() {
            assert(name != nullptr);
            return name;
        }

        const vector<SLType*>* get_type_params() {
            assert(name != nullptr);
            return type_params;
        }

        bool is_simple_type() {
            assert(name != nullptr);
            if(type_params == nullptr)
            {
                return true;
            }
            else
            {
                assert(!type_params->empty());
                return false;
            }
        }

        bool is_defined() {
            return name != nullptr;
        }
    };


    const bool is_strongly_typed = false;

    class Var {
        SLType *type = nullptr;
        Name *name = nullptr;
    public:

        Var(SLType *_type, Name *_name) : type(_type), name(_name) {};
        Var(Name *_type, Name *_name) : type(new SL::SLType(_type)), name(_name) {};

        Name *get_name() {
            return name;
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

    };

    class PolyType
    {
        const vector<SLType*>* type_params;
    public:
        explicit PolyType(const vector<SLType*>* _type_params): type_params(_type_params) {}

        const vector<SLType*>* get_type_params()
        {
            return type_params;
        }
    };

    class PolyVec: public PolyType, private vector<SL::VarVal*>
    {
    public:
        explicit PolyVec(const vector<SLType*>* _type_params): PolyType(_type_params){
            assert(get_type_params()->size() == 1);
        }
        void emplace_back(SL::VarVal* new_element);

        void sort();

        void clear();

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
        explicit PolyPair(const vector<SLType*>* _type_params, SL::VarVal* left, SL::VarVal* right);

        SL::VarVal* first() const
        {
            return pair<SL::VarVal*, SL::VarVal*>::first;
        }
        SL::VarVal* second() const
        {
            return pair<SL::VarVal*, SL::VarVal*>::second;
        }

        void clear();

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
        T get(T ret, bool do_count, bool do_assert)
        {
            if(do_count) {
                if(do_assert) {
                    assert(get_num_shared_ptr() >= 1);
                }
                increment_shared_ptr();
                dealloc();
            }
            return ret;
        }

        bool get_bool(bool do_count = true, bool do_assert = true)
        {
            assert(var_val_type == bool_val_type);
            return get<bool>(b, do_count, do_assert);
            return b;
        }

        int get_int(bool do_count = true, bool do_assert = true) {
            if(var_val_type == int_val_type) {
                return get<int>(i, do_count, do_assert);
            }
            else if(var_val_type == bool_val_type)
            {
                return get<bool>(i, do_count, do_assert);
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
            return method;
        }

        bool is_method() {
            return var_val_type == method_val_type;
        }

        string get_string(bool do_count = true, bool do_assert = true) {
            assert(var_val_type == string_val_type);
            return get<string>(s->to_string(), do_count, do_assert);
            return s->to_string();
        }

        SketchFunction *get_harness(bool do_count = true, bool do_assert = true) {
            assert(var_val_type == skfunc_val_type);
            assert(do_assert);
            return get<SketchFunction *>(skfunc, do_count, do_assert);
            return skfunc;
        }

        File *get_file(bool do_count = true, bool do_assert = true) {
            assert(var_val_type == file_val_type);
            assert(do_assert);
            return get<File *>(file, do_count, do_assert);
            return file;
        }
        SolverLanguagePrimitives::SolutionHolder *get_solution(bool do_count = true, bool do_assert = true) {
            assert(var_val_type == solution_val_type);
            assert(do_assert);
            return get<SolverLanguagePrimitives::SolutionHolder *>(solution, do_count, do_assert);
            return solution;
        }

        SketchFunction *get_function(bool do_count = true, bool do_assert = true) {
            assert(var_val_type == skfunc_val_type);
            assert(do_assert);
            return get<SketchFunction *>(skfunc, do_count, do_assert);
            return skfunc;
        }

        SolverLanguagePrimitives::InputHolder *get_input_holder(bool do_count = true, bool do_assert = true) {
            assert(var_val_type == input_val_type);
            assert(do_assert);
            return get<SolverLanguagePrimitives::InputHolder *>(input_holder, do_count, do_assert);
            return input_holder;
        }

        bool is_void(){
            return var_val_type == void_val_type;
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


        float get_float(bool do_count = true, bool do_assert = true) {
            assert(var_val_type == float_val_type);
            return get<float>(float_val, do_count, do_assert);
            return float_val;
        }

        PolyVec *get_poly_vec(bool do_count = true, bool do_assert = true) {
            assert(var_val_type == poly_vec_type);
            assert(do_assert);
            return get<PolyVec *>(poly_vec, do_count, do_assert);
            return poly_vec;
        }

        PolyPair *get_poly_pair(bool do_count = true, bool do_assert = true) {
            assert(var_val_type == poly_pair_type);
            assert(do_assert);
            return get<PolyPair *>(poly_pair, do_count, do_assert);
            return poly_pair;
        }

        void dealloc()
        {
            assert(num_shared_ptr >= 1);
            num_shared_ptr --;

            if (num_shared_ptr == 0) {
                cout << "VarVal->clear()" << endl;
                clear();
                delete this;
            } else {
                cout << "remaining pointers: " << num_shared_ptr << endl;
            }

        }

        template<typename T>
        static void clear(T*& val)
        {
            if(val != nullptr) {
                val->clear();
                delete val;
                val = nullptr;
            }
        }

        void clear() {

            switch (var_val_type) {
                case string_val_type:
                    clear<Name>(s);
                    break;
                case int_val_type:
                    //do nothing
                    break;
                case file_val_type:
                    clear<File>(file);
                    break;
                case method_val_type:
                    assert(false);
                    break;
                case skfunc_val_type:
                    if(skfunc != nullptr) {
                        skfunc->clear();
                        skfunc = nullptr;
                    }
                    break;
                case solution_val_type:
                    clear<SolverLanguagePrimitives::SolutionHolder>(solution);
                    break;
                case input_val_type:
                    clear<SolverLanguagePrimitives::InputHolder>(input_holder);
                    break;
                case bool_val_type:
                    //do nothing
                    break;
                case void_val_type:
                    assert(false);
                    break;
                case no_type:
                    assert(false);
                    break;
                case float_val_type:
                    //do nothing
                    break;
                case poly_pair_type:
                    clear<SL::PolyPair>(poly_pair);
                    break;
                case poly_vec_type:
                    clear<SL::PolyVec>(poly_vec);
                    break;
                default:
                    assert(false);
            }
        }

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

        bool get_is_return();
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

        pair<Var*, SL::VarVal*> get_var_assert_type(SolverProgramState* state, string type_name);

        SL::VarVal* eval(SolverProgramState* state);

        void run(SolverProgramState *pState);

        SL::VarVal*eval_type_constructor(SolverProgramState* state);
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

        Var* get_var() const
        {
            assert(dest_type == var_dest_type);
            return dest_var;
        }
        void run(SolverProgramState* state);

        bool has_assignment();
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

        SL::VarVal* eval(SolverProgramState* state);

        void run(SolverProgramState* state) {
            SL::VarVal* ret = eval(state);
            if(!ret->is_void())
            {
                cout << "WARNING: " << "Expression returns but result not stored." << endl;
            }
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
    };
    
    class CodeBlock;

    class While
    {
        Expression* expression = nullptr;
        CodeBlock* body = nullptr;
    public:
        While(Expression* _expression, CodeBlock* _body): expression(_expression), body(_body) {}

        void run(SolverProgramState* state);

    };

    class UnitLine;

    class For
    {
        UnitLine* def;
        Expression* expression = nullptr;
        UnitLine* plus_plus;
        CodeBlock* body = nullptr;
    public:
        For(UnitLine* _def, Expression* _expression, UnitLine* _plus_plus, CodeBlock* _body):
            def(_def), expression(_expression), plus_plus(_plus_plus), body(_body) {}

        void run(SolverProgramState* state);

    };


    class If
    {
        Expression* expression = nullptr;
        CodeBlock* body = nullptr;
    public:
        If(Expression* _expression, CodeBlock* _body): expression(_expression), body(_body) {}

        void run(SolverProgramState* state);
    };

    class Return
    {
        Expression* expression = nullptr;
    public:
        explicit Return(Expression* _expression) : expression(_expression) {}

        void run(SolverProgramState* state);
    };

    class UnitLine
    {
        enum LineType {var_line, assign_line, while_line, if_line, return_line, expression_line, for_line};
        union {
            Var* var;
            Assignment* assignment;
            While* while_loop;
            For* for_loop;
            If* if_block;
            Return* return_stmt;
            Expression* expression;
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

        void run(SolverProgramState *state) {
            switch (line_type) {
                case var_line:
                    var->run(state);
                    break;
                case assign_line:
                    assignment->run(state);
                    break;
                case while_line:
                    while_loop->run(state);
                    break;
                case if_line:
                    if_block->run(state);
                    break;
                case return_line:
                    return_stmt->run(state);
                    break;
                case expression_line:
                    expression->run(state);
                    break;
                case for_line:
                    for_loop->run(state);
                    break;
                default:
                    assert(false);
            }
        }
    };

    class CodeBlock
    {
        UnitLine* head = nullptr;
        CodeBlock* rest = nullptr;
    public:
        explicit CodeBlock(UnitLine* _head): head(_head) {}
        CodeBlock(UnitLine* _head, CodeBlock* _rest): head(_head), rest(_rest) {}

        void run(SolverProgramState *state) {
            assert(head != nullptr);
            head->run(state);
            if(rest != nullptr){
                rest->run(state);
            }
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

        void add_to_map(Frame& frame);

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
