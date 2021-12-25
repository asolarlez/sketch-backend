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
        string name;
    public:
        explicit Name(string _name) : name(std::move(_name)) {};

        string to_string() {
            return name;
        }

        SL::VarVal *eval(SolverProgramState *state);

        bool operator<(const Name &other) const {
            return name < other.name;
        }

        bool operator==(const Name &other) const {
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
                return name->to_string();
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
                    return true;
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
            return name;
        }

        const vector<SLType*>* get_type_params() {
            return type_params;
        }

        bool is_simple_type() {
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
    };

    class Var {
        SLType *type = nullptr;
        Name *name = nullptr;
    public:

        Var(SLType *_type, Name *_name) : type(_type), name(_name) {};
        Var(Name *_type, Name *_name) : type(new SLType(_type)), name(_name) {};

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
            return *type == *other.type && *name == *other.name;
        }

        bool operator < (const Var& other) const
        {
            assert(defined());
            assert(other.defined());
            if(*type < *other.type)
            {
                return true;
            }
            else if(*other.type < *type)
            {
                return false;
            }
            else
            {
                if(*name < *other.name)
                {
                    return true;
                }
                else if(*other.name < *name)
                {
                    return false;
                }
                else
                {
                    return false;
                }
            }
        }

        VarVal *eval(SolverProgramState *pState);

        SLType * get_type();

    };


    class VarVal;

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

    class PolyVec: public PolyType, private vector<VarVal*>
    {
    public:
        explicit PolyVec(const vector<SLType*>* _type_params): PolyType(_type_params){
            assert(get_type_params()->size() == 1);
        }
        void emplace_back(VarVal* new_element);

        void sort();

        size_t size(){
            return vector<VarVal*>::size();
        }

        VarVal* at(int idx)
        {
            assert(idx >= 0 && idx < size());
            return vector<VarVal*>::at(idx);
        }
    };

    class PolyPair: public PolyType, private pair<VarVal*, VarVal*>
    {
    public:
        explicit PolyPair(const vector<SLType*>* _type_params, VarVal* left, VarVal* right);

        VarVal* first() const
        {
            return pair<VarVal*, VarVal*>::first;
        }
        VarVal* second() const
        {
            return pair<VarVal*, VarVal*>::second;
        }

        bool operator < (const PolyPair& other) const;
    };

    class Method;

    enum VarValType {
        string_val_type, int_val_type, file_val_type,
        method_val_type, skfunc_val_type, solution_val_type,
        input_val_type, bool_val_type, void_val_type,
//        pair_int_solution_val_type,
        float_val_type, poly_vec_type,
//        vector_pair_int_solution_val_type,
        poly_val_type,
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

    class VarVal {
        union {
            string s;
            int i;
            bool b;
            float float_val;
            File* file;
            Method* method;
            SketchFunction* skfunc;
            SolverLanguagePrimitives::SolutionHolder* solution;
            SolverLanguagePrimitives::InputHolder* input_holder;
//            vector<pair<int, SolverLanguagePrimitives::SolutionHolder *>>* vector_pair_int_solution;
//            pair<int, SolverLanguagePrimitives::SolutionHolder *> pair_int_solution;
            PolyVec* poly_vec;
            VarVal* poly_val;
            PolyPair* poly_pair;
        };
        const VarValType var_val_type;
    public:
        explicit VarVal(const string& _s) : s(std::move(_s)), var_val_type(string_val_type) {}
        template <typename T>
        explicit VarVal(T val): var_val_type(get_var_val_type(val)){

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
        explicit VarVal(float _float_val) : float_val(_float_val) , var_val_type(float_val_type){}
        explicit VarVal(File* _file) : file(_file) , var_val_type(file_val_type){}
        explicit VarVal(Method* _method) : method(_method) , var_val_type(method_val_type){}
        explicit VarVal(SketchFunction* _harness) : skfunc(_harness) , var_val_type(skfunc_val_type){}
        explicit VarVal(PolyVec* _poly_vec) : poly_vec(_poly_vec) , var_val_type(poly_vec_type){}
        explicit VarVal(PolyPair* _poly_pair) : poly_pair(_poly_pair) , var_val_type(poly_pair_type){}
        explicit VarVal(VarVal* _poly_val) : poly_val(_poly_val) , var_val_type(poly_val_type){}
        explicit VarVal(SolverLanguagePrimitives::SolutionHolder* _solution) : solution(_solution) , var_val_type(solution_val_type){}
        explicit VarVal(SolverLanguagePrimitives::InputHolder* _input_holder) : input_holder(_input_holder), var_val_type(input_val_type){}
//        explicit VarVal(vector<pair<int, SolverLanguagePrimitives::SolutionHolder *> >* _vector_pair_int_solution) :
//            vector_pair_int_solution(_vector_pair_int_solution), var_val_type(vector_pair_int_solution_val_type){}
//        explicit VarVal(pair<int, SolverLanguagePrimitives::SolutionHolder *> _pair_int_solution) :
//                pair_int_solution(std::move(_pair_int_solution)), var_val_type(pair_int_solution_val_type){}
        VarVal(): var_val_type(void_val_type) {}

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
                case poly_val_type:
                    return *poly_val < *other.poly_val;
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

        VarVal *eval(SolverProgramState *state) {
            return this;
        }

        bool get_bool()
        {
            assert(var_val_type == bool_val_type);
            return b;
        }

        int get_int() {
            if(var_val_type == int_val_type) {
                return i;
            }
            else if(var_val_type == bool_val_type)
            {
                return (int) b;
            }
            else
            {
                assert(false);
            }
        }

        Method *get_method() {
            assert(is_method());
            return method;
        }

        bool is_method() {
            return var_val_type == method_val_type;
        }

        string get_string() {
            assert(var_val_type == string_val_type);
            return s;
        }

        SketchFunction *get_harness() {
            assert(var_val_type == skfunc_val_type);
            return skfunc;
        }

        File *get_file() {
            assert(var_val_type == file_val_type);
            return file;
        }
        SolverLanguagePrimitives::SolutionHolder *get_solution() {
            assert(var_val_type == solution_val_type);
            return solution;
        }

        SketchFunction *get_function() {
            assert(var_val_type == skfunc_val_type);
            return skfunc;
        }

        SolverLanguagePrimitives::InputHolder *get_input_holder() {
            assert(var_val_type == input_val_type);
            return input_holder;
        }

        bool is_void(){
            return var_val_type == void_val_type;
        }

        string to_string()
        {
            switch (var_val_type) {

                case string_val_type:
                    return get_string();
                    break;
                case int_val_type:
                    return std::to_string(get_int());
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
                    return std::to_string(get_bool());
                    break;
                case void_val_type:
                    assert(false);
                    break;
                case no_type:
                    assert(false);
                    break;
                case float_val_type:
                    return std::to_string(float_val);
                    break;
                case poly_val_type:
                    return poly_val->to_string();
                    break;
                default:
                    assert(false);
            }
        }

//        vector<pair<int, SolverLanguagePrimitives::SolutionHolder*> > *get_vector_pair_int_solution() {
//            assert(var_val_type == vector_pair_int_solution_val_type);
//            return vector_pair_int_solution;
//        }
//
//        pair<int, SolverLanguagePrimitives::SolutionHolder *> get_pair() {
//            assert(var_val_type == pair_int_solution_val_type);
//            return pair_int_solution;
//        }

        float get_float() {
            assert(var_val_type == float_val_type);
            return float_val;
        }

        PolyVec* get_poly_vec() {
            assert(var_val_type == poly_vec_type);
            return poly_vec;
        }

        VarVal *get_var_val() {
            assert(var_val_type == poly_val_type);
            return poly_val;
        }

        PolyPair *get_poly_pair() {
            assert(var_val_type == poly_pair_type);
            return poly_pair;
        }

//        VarVal* get_poly_val()
//        {
//            assert(var_val_type == poly_val_type);
//            return poly_val;
//        }
    };

    class Param;

    class Params {
        Param *head = nullptr;
        Params *rest = nullptr;
    public:
        Params() = default;
        Params(Param *_head, Params *_rest = nullptr) : head(_head), rest(_rest) {};
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

    class FuncCall {
        Name *var_name = nullptr;
        union {
            Name *method_name;
            SLType *type_constructor;
        };
        vector<Param*> params;

        enum MethodMetaType {name_meta_type, type_constructor_meta_type};

        MethodMetaType method_meta_type;

    public:
        FuncCall(
                Name *_class_name, Name *_method_name, Params *_params) :
                var_name(_class_name), method_name(_method_name), method_meta_type(name_meta_type) {
            _params->populate_vector(params);
        };
        FuncCall(
                Name *_class_name, SLType *_type_constructor, Params *_params) :
                var_name(_class_name), type_constructor(_type_constructor), method_meta_type(type_constructor_meta_type) {
            _params->populate_vector(params);
        };

        Var* get_var_assert_type(SolverProgramState* state, string type_name);

        VarVal* eval(SolverProgramState* state);

        void run(SolverProgramState *pState);

        SL::VarVal *eval_type_constructor(SolverProgramState* state);
    };



    class Assignment
    {
        union {
            Var* dest_var;
            Name* dest_name;
        };
        union {
            FuncCall *func_call;
            VarVal* my_const;
            Name* from_name;
            SLType* from_type;
        };

        enum DestMetaType {var_dest_type, name_dest_type, no_dest_type};
        enum SrcMetaType {func_call_src_type, const_src_type, name_src_type, type_constructor_src_type, no_src_type};

        DestMetaType dest_type = no_dest_type;
        SrcMetaType src_type = no_src_type;

    public:

        explicit Assignment(Var* _var): dest_var(_var), dest_type(var_dest_type), src_type(no_src_type){}
        Assignment(Var* _var, FuncCall* _func_call): dest_var(_var), func_call(_func_call), dest_type(var_dest_type), src_type(func_call_src_type) {}
        Assignment(Name* _name, FuncCall* _func_call): dest_name(_name), func_call(_func_call), dest_type(name_dest_type), src_type(func_call_src_type) {}
        Assignment(Var* _var, VarVal* _my_const): dest_var(_var), my_const(_my_const), dest_type(var_dest_type), src_type(const_src_type) {}
        Assignment(Name* _name, Name* _from_name): dest_name(_name), from_name(_from_name), dest_type(name_dest_type), src_type(name_src_type) {}
        Assignment(Var* _var, Name* _from_name): dest_var(_var), from_name(_from_name), dest_type(var_dest_type), src_type(name_src_type) {}
//        Assignment(Var* _var, SLType* _from_type): dest_var(_var), from_type(_from_type), dest_type(var_dest_type), src_type(type_constructor_src_type) {}

        Var* get_var() const
        {
            assert(dest_type == var_dest_type);
            return dest_var;
        }
        void run(SolverProgramState* state);

        bool has_assignment();
    };
    class Param {
        union {
            Name *name;
            VarVal *var_val;
            Var* var;
            FuncCall* func_call;
        };
        enum ParamMetaType {is_name, is_var_val, is_var, is_func_call};
        ParamMetaType meta_type;
    public:
        explicit Param(Name *_name) : name(_name), meta_type(is_name) {};
        explicit Param(Var *_var) : var(_var), meta_type(is_var) {};
        explicit Param(Assignment *assignment) : var(assignment->get_var()), meta_type(is_var) {
            assert(!assignment->has_assignment());
        };
        explicit Param(VarVal *_var_val) : var_val(_var_val), meta_type(is_var_val) {};
        explicit Param(FuncCall *_func_call) : func_call(_func_call), meta_type(is_func_call) {};

        VarVal *eval(SolverProgramState *state);

        Var *get_var() {
            switch (meta_type) {
                case is_name:
                    assert(false);
//                    return state->get_var_val(state->name_to_var(name));
                    break;
                case is_var:
                    return var;
                    break;
                case is_var_val:
                    assert(false);
                    break;
                case is_func_call:
                    assert(false);
                    break;
                default:
                    assert(false);
            }
        }

    };


//    void set_var_val(Name *name, FuncCall *expr);
//
//    void set_var_val(Var *var, FuncCall *expr);

//    Var* name_to_var(string name);
    class Predicate;

    class Operand
    {
        union {
            Name* name;
            Predicate* predicate;
        };
        enum OperandMetaType {var_operand, predicate_operand};
        OperandMetaType meta_type;
    public:
        explicit Operand(Name* _name): name(_name), meta_type(var_operand) {
            // TODO: get var from name;
        }
        explicit Operand(Predicate* _predicate): predicate(_predicate), meta_type(var_operand) {}

        int eval(SolverProgramState* state);
    };

    enum MyOperator {lt, gt, eq};
//    class MyOperator
//    {
//        enum Op {lt, gt};
//        Op op;
//    public:
//        MyOperator(Op _op): op(_op){}
//    };

    class CompositePredicate
    {
        Operand* left_operand = nullptr;
        Operand* right_operand = nullptr;
    private:
        MyOperator op;
    public:
        CompositePredicate(MyOperator _op, Operand* _left, Operand* _right): op(_op), left_operand(_left), right_operand(_right) {};

        bool eval(SolverProgramState* state)
        {
            int left_val = left_operand->eval(state);
            int right_val = right_operand->eval(state);
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

    class Predicate
    {
        union {
            CompositePredicate* composite_predicate;
            Operand* operand;
        };
        enum PredicateMetaType {composite_type, operand_type};
        PredicateMetaType meta_type;
    public:
        explicit Predicate(Operand* _operand): operand(_operand), meta_type(operand_type) {};
        explicit Predicate(CompositePredicate* _composite_predicate): composite_predicate(_composite_predicate), meta_type(composite_type) {};

        bool eval(SolverProgramState* state)
        {
            switch (meta_type) {
                case composite_type:
                    return composite_predicate->eval(state);
                    break;
                case operand_type:
                    return operand->eval(state);
                    break;
                default:
                    assert(false);
            }
        }
    };

    class CodeBlock;

    class While
    {
        Predicate* predicate = nullptr;
        CodeBlock* body = nullptr;
    public:
        While(Predicate* _predicate, CodeBlock* _body): predicate(_predicate), body(_body) {}

        void run(SolverProgramState* state);

    };

    class UnitLine;

    class For
    {
        UnitLine* def;
        Predicate* predicate = nullptr;
        UnitLine* plus_plus;
        CodeBlock* body = nullptr;
    public:
        For(UnitLine* _def, Predicate* _predicate, UnitLine* _plus_plus, CodeBlock* _body):
            def(_def), predicate(_predicate), plus_plus(_plus_plus), body(_body) {}

        void run(SolverProgramState* state);

    };


    class If
    {
        Predicate* predicate = nullptr;
        CodeBlock* body = nullptr;
    public:
        If(Predicate* _predicate, CodeBlock* _body): predicate(_predicate), body(_body) {}

        void run(SolverProgramState* state);
    };

    class Return
    {
        Name* name = nullptr;
    public:
        explicit Return(Name* _name) : name(_name) {//TODO: get var from name;
            }

        void run(SolverProgramState* state);
    };

    class UnitLine
    {
        enum LineType {var_line, assign_line, while_line, if_line, return_line, func_call_line, for_line};
        union {
            Var* var;
            Assignment* assignment;
            While* while_loop;
            For* for_loop;
            If* if_block;
            Return* return_stmt;
            FuncCall* func_call;
        };
        LineType line_type;
    public:
        explicit UnitLine(Var* _var): var(_var), line_type(var_line){}
        explicit UnitLine(Assignment* _assignment): assignment(_assignment), line_type(assign_line){}
        explicit UnitLine(While* _while_loop): while_loop(_while_loop), line_type(while_line){}
        explicit UnitLine(If* _if_block): if_block(_if_block), line_type(if_line){}
        explicit UnitLine(Return* _return_stmt): return_stmt(_return_stmt), line_type(return_line){}
        explicit UnitLine(FuncCall* _func_call): func_call(_func_call), line_type(func_call_line){}
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
                case func_call_line:
                    func_call->run(state);
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

        VarVal *eval(SolverProgramState* state, vector<Param*>& params);

        Var* get_var()
        {
            return var;
        }
    };

    bool var_val_invariant(SL::SLType *var_type, SL::VarVal *var_val);
    bool var_val_invariant(SL::Var *var, SL::VarVal *var_val);

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
