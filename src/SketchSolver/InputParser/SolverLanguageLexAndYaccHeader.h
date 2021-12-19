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

using namespace std;

class SolverProgramState;
class Frame;

class Harness;
class FloatManager;
class CommandLineArgs;
class HoleHardcoder;

namespace SL_LY {

    class VarVal;

    class Name {
        string name;
    public:
        explicit Name(string _name) : name(std::move(_name)) {};

        string to_string() {
            return name;
        }

        SL_LY::VarVal *eval(SolverProgramState *state);

        bool operator < (const Name& other) const
        {
            return name < other.name;
        }

    };

    class Var {
        Name *type = nullptr;
        Name *name = nullptr;
    public:
        Var(Name *_type, Name *_name) : type(_type), name(_name) {
        };

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
    };

    class Method;

    class VarVal {
        union {
            string s;
            int i;
            ofstream* file;
            Method* method;
            Harness* harness;
        };
        enum VarValType {string_val_type, int_val_type, file_val_type, method_val_type, harness_val_type};
        VarValType var_val_type;
    public:
        explicit VarVal(string _s) : s(std::move(_s)), var_val_type(string_val_type) {}
        explicit VarVal(int _i) : i(_i) , var_val_type(int_val_type){}
        explicit VarVal(ofstream* _file) : file(_file) , var_val_type(file_val_type){}
        explicit VarVal(Method* _method) : method(_method) , var_val_type(method_val_type){}
        explicit VarVal(Harness* _harness) : harness(_harness) , var_val_type(harness_val_type){}

        VarVal *eval(SolverProgramState *state) {
            return this;
        }

        int get_int() {
            assert(var_val_type == int_val_type);
            return i;
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
        Name *method_name = nullptr;
        vector<Param*> params;
    public:
        FuncCall(
                Name *_class_name, Name *_method_name, Params *_params) :
                var_name(_class_name), method_name(_method_name) {
            _params->populate_vector(params);
        };

        VarVal* eval(SolverProgramState* state);

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
        };

        enum DestMetaType {var_dest_type, name_dest_type};
        enum SrcMetaType {func_call_src_type, const_src_type, name_src_type, no_src_type};

        DestMetaType dest_type;
        SrcMetaType src_type;

    public:

        explicit Assignment(Var* _var): dest_var(_var), dest_type(var_dest_type), src_type(no_src_type){
//            set_var_val(var, func_call);
        }
        Assignment(Var* _var, FuncCall* _func_call): dest_var(_var), func_call(_func_call), dest_type(var_dest_type), src_type(func_call_src_type) {
//            set_var_val(var, func_call);
        }
        Assignment(Name* _name, FuncCall* _func_call): dest_name(_name), func_call(_func_call), dest_type(name_dest_type), src_type(func_call_src_type) {
              //TODO: get var from name;
//            set_var_val(var, func_call);
        }
        Assignment(Var* _var, VarVal* _my_const): dest_var(_var), my_const(_my_const), dest_type(var_dest_type), src_type(const_src_type) {
            //TODO: get var from name;
//            set_var_val(var, func_call);
        }
        Assignment(Name* _name, Name* _from_name): dest_name(_name), from_name(_from_name), dest_type(name_dest_type), src_type(name_src_type) {
            //TODO: get var from name;
//            set_var_val(var, func_call);
        }
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
        };
        enum ParamMetaType {is_name, is_var_val, is_var};
        ParamMetaType meta_type;
    public:
        explicit Param(Name *_name) : name(_name), meta_type(is_name) {};
        explicit Param(Var *_var) : var(_var), meta_type(is_var) {};
        explicit Param(Assignment *assignment) : var(assignment->get_var()), meta_type(is_var) {
            assert(!assignment->has_assignment());
        };
        explicit Param(VarVal *_var_val) : var_val(_var_val), meta_type(is_var_val) {};

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
        enum LineType {var_line, assign_line, while_line, if_line, return_line};
        union {
            Var* var;
            Assignment* assignment;
            While* while_loop;
            If* if_block;
            Return* return_stmt;
        };
        LineType line_type;
    public:
        explicit UnitLine(Var* _var): var(_var), line_type(var_line){}
        explicit UnitLine(Assignment* _assignment): assignment(_assignment), line_type(assign_line){}
        explicit UnitLine(While* _while_loop): while_loop(_while_loop), line_type(while_line){}
        explicit UnitLine(If* _if_block): if_block(_if_block), line_type(if_line){}
        explicit UnitLine(Return* _return_stmt): return_stmt(_return_stmt), line_type(return_line){}

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
    };

    class Methods
    {
        Method* head = nullptr;
        Methods* rest = nullptr;
    public:
        explicit Methods(Method* _head): head(_head) {}
        Methods(Method* _head, Methods* _rest): head(_head), rest(_rest) {}

        void populate_state(Frame& frame) {
            Methods* at = this;
            while(at != nullptr)
            {
                at->head->add_to_map( frame);
                at = at->rest;
            }
        }


    };
};


typedef void* yyscan_t;
void yyerror(yyscan_t scanner, SolverProgramState* state, string s);


using namespace SL_LY;

#endif //SOLVERLANGUAGEPARSER_LEXANDYACCHEADER_H
