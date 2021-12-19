//
// Created by kliment on 12/17/21.
//

#ifndef SOLVERLANGUAGEPARSER_LEXANDYACCHEADER_H
#define SOLVERLANGUAGEPARSER_LEXANDYACCHEADER_H

#include <iostream>
#include <cassert>
#include <string>
#include <utility>

namespace SL_LY {

    using namespace std;

    class Name {
        string name;
    public:
        explicit Name(string _name) : name(std::move(_name)) {};

        string get_name() {
            return name;
        }
    };

    class Var {
        Name *type = nullptr;
        Name *name = nullptr;
    public:
        Var(Name *_type, Name *_name) : type(_type), name(_name) {

            cout << name->get_name() << " " << type->get_name() << endl;

        };

        Name *get_name() {
            cout << "ret name: " << name->get_name() << endl;
            return name;
        }
    };

    class Const {
        union {
            string s;
            int i;
        };
    public:
        explicit Const(string _s) : s(std::move(_s)) {

        }
        explicit Const(int _i) : i(_i) {

        }
    };

    class Param;

    class Params {
        Param *head = nullptr;
        Params *rest = nullptr;
    public:
        Params() = default;;
        Params(Param *_head, Params *_rest = nullptr) : head(_head), rest(_rest) {};
    };

    class FuncCall {
        Name *class_name = nullptr;
        Name *method_name = nullptr;
        Params *params = nullptr;
    public:
        FuncCall(
                Name *_class_name, Name *_method_name, Params *_params) :
                class_name(_class_name), method_name(_method_name), params(_params) {
            if (method_name->get_name() == "concretize") {
                cout << "NOW CONCRETIZING" << endl;
            }
        };

    };


    class Assignment
    {
        union {

            Var* var;
            Name* name;
        };
        union {
            FuncCall *func_call;
            Const* my_const;
            Name* from_name;
        };
    public:

        explicit Assignment(Var* _var): var(_var){
//            set_var_val(var, func_call);
        }
        Assignment(Var* _var, FuncCall* _func_call): var(_var), func_call(_func_call){
//            set_var_val(var, func_call);
        }
        Assignment(Name* _name, FuncCall* _func_call): name(_name), func_call(_func_call){
              //TODO: get var from name;
//            set_var_val(var, func_call);
        }
        Assignment(Var* _var, Const* _my_const): var(_var), my_const(_my_const){
            //TODO: get var from name;
//            set_var_val(var, func_call);
        }
        Assignment(Name* _name, Name* _from_name): name(_name), from_name(_from_name){
            //TODO: get var from name;
//            set_var_val(var, func_call);
        }
        Var* get_var() const
        {
            return var;
        }
    };
    class Param {
        union {
            Name *identifier;
            Const *constant;
            Var* var;
        };
    public:
        explicit Param(Name *_id) : identifier(_id) {};
        explicit Param(Var *_var) : var(_var) {};
        explicit Param(Assignment *assignment) : var(assignment->get_var()) {};
        explicit Param(Const *_const) : constant(_const) {};
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
    public:
        explicit Operand(Name* _name): name(_name) {
            // TODO: get var from name;
        }
        explicit Operand(Predicate* _predicate): predicate(_predicate) {}
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
    };

    class Predicate
    {
        union {
            CompositePredicate* composite_predicate;
            Operand* operand;
        };
    public:
        explicit Predicate(Operand* _operand): operand(_operand) {};
        explicit Predicate(CompositePredicate* _composite_predicate): composite_predicate(_composite_predicate) {};
    };

    class CodeBlock;

    class While
    {
        Predicate* predicate = nullptr;
        CodeBlock* body = nullptr;
    public:
        While(Predicate* _predicate, CodeBlock* _body): predicate(_predicate), body(_body) {}

    };

    class If
    {
        Predicate* predicate = nullptr;
        CodeBlock* body = nullptr;
    public:
        If(Predicate* _predicate, CodeBlock* _body): predicate(_predicate), body(_body) {}
    };

    class Return
    {
        Name* name = nullptr;
    public:
        explicit Return(Name* _name) : name(_name) {//TODO: get var from name;
            }
    };

    class UnitLine
    {
        union {
            Var* var;
            Assignment* assignment;
            While* while_loop;
            If* if_block;
            Return* return_stmt;
        };
    public:
        explicit UnitLine(Var* _var): var(_var){}
        explicit UnitLine(Assignment* _assignment): assignment(_assignment){}
        explicit UnitLine(While* _while_loop): while_loop(_while_loop){}
        explicit UnitLine(If* _if_block): if_block(_if_block){}
        explicit UnitLine(Return* _return_stmt): return_stmt(_return_stmt){}
    };

    class CodeBlock
    {
        UnitLine* head = nullptr;
        CodeBlock* rest = nullptr;
    public:
        explicit CodeBlock(UnitLine* _head): head(_head) {}
        CodeBlock(UnitLine* _head, CodeBlock* _rest): head(_head), rest(_rest) {}
    };

    class Method
    {
        CodeBlock* body = nullptr;
        Var* var = nullptr;
        Params* params = nullptr;
    public:
        Method(Var* _var, Params* _params, CodeBlock* _body): var(_var), body(_body), params(_params)
        {

        }
    };

    class Methods
    {
        Method* head = nullptr;
        Methods* rest = nullptr;
    public:
        explicit Methods(Method* _head): head(_head) {}
        Methods(Method* _head, Methods* _rest): head(_head), rest(_rest) {}
    };





    void run_solver_langauge_program();

};

using namespace SL_LY;

#endif //SOLVERLANGUAGEPARSER_LEXANDYACCHEADER_H
