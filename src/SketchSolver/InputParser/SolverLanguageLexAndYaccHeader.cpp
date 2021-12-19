//
// Created by kliment on 12/19/21.
//

#include "SolverLanguageLexAndYaccHeader.h"
#include "SolverLanguageYaccHeader.h"

void Var::run(SolverProgramState *state)  {
    state->add_var(this);
}

VarVal *Var::eval(SolverProgramState *state) {
    return state->get_var_val(this);
}

void While::run(SolverProgramState* state)
{
    while(predicate->eval(state))
    {
        cout << "LOOPING" << endl;
        body->run(state);
        assert(false);
    }
    assert(false);

}

int Operand::eval(SolverProgramState* state)
{
    switch (meta_type) {
        case var_operand:
            return state->get_var_val(state->name_to_var(name))->get_int();
            break;
        case predicate_operand:
            return predicate->eval(state);
            break;
        default:
            assert(false);
    }
}

void If::run(SolverProgramState *state) {
    if(predicate->eval(state))
    {
        body->run(state);
    }
}

void Return::run(SolverProgramState *state)
{
    state->set_return_var_val(state->name_to_var(name)->eval(state));
}

void Assignment::run(SolverProgramState *state)
{
    Var* to_var = nullptr;
    switch (dest_type) {
        case name_dest_type:
            to_var = state->name_to_var(dest_name);
            break;
        case var_dest_type:
            to_var = dest_var;
            break;
        default:
            assert(false);
    }
    assert(to_var != nullptr);

    VarVal* var_val = nullptr;
    switch (src_type) {
        case func_call_src_type:
            var_val = func_call->eval(state);
            break;
        case const_src_type:
            var_val = my_const->eval(state);
            break;
        case name_src_type:
            var_val = from_name->eval(state);
            break;
        case no_src_type:
            var_val = nullptr;
            break;
    }

    state->set_var_val(to_var, var_val);

}

bool Assignment::has_assignment() {
    return src_type != no_src_type;
}


SL_LY::VarVal *Name::eval(SolverProgramState *state)  {
    return state->get_var_val(state->name_to_var(this));
}

VarVal *FuncCall::eval(SolverProgramState *state)
{

    enum PredefMethod {no_predef, predef_file};

    PredefMethod predef_method = no_predef;

    string method_str = method_name->to_string();

    if(method_str == "File")
    {
        predef_method = predef_file;
    }

    switch (predef_method) {
        case predef_file:
            assert(params.size() == 1);
            return new VarVal(new ofstream(params[0]->eval(state)->get_string()));
            break;
        case no_predef:
            return state->get_method(state->method_name_to_var(method_name))->eval(state, params);
        default:
            assert(false);
    }
    return nullptr;
}

VarVal *Method::eval(SolverProgramState *state, vector<Param*>& input_params)  {
    run(state, input_params);
    return state->get_return_var_val();
}

void Method::run(SolverProgramState *state, vector<Param *> &input_params)  {
    assert(var != nullptr);
    assert(body != nullptr);

    state->new_stack_frame(params, input_params);

    body->run(state);

    state->pop_stack_frame();

}

void Method::add_to_map(Frame &frame)  {
    assert(var != nullptr);
    frame.set_var_val(var, new VarVal(this));
}

VarVal *Param::eval(SolverProgramState *state)  {
    switch (meta_type) {
        case is_name:
            return state->get_var_val(state->name_to_var(name));
            break;
        case is_var:
            return state->get_var_val(var);
            break;
        case is_var_val:
            return var_val;
            break;
        default:
            assert(false);
    }
}

