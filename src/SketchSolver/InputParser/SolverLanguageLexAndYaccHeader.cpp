//
// Created by kliment on 12/19/21.
//

#include "SolverLanguageLexAndYaccHeader.h"
#include "SolverLanguageYaccHeader.h"
#include "SketchFunction.h"
#include "File.h"
#include "SolverLanguage.h"

void SL::Var::run(SolverProgramState *state)  {
    state->add_var(this);
}

SL::VarVal *SL::Var::eval(SolverProgramState *state) {
    return state->get_var_val(this);
}

SL::SLType * SL::Var::get_type() {
    return type;
}


void SL::While::run(SolverProgramState* state)
{
    while(expression->eval(state)->get_bool())
    {
        body->run(state);
    }
}
void SL::For::run(SolverProgramState* state)
{
    def->run(state);
    while(expression->eval(state)->get_bool())
    {
        body->run(state);
        plus_plus->run(state);
    }
}

void SL::If::run(SolverProgramState *state) {
    if(expression->eval(state)->get_bool())
    {
        body->run(state);
    }
}

void SL::Return::run(SolverProgramState *state)
{
    state->set_return_var_val(expression->eval(state));
}

void SL::Assignment::run(SolverProgramState *state)
{
    Var* to_var = nullptr;
    switch (dest_type) {
        case name_dest_type:
            to_var = state->name_to_var(dest_name);
            break;
        case var_dest_type:
            to_var = dest_var;
            state->add_var(to_var);
            break;
        default:
            assert(false);
    }
    assert(to_var != nullptr);

    SL::VarVal* var_val = nullptr;
    if(expression != nullptr)
    {
        var_val = expression->eval(state);
        state->set_var_val(to_var, var_val);
    }
    else
    {
        assert(dest_type == var_dest_type);;
    }
}

bool SL::Assignment::has_assignment() {
    return expression != nullptr;
}


SL::VarVal *SL::Name::eval(SolverProgramState *state)  {
    return state->get_var_val(state->name_to_var(this));
}

SL::VarVal *SL::FuncCall::eval_type_constructor(SolverProgramState* state)
{
    string root_type_name = type_constructor->get_head()->to_string();

    enum RootType {vec, pair, do_predef};

    RootType root_type = do_predef;

    if(root_type_name == "vector")
    {
        root_type = vec;
    }
    if(root_type_name == "pair")
    {
        root_type = pair;
    }

    switch (root_type) {
        case vec: {
            const vector<SL::SLType *>* type_params = type_constructor->get_type_params();
            assert(type_params->size() == 1);
            assert(params.empty());
            return new SL::VarVal(new PolyVec(type_params));
            break;
        }
        case pair: {
            const vector<SL::SLType *>* type_params = type_constructor->get_type_params();
            assert(type_params->size() == 2);
            assert(params.size() == 2);
            VarVal* left = params[0]->eval(state);
            VarVal* right = params[1]->eval(state);
            return new SL::VarVal(new PolyPair(type_params, left, right));
            break;
        }
        case do_predef:
            assert(false);
            break;
    }
}

SL::VarVal *SL::FuncCall::eval(SolverProgramState *state)
{
    enum PredefMethod {
        no_predef,
        predef_file, produce_subset_file,
        sat_solver, produce_concretization,
        concretize, size, get,
        passes, plus, clear, Solution, join,
        print, num_holes, emplace_back,
        first, second, div,
        mult, to_float, sort_vec,
        my_clone, my_assert, reverse};

    PredefMethod predef_method = no_predef;

    string method_str;

    switch (method_meta_type) {

        case name_meta_type:
            method_str = method_name->to_string();
            break;
        case type_constructor_meta_type:
            method_str = type_constructor->to_string();
            break;
        default:
            assert(false);
    }

    assert(method_str != "");

    if(method_str == "File")
    {
        predef_method = predef_file;
    }
    else if(method_str == "produce_subset_file")
    {
        predef_method = produce_subset_file;
    }
    else if(method_str == "SATSolver")
    {
        predef_method = sat_solver;
    }
    else if(method_str == "produce_concretization")
    {
        predef_method = produce_concretization;
    }
    else if(method_str == "concretize")
    {
        predef_method = concretize;
    }
    else if(method_str == "size")
    {
        predef_method = size;
    }
    else if(method_str == "get")
    {
        predef_method = get;
    }
    else if(method_str == "passes")
    {
        predef_method = passes;
    }
    else if(method_str == "plus")
    {
        predef_method = plus;
    }
    else if(method_str == "clear")
    {
        predef_method = clear;
    }
    else if(method_str == "Solution")
    {
        predef_method = Solution;
    }
    else if(method_str == "join")
    {
        predef_method = join;
    }
    else if(method_str == "print")
    {
        predef_method = print;
    }
    else if(method_str == "num_holes")
    {
        predef_method = num_holes;
    }
    else if(method_str == "emplace_back")
    {
        predef_method = emplace_back;
    }
    else if(method_str == "first")
    {
        predef_method = first;
    }
    else if(method_str == "second")
    {
        predef_method = second;
    }
    else if(method_str == "div")
    {
        predef_method = div;
    }
    else if(method_str == "mult")
    {
        predef_method = mult;
    }
    else if(method_str == "float")
    {
        predef_method = to_float;
    }
    else if(method_str == "sort")
    {
        predef_method = sort_vec;
    }
    else if(method_str == "clone")
    {
        predef_method = my_clone;
    }
    else if(method_str == "assert")
    {
        predef_method = my_assert;
    }
    else if(method_str == "reverse")
    {
        predef_method = reverse;
    }



    switch (predef_method) {
        case predef_file:
        {
            pair<SL::Var*, SL::VarVal*> var_and_var_val = get_var_assert_type(state, "namespace");
            SL::Var* var = var_and_var_val.first;
            assert(var == nullptr);
            assert(params.size() == 2);
            string file_name = params[0]->eval(state)->get_string();
            SketchFunction* harness = params[1]->eval(state)->get_harness();
            return new SL::VarVal(new File(harness, file_name, state->floats, state->args.seed));
            break;
        }
        case produce_subset_file:
        {
            pair<SL::Var*, SL::VarVal*> var_and_var_val = get_var_assert_type(state, "File");
            assert(params.size() == 1);
            int num_rows = params[0]->eval(state)->get_int();
            File* file = var_and_var_val.second->get_file();
            return new SL::VarVal(file->sample_sub_file(num_rows));
            break;
        }
        case sat_solver:
        {
            pair<SL::Var*, SL::VarVal*> var_and_var_val = get_var_assert_type(state, "namespace");
            SL::Var* var = var_and_var_val.first;
            assert(var == nullptr);
            assert(params.size() == 2);
            SketchFunction* harness = params[0]->eval(state)->get_harness()->produce_inlined_dag();
            File* file = params[1]->eval(state)->get_file();
            using namespace SolverLanguagePrimitives;
            WrapperAssertDAG* solver =
                    new WrapperAssertDAG(state->floats, state->hc, state->args, state->hasGoodEnoughSolution);
            assert(file->get_counterexample_ids_over_time().empty());
            SolutionHolder* sol = (solver)->
                    solve(new ProblemAE(harness, file));
            harness->clear();
            solver->clear();
            return new SL::VarVal(sol);
            break;
        }
        case size:
        {
            Name* var_name = expression->get_var_name();
            assert(var_name != nullptr);
            Var* var = state->name_to_var(var_name);
            string var_head_type_str;
            VarVal* var_val = nullptr;
            if(var->has_type()) {
                var_head_type_str = var->get_type()->get_head()->to_string();
            }
            else
            {
                assert(!SL::is_strongly_typed);
                var_val = expression->eval(state);
                var_head_type_str = var_val->get_type_string();
            }
            assert(var_head_type_str == "File" || var_head_type_str == "vector");

            if(var_val == nullptr)
            {
                var_val = expression->eval(state);
            }

            assert(params.empty());
            if(var_head_type_str == "File") {
                File *file = expression->eval(state)->get_file();
                return new SL::VarVal((int) file->size());
            } else if (var_head_type_str == "vector")
            {
                PolyVec* poly_vec = expression->eval(state)->get_poly_vec();
                return new SL::VarVal((int)poly_vec->size());
            }
            else
            {
                assert(false);
            }
            break;
        }
        case produce_concretization:
        {
            pair<SL::Var*, SL::VarVal*> var_and_var_val = get_var_assert_type(state, "SketchFunction");
            assert(params.size() == 1);
            using namespace SolverLanguagePrimitives;
            SolutionHolder* sol = params[0]->eval(state)->get_solution();
            SketchFunction* harness = var_and_var_val.second->get_harness();
            SketchFunction* concretized_function =
                    harness->produce_with_concretized_holes(sol, true);
            return new SL::VarVal(concretized_function);
            break;
        }
        case concretize:
        {
            pair<SL::Var*, SL::VarVal*> var_and_var_val = get_var_assert_type(state, "SketchFunction");
            assert(params.size() == 1);
            using namespace SolverLanguagePrimitives;
            SolutionHolder* sol = params[0]->eval(state)->get_solution();
            SketchFunction* harness = var_and_var_val.second->get_harness();
            harness = harness->concretize(sol, true);
            return new SL::VarVal(harness);
            break;
        }
        case get:
        {
            Name* var_name = expression->get_var_name();
            assert(var_name != nullptr);
            Var* var = state->name_to_var(var_name);
            VarVal* var_val = nullptr;
            string var_type_str;
            if(var->has_type()) {
                var_type_str = var->get_type()->get_head()->to_string();
            }
            else {
                assert(!SL::is_strongly_typed);
                var_val = expression->eval(state);
                var_type_str = var_val->get_type_string();
            }
            assert(var_type_str == "File" || var_type_str == "vector");
            if(var_val == nullptr)
            {
                var_val = expression->eval(state);
            }
            if (var_type_str == "File") {
                assert(params.size() == 1);
                File *file = var_val->get_file();
                int row_id = params[0]->eval(state)->get_int();
                return new SL::VarVal(new SolverLanguagePrimitives::InputHolder(file->at(row_id), state->floats));
            } else if (var_type_str == "vector") {
                if(var->has_type()) {
                    assert(var->get_type()->get_type_params()->size() == 1);
                }
                 assert(params.size() == 1);
                PolyVec *vec = var_val->get_poly_vec();
                int idx = params[0]->eval(state)->get_int();
                return vec->at(idx);
            }
            else
            {
                assert(false);
            }
            break;
        }
        case passes:
        {
            pair<SL::Var*, SL::VarVal*> var_and_var_val = get_var_assert_type(state, "SketchFunction");
            assert(params.size() == 1);
            SketchFunction* program = var_and_var_val.second->get_function();
            SolverLanguagePrimitives::InputHolder* input_holder = params[0]->eval(state)->get_input_holder();
            SketchFunction* concretized_function = program->produce_with_concretized_inputs(input_holder);
            assert((concretized_function->get_dag()->size() == 0) == (concretized_function->get_dag()->get_failed_assert() == nullptr));
            bool ret = concretized_function->get_dag()->get_failed_assert() == nullptr;
            concretized_function->clear();
            return new VarVal(ret);
            break;
        }
        case plus:
        {
            pair<SL::Var*, SL::VarVal*> var_and_var_val = get_var_assert_type(state, "namespace");
            SL::Var* var = var_and_var_val.first;
            assert(var == nullptr);
            assert(params.size() == 2);
            int left_op = params[0]->eval(state)->get_int();
            int right_op = params[1]->eval(state)->get_int();
            return new VarVal(left_op+right_op);
            break;
        }
        case div:
        {
            pair<SL::Var*, SL::VarVal*> var_and_var_val = get_var_assert_type(state, "namespace");
            SL::Var* var = var_and_var_val.first;
            assert(var == nullptr);
            assert(params.size() == 2);
            float left_op = params[0]->eval(state)->get_float();
            float right_op = params[1]->eval(state)->get_float();
            return new VarVal(left_op/right_op);
            break;
        }
        case mult:
        {
            pair<SL::Var*, SL::VarVal*> var_and_var_val = get_var_assert_type(state, "namespace");
            SL::Var* var = var_and_var_val.first;
            assert(var == nullptr);
            assert(params.size() == 2);
            float left_op = params[0]->eval(state)->get_float();
            float right_op = params[1]->eval(state)->get_float();
            return new VarVal(left_op*right_op);
            break;
        }
        case clear:
        {
            Name* var_name = expression->get_var_name();
            assert(var_name != nullptr);
            Var* var = state->name_to_var(var_name);
            VarVal* var_val = nullptr;
            string var_type_str;
            if(var->has_type()) {
                var_type_str = var->get_type()->get_head()->to_string();
            }
            else {
                assert(!SL::is_strongly_typed);
                var_val = expression->eval(state);
                var_type_str = var_val->get_type_string();
            }
            assert(var_type_str == "File" || var_type_str == "SketchFunction");
            if(var_val == nullptr)
            {
                var_val = expression->eval(state);
            }
            if (var_type_str == "File") {
                assert(params.empty());
                File* file = var_val->get_file();
                file->clear();
                delete file;
                return new VarVal();
            } else if (var_type_str == "SketchFunction") {
                assert(params.empty());
                SketchFunction* program = var_val->get_function();
                program->clear();
                return new VarVal();
            }
            else
            {
                assert(false);
            }

            break;
        }
        case Solution:
        {
            pair<SL::Var*, SL::VarVal*> var_and_var_val = get_var_assert_type(state, "namespace");
            SL::Var* var = var_and_var_val.first;
            assert(var == nullptr);
            assert(params.empty());
            using namespace SolverLanguagePrimitives;
            return new VarVal(new SolutionHolder(false));
        }
        case join:
        {
            pair<SL::Var*, SL::VarVal*> var_and_var_val = get_var_assert_type(state, "Solution");
            assert(params.size() == 1);
            using namespace SolverLanguagePrimitives;
            SolutionHolder* other_solution = params[0]->eval(state)->get_solution();
            SolutionHolder* base_solution = var_and_var_val.second->get_solution();
            base_solution->join_with(other_solution);
            return new VarVal();
        }
        case print:
        {
            pair<SL::Var*, SL::VarVal*> var_and_var_val = get_var_assert_type(state, "namespace");
            SL::Var* var = var_and_var_val.first;
            assert(var == nullptr);
            assert(!params.empty());
            string print_str = "";
            cout << "SOLVER_PROGRAM_PRINT INIT" << endl;
            for(int i = 0;i<params.size();i++)
            {
                cout << params[i]->eval(state)->to_string();
            }
            cout << endl << "SOLVER_PROGRAM_PRINT END" << endl;
            return new VarVal();
        }
        case num_holes: {
            pair<SL::Var*, SL::VarVal*> var_and_var_val = get_var_assert_type(state, "SketchFunction");
            assert(params.empty());
            SketchFunction* func_clone = var_and_var_val.second->get_harness()->clone()->do_inline();
            int num_ctrls = (int) func_clone->get_dag()->getNodesByType(bool_node::CTRL).size();
            func_clone->clear();
            return new VarVal(num_ctrls);
            break;
        }
        case emplace_back:
        {
            pair<SL::Var*, SL::VarVal*> var_and_var_val = get_var_assert_type(state, "vector");
            assert(params.size() == 1);
            PolyVec* poly_vec = var_and_var_val.second->get_poly_vec();
            VarVal* ret_param = params[0]->eval(state);
            poly_vec->emplace_back(ret_param);
            return new VarVal();
            break;
        }
        case first:
        {
            pair<SL::Var*, SL::VarVal*> var_and_var_val = get_var_assert_type(state, "pair");
            assert(params.empty());
            PolyPair* the_pair = var_and_var_val.second->get_poly_pair();
            return the_pair->first();
            break;
        }
        case second:
        {
            pair<SL::Var*, SL::VarVal*> var_and_var_val = get_var_assert_type(state, "pair");
            assert(params.empty());
            using namespace SolverLanguagePrimitives;
            PolyPair* the_pair = var_and_var_val.second->get_poly_pair();
            return the_pair->second();
            break;
        }
        case to_float:
        {
            pair<SL::Var*, SL::VarVal*> var_and_var_val = get_var_assert_type(state, "namespace");
            SL::Var* var = var_and_var_val.first;
            assert(var == nullptr);
            assert(params.size() == 1);
            float the_float = (float)params[0]->eval(state)->get_int();
            return new VarVal(the_float);
            break;
        }
        case sort_vec:
        {
            pair<SL::Var*, SL::VarVal*> var_and_var_val = get_var_assert_type(state, "vector");
            assert(params.empty());
            PolyVec* vec = var_and_var_val.second->get_poly_vec();
            vec->sort();
            return new VarVal();
            break;
        }
        case my_clone:
        {
            Name* var_name = expression->get_var_name();
            assert(var_name != nullptr);
            Var* var = state->name_to_var(var_name);
            assert(params.empty());
            string var_type_str;
            VarVal* var_val = nullptr;
            if(var->has_type()) {
                var_type_str = var->get_type()->get_head()->to_string();
            }
            else
            {
                assert(!SL::is_strongly_typed);
                var_val = expression->eval(state);
                var_type_str = var_val->get_type_string();
            }
            assert(var_type_str == "SketchFunction" || var_type_str == "Solution");

            if(var_val == nullptr)
            {
                var_val = expression->eval(state);
            }

            return new VarVal(var_val->get_harness()->clone());

            break;
        }
        case my_assert:
        {
            pair<SL::Var*, SL::VarVal*> var_and_var_val = get_var_assert_type(state, "namespace");
            SL::Var* var = var_and_var_val.first;
            assert(var == nullptr);
            assert(!params.empty());
            bool assert_true = params[0]->eval(state)->get_bool();
            string message_suffix = "\nassert(false); // triggered in Solver Program";
            if(params.size() == 1)
            {
                AssertDebug(assert_true, message_suffix);
            }
            else if(params.size() == 2)
            {
                AssertDebug(assert_true, "MESSAGE: " + params[1]->eval(state)->get_string() + message_suffix);
            }
            else
            {
                assert(false);
            }
            return new VarVal();
            break;
        }
        case reverse:
        {
            pair<SL::Var*, SL::VarVal*> var_and_var_val = get_var_assert_type(state, "vector");
            assert(params.empty());
            var_and_var_val.second->get_poly_vec()->reverse();
            return new VarVal();
        }
        case no_predef: {
            switch (method_meta_type) {
                case name_meta_type:
                    return state->get_method(state->method_name_to_var(method_name))->eval(state, params);
                    break;
                case type_constructor_meta_type:
                    return eval_type_constructor(state);
                    break;
            }
            break;
        }
        default:
            assert(false);
    }
    assert(false);
    return nullptr;
}

void SL::FuncCall::run(SolverProgramState *state) {
    SL::VarVal* ret = eval(state);
    assert(ret->is_void());
}

pair<SL::Var *, SL::VarVal*> SL::FuncCall::get_var_assert_type(SolverProgramState* state, string type_name) {

    if(type_name == "namespace") {
        assert(expression == nullptr);
        return make_pair(nullptr, nullptr);
    }
    Name* var_name = expression->get_var_name();
    if(var_name == nullptr) {
        return make_pair(nullptr, expression->eval(state));
    }
    Var* var = state->name_to_var(var_name);
    string var_type_str;
    VarVal* var_val = nullptr;
    if(var->has_type())
    {
        var_type_str = var->get_type()->get_head()->to_string();
    }
    else
    {
        assert(!SL::is_strongly_typed);
        var_val = expression->eval(state);
        var_type_str = var_val->get_type_string();
    }
    assert(var_type_str == type_name);
    if(var_val == nullptr)
    {
        var_val = expression->eval(state);
    }

    assert(var_val != nullptr);
    return make_pair(var, var_val);
}

SL::VarVal *SL::Method::eval(SolverProgramState *state, vector<Param*>& input_params)  {
    run(state, input_params);
    return state->get_return_var_val();
}

void SL::Method::run(SolverProgramState *state, vector<Param *> &input_params)  {
    assert(var != nullptr);
    assert(body != nullptr);

    state->new_stack_frame(params, input_params);

    body->run(state);

    state->pop_stack_frame();

}

SL::VarVal *SL::Param::eval(SolverProgramState *state)  {
    switch (meta_type) {
        case is_expression:
            return expression->eval(state);
            break;
        case is_var:
            return state->get_var_val(var);
            break;
        default:
            assert(false);
    }
}

void SL::Methods::populate_state(Frame &frame)  {
    Methods* at = this;
    while(at != nullptr)
    {
        frame.add_var_and_set_var_val(at->head->get_var(), new SL::VarVal(at->head));
        at = at->rest;
    }
}

bool SL::var_val_invariant(SL::SLType *var_type, SL::VarVal *var_val)
{
    SL::VarValType var_val_type = var_val->get_type();
    if (var_type->is_defined()) {
        string var_type_str = var_type->get_head()->to_string();
        if (var_type->is_simple_type()) {
            if (var_type_str == "File") {
                assert(var_val_type == SL::file_val_type);
            } else if (var_type_str == "int") {
                assert(var_val_type == SL::int_val_type);
            } else if (var_type_str == "string") {
                assert(var_val_type == SL::string_val_type);
            } else if (var_type_str == "method") {
                assert(false);
            } else if (var_type_str == "SketchFunction") {
                assert(var_val_type == SL::skfunc_val_type);
            } else if (var_type_str == "Solution") {
                assert(var_val_type == SL::solution_val_type);
            } else if (var_type_str == "Program") {
                assert(var_val_type == SL::skfunc_val_type);
            } else if (var_type_str == "Input") {
                assert(var_val_type == SL::input_val_type);
            } else if (var_type_str == "bool") {
                assert(var_val_type == SL::bool_val_type);
            } else {
                assert(false);
            }
        } else {
            if (var_type_str == "vector") {
                assert(var_val_type == SL::poly_vec_type);
                const vector<SLType *> *type_params = var_type->get_type_params();
                assert(type_params->size() == 1);
                assert(*type_params->at(0) == *var_val->get_poly_vec()->get_type_params()->at(0));
            } else if (var_type_str == "pair") {
                assert(var_val_type == SL::poly_pair_type);
                const vector<SLType *> *type_params = var_type->get_type_params();
                assert(type_params->size() == 2);
                assert(*type_params->at(0) == (*var_val->get_poly_pair()->get_type_params()->at(0)));
                assert(*type_params->at(1) == (*var_val->get_poly_pair()->get_type_params()->at(1)));
            } else {
                assert(false);
            }
        }
    }
    else
    {
        assert(!SL::is_strongly_typed);
    }
    return true;
}

bool SL::var_val_invariant(SL::Var *var, SL::VarVal *var_val)
{
    return var_val_invariant(var->get_type(), var_val);
}


void SL::PolyVec::emplace_back(SL::VarVal *new_element)
{
    assert(SL::var_val_invariant(get_type_params()->at(0), new_element));
    vector<VarVal*>::emplace_back(new_element);
}

bool comparePtrToNode(SL::VarVal* a, SL::VarVal* b) { return (*a < *b); }

void SL::PolyVec::sort() {
    ::sort(begin(), end(), comparePtrToNode);
}

void SL::PolyVec::reverse() {
    ::reverse(begin(), end());
}

SL::PolyPair::PolyPair(const vector<SLType *> *_type_params, SL::VarVal *left, SL::VarVal *right) :
        PolyType(_type_params), pair<VarVal*, VarVal*>(left, right){
    assert(get_type_params()->size() == 2);
    assert(var_val_invariant(get_type_params()->at(0), left));
    assert(var_val_invariant(get_type_params()->at(1), right));
}

bool SL::PolyPair::operator<(const SL::PolyPair &other) const
{
    if(*first() < *other.first())
    {
        return true;
    }
    else if(*other.first() < *first())
    {
        return false;
    }

    if(*second() < *other.second())
    {
        return true;
    }
    else if(*other.second() < *second())
    {
        return false;
    }

    return false;
}

SL::VarVal *SL::Expression::eval(SolverProgramState *state)
{
    SL::VarVal* ret = nullptr;
    switch (expression_meta_type) {
        case predicate_meta_type:
            ret = new VarVal((bool)predicate->eval(state));
            break;
        case func_call_meta_type:
            ret = func_call->eval(state);
            break;
        case identifier_meta_type:
            ret = state->get_var_val(state->name_to_var(identifier));
            break;
        case var_val_meta_type:
            ret = var_val;
            break;
        default:
            assert(false);
    }
    if(ret == nullptr)
    {
        ret = new VarVal();
    }
    return ret;
}