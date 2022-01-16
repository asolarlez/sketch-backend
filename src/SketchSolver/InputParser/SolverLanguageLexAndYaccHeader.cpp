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

SL::VarVal* SL::Var::eval(SolverProgramState *state) {
    return state->get_var_val(this);
}

SL::SLType * SL::Var::get_type() {
    return type;
}


void SL::While::run(SolverProgramState* state)
{
    SL::VarVal* cond = expression->eval(state);
    while(cond->get_bool(true, false))
    {
        body->run(state);
        cond = expression->eval(state);
    }
}

void SL::While::clear()
{
    expression->clear();
    body->clear();
    delete this;
}

SL::While::While(While* to_copy){
    expression = new Expression(to_copy->expression);
    body = new CodeBlock(to_copy->body);
}

void SL::For::run(SolverProgramState* state)
{
    def->run(state);
    SL::VarVal* cond = expression->eval(state);
    while(cond->get_bool(true, false))
    {
        body->run(state);
        plus_plus->run(state);
        cond = expression->eval(state);
    }
}

void SL::For::clear() {
    def->clear();
    expression->clear();
    plus_plus->clear();
    body->clear();
    delete this;
}

SL::For::For(SL::For *to_copy)
{
    def = new UnitLine(to_copy->def);
    expression = new Expression(to_copy->expression);
    plus_plus = new UnitLine(to_copy->plus_plus);
    body = new CodeBlock(to_copy->body);
}

void SL::If::run(SolverProgramState *state) {
    SL::VarVal* cond = expression->eval(state);
    if(cond->get_bool(true, false))
    {
        body->run(state);
    }
}

void SL::If::clear() {
    expression->clear();
    body->clear();
    delete this;
}

SL::If::If(SL::If *to_copy) {
    expression = new Expression(to_copy->expression);
    body = new CodeBlock(to_copy->body);
}

void SL::Return::run(SolverProgramState *state){
    state->set_return_var_val(expression->eval(state));
}

void SL::Return::clear() {
    expression->clear();
    delete this;
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
        if(var_val->get_is_return())
        {
            var_val->complete_return();
        }
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

void SL::Assignment::clear()
{
    switch (dest_type) {
        case name_dest_type:
            dest_name->clear();
            break;
        case var_dest_type:
            dest_var->clear();
            break;
        default:
            assert(false);
    }
    if(expression != nullptr) {
        expression->clear();
    }
    delete this;
}

SL::Assignment::Assignment(SL::Assignment *to_copy) : dest_type(to_copy->dest_type)
{
    switch (dest_type)
    {
        case var_dest_type:
            dest_var = new Var(to_copy->dest_var);
            break;
        case name_dest_type:
            dest_name = new Name(to_copy->dest_name);
            break;
        case no_dest_type:
            assert(false);
            break;
        default:
            assert(false);
    }
    if(to_copy->expression != nullptr)
    {
        expression = new Expression(to_copy->expression);
    }
}
SL::VarVal* SL::Name::eval(SolverProgramState *state)  {
    return state->get_var_val(state->name_to_var(this));
}

bool SL::Name::is_defined() {
    return defined;
}

SL::VarVal* SL::FuncCall::eval_type_constructor(SolverProgramState* state)
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
            PolyType* type_params = type_constructor->get_type_params();
            assert(type_params->size() == 1);
            assert(params.empty());
            return new SL::VarVal(new PolyVec(type_params));
            break;
        }
        case pair: {
            PolyType* type_params = type_constructor->get_type_params();
            assert(type_params->size() == 2);
            assert(params.size() == 2);
            SL::VarVal* left = params[0]->eval(state);
            SL::VarVal* right = params[1]->eval(state);
            left->increment_shared_ptr();
            right->increment_shared_ptr();
            return new SL::VarVal(new PolyPair(type_params, left, right));
            break;
        }
        case do_predef:
            assert(false);
            break;
    }
}

SL::VarVal* SL::FuncCall::eval(SolverProgramState *state)
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
            pair<SL::Var*, SL::VarVal* > var_and_var_val = get_var_assert_type(state, "namespace");
            SL::Var* var = var_and_var_val.first;
            assert(var == nullptr);
            assert(params.size() == 2);
            string file_name = params[0]->eval(state)->get_string();
            SketchFunction* harness = params[1]->eval(state)->get_harness();
            SL::VarVal* ret = new SL::VarVal(new File(harness, file_name, state->floats, state->args.seed));
            return ret;
            break;
        }
        case produce_subset_file:
        {
            pair<SL::Var*, SL::VarVal* > var_and_var_val = get_var_assert_type(state, "File");
            assert(params.size() == 1);
            int num_rows = params[0]->eval(state)->get_int();
            File* file = var_and_var_val.second->get_file();
            return new SL::VarVal(file->sample_sub_file(num_rows));
            break;
        }
        case sat_solver:
        {
            pair<SL::Var*, SL::VarVal* > var_and_var_val = get_var_assert_type(state, "namespace");
            SL::Var* var = var_and_var_val.first;
            assert(var == nullptr);
            assert(params.size() == 2);
            SketchFunction* harness = params[0]->eval(state)->get_harness()->produce_inlined_dag();
            File* file = params[1]->eval(state)->get_file();
            using namespace SolverLanguagePrimitives;
            WrapperAssertDAG* solver =
                    new WrapperAssertDAG(state->floats, state->hc, state->args, state->hasGoodEnoughSolution);
            assert(file->get_counterexample_ids_over_time().empty());
            ProblemAE* problem = new ProblemAE(harness, file);
            SolutionHolder* sol = (solver)->
                    solve(problem);
            delete problem;
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
            SL::VarVal* var_val = nullptr;
            if(var->has_type() && !var->has_any_type()) {
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
            pair<SL::Var*, SL::VarVal* > var_and_var_val = get_var_assert_type(state, "SketchFunction");
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
            pair<SL::Var*, SL::VarVal* > var_and_var_val = get_var_assert_type(state, "SketchFunction");
            assert(params.size() == 1);
            using namespace SolverLanguagePrimitives;
            SolutionHolder* sol = params[0]->eval(state)->get_solution();
            SketchFunction* harness = var_and_var_val.second->get_harness();
            harness = harness->concretize(sol, true);
            return new SL::VarVal();
            break;
        }
        case get:
        {
            Name* var_name = expression->get_var_name();
            assert(var_name != nullptr);
            Var* var = state->name_to_var(var_name);
            SL::VarVal* var_val = nullptr;
            string var_type_str;
            if(var->has_type() && !var->has_any_type()) {
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
                if(var->has_type() && !var->has_any_type()) {
                    assert(var->get_type()->get_type_params()->size() == 1);
                }
                 assert(params.size() == 1);
                PolyVec *vec = var_val->get_poly_vec();
                int idx = params[0]->eval(state)->get_int(true, false);
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
            pair<SL::Var*, SL::VarVal* > var_and_var_val = get_var_assert_type(state, "SketchFunction");
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
            pair<SL::Var*, SL::VarVal* > var_and_var_val = get_var_assert_type(state, "namespace");
            SL::Var* var = var_and_var_val.first;
            assert(var == nullptr);
            assert(params.size() == 2);
            int left_op = params[0]->eval(state)->get_int(true, false);
            int right_op = params[1]->eval(state)->get_int(true, false);
            return new VarVal(left_op+right_op);
            break;
        }
        case div:
        {
            pair<SL::Var*, SL::VarVal* > var_and_var_val = get_var_assert_type(state, "namespace");
            SL::Var* var = var_and_var_val.first;
            assert(var == nullptr);
            assert(params.size() == 2);
            float left_op = params[0]->eval(state)->get_float(true, false);
            float right_op = params[1]->eval(state)->get_float(true, false);
            return new VarVal(left_op/right_op);
            break;
        }
        case mult:
        {
            pair<SL::Var*, SL::VarVal* > var_and_var_val = get_var_assert_type(state, "namespace");
            SL::Var* var = var_and_var_val.first;
            assert(var == nullptr);
            assert(params.size() == 2);
            float left_op = params[0]->eval(state)->get_float(true, false);
            float right_op = params[1]->eval(state)->get_float(true, false);
            return new VarVal(left_op*right_op);
            break;
        }
        case clear:
        {
            Name* var_name = expression->get_var_name();
            assert(var_name != nullptr);
            Var* var = state->name_to_var(var_name);
            SL::VarVal* var_val = nullptr;
            string var_type_str;
            if(var->has_type() && !var->has_any_type()) {
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

            var_val->force_clear();

            return new VarVal();

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
            pair<SL::Var*, SL::VarVal* > var_and_var_val = get_var_assert_type(state, "namespace");
            SL::Var* var = var_and_var_val.first;
            assert(var == nullptr);
            assert(params.empty());
            using namespace SolverLanguagePrimitives;
            return new VarVal(new SolutionHolder(false));
        }
        case join:
        {
            pair<SL::Var*, SL::VarVal* > var_and_var_val = get_var_assert_type(state, "Solution");
            assert(params.size() == 1);
            using namespace SolverLanguagePrimitives;
            SolutionHolder* other_solution = params[0]->eval(state)->get_solution();
            SolutionHolder* base_solution = var_and_var_val.second->get_solution();
            base_solution->join_with(other_solution);
            return new VarVal();
        }
        case print:
        {
            pair<SL::Var*, SL::VarVal* > var_and_var_val = get_var_assert_type(state, "namespace");
            SL::Var* var = var_and_var_val.first;
            assert(var == nullptr);
            assert(!params.empty());
            string print_str = "";
            cout << "SOLVER_PROGRAM_PRINT INIT" << endl;
            for(int i = 0;i<params.size();i++)
            {
                cout << params[i]->eval(state)->to_string(true, false);
            }
            cout << endl << "SOLVER_PROGRAM_PRINT END" << endl;
            return new VarVal();
        }
        case num_holes: {
            pair<SL::Var*, SL::VarVal* > var_and_var_val = get_var_assert_type(state, "SketchFunction");
            assert(params.empty());
            SketchFunction* func_clone = var_and_var_val.second->get_harness()->clone()->do_inline();
            int num_ctrls = (int) func_clone->get_dag()->getNodesByType(bool_node::CTRL).size();
            func_clone->clear();
            return new VarVal(num_ctrls);
            break;
        }
        case emplace_back:
        {
            pair<SL::Var*, SL::VarVal* > var_and_var_val = get_var_assert_type(state, "vector");
            assert(params.size() == 1);
            PolyVec* poly_vec = var_and_var_val.second->get_poly_vec();
            SL::VarVal* ret_param = params[0]->eval(state);
            ret_param->increment_shared_ptr();
            poly_vec->emplace_back(ret_param);
            return new VarVal();
            break;
        }
        case first:
        {
            pair<SL::Var*, SL::VarVal* > var_and_var_val = get_var_assert_type(state, "pair");
            assert(params.empty());
            PolyPair* the_pair = var_and_var_val.second->get_poly_pair();
            return the_pair->first();
            break;
        }
        case second:
        {
            pair<SL::Var*, SL::VarVal* > var_and_var_val = get_var_assert_type(state, "pair");
            assert(params.empty());
            using namespace SolverLanguagePrimitives;
            PolyPair* the_pair = var_and_var_val.second->get_poly_pair();
            return the_pair->second();
            break;
        }
        case to_float:
        {
            pair<SL::Var*, SL::VarVal* > var_and_var_val = get_var_assert_type(state, "namespace");
            SL::Var* var = var_and_var_val.first;
            assert(var == nullptr);
            assert(params.size() == 1);
            float the_float = (float)params[0]->eval(state)->get_int(true, false);
            return new VarVal(the_float);
            break;
        }
        case sort_vec:
        {
            pair<SL::Var*, SL::VarVal* > var_and_var_val = get_var_assert_type(state, "vector");
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
            SL::VarVal* var_val = nullptr;
            if(var->has_type() && !var->has_any_type()) {
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
            pair<SL::Var*, SL::VarVal* > var_and_var_val = get_var_assert_type(state, "namespace");
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
            pair<SL::Var*, SL::VarVal* > var_and_var_val = get_var_assert_type(state, "vector");
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

pair<SL::Var *, SL::VarVal* > SL::FuncCall::get_var_assert_type(SolverProgramState* state, const string& type_name) {

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
    SL::VarVal* var_val = nullptr;
    if(var->has_type() && !var->has_any_type())
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

void SL::FuncCall::clear() {
    if(expression != nullptr) {
        expression->clear();
    }
    switch (method_meta_type){
        case name_meta_type:
            method_name->clear();
            break;
        case type_constructor_meta_type:
            type_constructor->clear();
            break;
    }
    for(auto it: params){
        it->clear();
    }
    params.clear();
    delete this;
}

SL::FuncCall::FuncCall(SL::FuncCall *to_copy): method_meta_type(to_copy->method_meta_type)
{
    if(to_copy->expression != nullptr)
    {
        expression = new Expression(to_copy->expression);
    }
    switch(method_meta_type)
    {
        case name_meta_type:
            method_name = new Name(to_copy->method_name);
            break;
        case type_constructor_meta_type:
            type_constructor = new SLType(to_copy->type_constructor);
            break;
        default:
            assert(false);
    }
    for(auto it: to_copy->params)
    {
        params.push_back(new Param(it));
    }
}

SL::VarVal* SL::Method::eval(SolverProgramState *state, vector<Param*>& input_params)  {
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

void SL::Method::clear()
{
    body->clear();
    var->clear();
    for(auto & it: params)
    {
        it->clear();
    }
    params.clear();
}


SL::VarVal* SL::Param::eval(SolverProgramState *state)  {
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

void SL::Param::clear() {
    switch (meta_type) {
        case is_expression:
            expression->clear();
            break;
        case is_var:
            var->clear();
            break;
        default:
            assert(false);
    }
    delete this;
}

SL::Param::Param(SL::Param *to_copy): meta_type(to_copy->meta_type)
{
    switch (meta_type) {
        case is_var:
            var = new Var(to_copy->var);
            break;
        case is_expression:
            expression = new Expression(to_copy->expression);
            break;
        default:
            assert(false);
    }
}

void SL::Methods::populate_state(Frame &frame)  {
    Methods* at = this;
    while(at != nullptr)
    {
        frame.add_var_and_set_var_val_and_clear_var(new SL::Var(at->head->get_var()),
                                                    new SL::VarVal(new Method(at->head)));
        at = at->rest;
    }
}

bool SL::var_val_invariant(SL::SLType *var_type, SL::VarVal* var_val)
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
            }
            else {
                assert(var_type_str == "any");
            }
        } else {
            if (var_type_str == "vector") {
                assert(var_val_type == SL::poly_vec_type);
                PolyType* type_params = var_type->get_type_params();
                assert(type_params->size() == 1);
                assert(*type_params->at(0) == *var_val->get_poly_vec(false)->get_type_params()->at(0));
            } else if (var_type_str == "pair") {
                assert(var_val_type == SL::poly_pair_type);
                PolyType* type_params = var_type->get_type_params();
                assert(type_params->size() == 2);
                assert(*type_params->at(0) == (*var_val->get_poly_pair(false)->get_type_params()->at(0)));
                assert(*type_params->at(1) == (*var_val->get_poly_pair(false)->get_type_params()->at(1)));
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

bool SL::var_val_invariant(SL::Var *var, SL::VarVal* var_val)
{
    return var_val_invariant(var->get_type(), var_val);
}


void SL::PolyVec::emplace_back(SL::VarVal* new_element)
{
    assert(SL::var_val_invariant(get_type_params()->at(0), new_element));
    vector<SL::VarVal* >::emplace_back(new_element);
}

bool comparePtrToNode(SL::VarVal* a, SL::VarVal* b) { return (*a < *b); }

void SL::PolyVec::sort() {
    ::sort(begin(), end(), comparePtrToNode);
}

void SL::PolyVec::reverse() {
    ::reverse(begin(), end());
}

void SL::PolyVec::clear()  {
    cout << "CLEARING POLYVEC " << this << endl;
    PolyType::clear();
    for(int i = 0;i<size();i++)
    {
        if(at(i) != nullptr) {
            at(i)->decrement_shared_ptr();
        }
    }
    vector<SL::VarVal*>::clear();
}

vector<SL::SLType *> * SL::copy_type_params(vector<SL::SLType *> *to_copy)
{
    vector<SL::SLType*>* ret = new vector<SL::SLType*>();
    for(auto it : *to_copy) {
        ret->push_back(new SL::SLType(it));
    }
    return ret;
}

SL::PolyVec::PolyVec(PolyVec* to_copy): PolyType(to_copy)
{
    for(auto it : *to_copy)
    {
        push_back(new SL::VarVal(it));
    }
}
SL::PolyPair::PolyPair(PolyType* _type_params, SL::VarVal* left, SL::VarVal* right) :
        PolyType(_type_params), pair<SL::VarVal* , SL::VarVal* >(left, right){
    cout << "CREATE POLYPAIR " << this << endl;
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

void SL::PolyPair::clear() {
    cout << "CLEARING POLYPAIR " << this << endl;
    first()->decrement_shared_ptr();
    second()->decrement_shared_ptr();
    PolyType::clear();
}

SL::PolyPair::PolyPair(SL::PolyPair *to_copy): PolyType(to_copy), pair<SL::VarVal*, SL::VarVal*>(
        new SL::VarVal(to_copy->first()), new SL::VarVal(to_copy->second())) {};

SL::VarVal* SL::Expression::eval(SolverProgramState *state)
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
            ret = new VarVal(var_val);
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

void SL::Expression::clear()
{
    switch (expression_meta_type) {
        case predicate_meta_type:
            predicate->clear();
            break;
        case func_call_meta_type:
            func_call->clear();
            break;
        case identifier_meta_type:
            identifier->clear();
            break;
        case var_val_meta_type:
            var_val->increment_shared_ptr();
            var_val->decrement_shared_ptr();
            break;
        default:
            assert(false);
    }
    delete this;
}

SL::Expression::Expression(Expression* to_copy) : expression_meta_type(to_copy->expression_meta_type)
{
    switch (expression_meta_type) {
        case predicate_meta_type:
            predicate = new Predicate(to_copy->predicate);
            break;
        case func_call_meta_type:
            func_call = new FuncCall(to_copy->func_call);
            break;
        case identifier_meta_type:
            identifier = new Name(to_copy->identifier);
            break;
        case var_val_meta_type:
            var_val = new VarVal(to_copy->var_val);
            break;
        case no_meta_type:
            assert(false);
            break;
        default:
            assert(false);
    }
}


SL::VarVal::VarVal(float _float_val) : float_val(_float_val) , var_val_type(float_val_type){}
SL::VarVal::VarVal(File* _file) : file(_file) , var_val_type(file_val_type){}
SL::VarVal::VarVal(Method* _method) : method(_method) , var_val_type(method_val_type){}
SL::VarVal::VarVal(SketchFunction* _harness) : skfunc(_harness) , var_val_type(skfunc_val_type){}
SL::VarVal::VarVal(PolyVec* _poly_vec) : poly_vec(_poly_vec) , var_val_type(poly_vec_type){}
SL::VarVal::VarVal(PolyPair* _poly_pair) : poly_pair(_poly_pair) , var_val_type(poly_pair_type){}
SL::VarVal::VarVal(SolverLanguagePrimitives::SolutionHolder* _solution) : solution(_solution), var_val_type(solution_val_type){}
SL::VarVal::VarVal(SolverLanguagePrimitives::InputHolder* _input_holder) : input_holder(_input_holder), var_val_type(input_val_type){}

SL::VarVal::VarVal(string  _s) : s(new Name(_s)), var_val_type(string_val_type) {}

template <typename T>
SL::VarVal::VarVal(T val): var_val_type(get_var_val_type(val)){

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

SL::VarVal::VarVal(VarVal* _to_copy): var_val_type(_to_copy->var_val_type)
{
    switch (_to_copy->var_val_type) {
        case string_val_type:
            s = new Name(_to_copy->get_string(false));
            break;
        case int_val_type:
            i = _to_copy->get_int(false);
            break;
        case file_val_type:
            file = new File(_to_copy->get_file(false));
            break;
        case method_val_type:
            method = new Method(_to_copy->get_method(false));
            break;
        case skfunc_val_type:
            skfunc = _to_copy->get_function(false)->clone();
            break;
        case solution_val_type:
            assert(false);
//            solution = new SolverLanguagePrimitives::SolutionHolder(_to_copy->get_solution(false));
            break;
        case input_val_type:
            assert(false);
//            input_holder = new SolverLanguagePrimitives::InputHolder(_to_copy->get_input_holder(false));
            break;
        case bool_val_type:
            b = _to_copy->get_bool(false);
            break;
        case void_val_type:
            assert(false);
            break;
        case float_val_type:
            float_val = _to_copy->get_float(false);
            break;
        case poly_vec_type:
            poly_vec = new PolyVec(_to_copy->get_poly_vec(false));
            break;
        case poly_pair_type:
            poly_pair = new PolyPair(_to_copy->get_poly_pair(false));
            break;
        case no_type:
            break;
    }
}

void SL::VarVal::increment_shared_ptr() {
    assert(num_shared_ptr >= 0);
    num_shared_ptr+=1;
}

void SL::VarVal::set_return() {
    assert(!is_return);
    is_return = true;
    increment_shared_ptr();
}

bool SL::VarVal::get_is_return() const {
    return is_return;
}

SL::UnitLine::UnitLine(SL::UnitLine *to_copy): line_type(to_copy->line_type)
{
    switch (line_type) {
        case var_line:
            var = new SL::Var(to_copy->var);
            break;
        case assign_line:
            assignment = new SL::Assignment(to_copy->assignment);
            break;
        case while_line:
            while_loop = new SL::While(to_copy->while_loop);
            break;
        case if_line:
            if_block = new SL::If(to_copy->if_block);
            break;
        case return_line:
            return_stmt = new SL::Return(to_copy->return_stmt);
            break;
        case expression_line:
            expression = new SL::Expression(to_copy->expression);
            break;
        case for_line:
            for_loop = new SL::For(to_copy->for_loop);
            break;
        default:
            assert(false);
    }
}

SL::Predicate::Predicate(SL::Predicate *to_copy): op(to_copy->op)
{
    left_operand = new SL::Expression(to_copy->left_operand);
    right_operand = new SL::Expression(to_copy->right_operand);
}

SL::SLType::SLType(SL::Name *_name, SL::TypeParams *_type_params) : name(_name), type_params(new PolyType(_type_params)){}

SL::SLType::SLType(SL::SLType *to_copy) : name(new Name(to_copy->name)) {
    if(to_copy->type_params != nullptr) {
        type_params = new PolyType(to_copy->type_params);
    }
}

void SL::SLType::clear()
{
    assert(name->is_defined() == 1) ;
    name->clear();
    name = nullptr;
    if(type_params != nullptr) {
        type_params->clear();
        delete type_params;
        type_params = nullptr;
    }
    delete this;
}

string SL::SLType::to_string()
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
        string str = type_params->to_string();
        return name->to_string() + "<" + str + " >";
    }
}

bool SL::SLType::operator<(const SL::SLType &other) const
{
    assert(is_strongly_typed);
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
    return *type_params < *other.type_params;

}

bool SL::SLType::operator == (const SL::SLType &other) const
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
    return *type_params == *other.type_params;
}

bool SL::SLType::is_simple_type() {
    assert(name != nullptr);
    if(type_params == nullptr)
    {
        return true;
    }
    else
    {
        assert(type_params->size() >= 1);
        return false;
    }
}