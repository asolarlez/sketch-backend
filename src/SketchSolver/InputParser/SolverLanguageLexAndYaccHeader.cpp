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
    while(expression->eval(state)->get_bool(true, false))
    {
        body->run(state);
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
    state->open_subframe();
    def->run(state);
    while(expression->eval(state)->get_bool(true, false))
    {
        body->run(state);
        plus_plus->run(state);
    }
    state->close_subframe();
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
    if(expression->eval(state)->get_bool(true, false))
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
            try {
                to_var = state->name_to_var_throws(dest_name);
            }
            catch (exception e) {
                SL::SLType* any_type = new SL::SLType(new SL::Identifier("any"));
                SL::Identifier* copy_name = new SL::Identifier(dest_name);
                SL::Var* var = new SL::Var(any_type, copy_name);
                state->add_var(var);
                var->clear();
                to_var = state->name_to_var(dest_name);
            }
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
        assert(dest_type == var_dest_type);
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
            dest_name = new Identifier(to_copy->dest_name);
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
SL::VarVal* SL::Identifier::eval(SolverProgramState *state)  {
    return state->get_var_val(state->name_to_var(this));
}

bool SL::Identifier::is_defined() {
    return defined;
}

SL::VarVal* SL::FunctionCall::eval_type_constructor(SolverProgramState* state)
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
        default:
            assert(false);
    }
    assert(false);
}

template<>
SL::VarVal *SL::FunctionCall::eval<SL::PolyVec*>(SL::PolyVec*& poly_vec, SolverProgramState* state, SL::VarVal* the_var_val)
{
    assert(poly_vec == the_var_val->get_poly_vec(false));
    switch (method_id) {
        case _size: {
            assert(params.empty());
            return new SL::VarVal((int) poly_vec->size());
            break;
        }
        case _get:
        {
            assert(params.size() == 1);
            int idx = params[0]->eval(state)->get_int(true, false);
            return poly_vec->at(idx);
            break;
        }
        case _append:
        {
            assert(params.size() == 1);
            SL::VarVal* ret_param = params[0]->eval(state);
            ret_param->increment_shared_ptr();
            poly_vec->emplace_back(ret_param);
            return new VarVal();
            break;
        }
        case _sort_vec:
        {
            assert(params.empty());
            poly_vec->sort();
            return new VarVal();
            break;
        }
        case _reverse:
        {
            assert(params.empty());
            poly_vec->reverse();
            return new VarVal();
        }
        default:
            assert(false);
    }
    assert(false);
}

template<>
SL::VarVal *SL::FunctionCall::eval<File*>(File*& file, SolverProgramState *state, SL::VarVal* the_var_val) {
    assert(file == the_var_val->get_file(false));
    switch (method_id) {
        case _produce_subset_file:
        {
            assert(params.size() == 1);
            int num_rows = params[0]->eval(state)->get_int();
            return new SL::VarVal(file->sample_sub_file(num_rows));
            break;
        }
        case _size:
        {
            assert(params.empty());
            return new SL::VarVal((int) file->size());
            break;
        }
        case _get:
        {
            assert(params.size() == 1);
            int row_id = params[0]->eval(state)->get_int();
            return new SL::VarVal(new SolverLanguagePrimitives::InputAssignment(file->at(row_id), state->floats));
            break;
        }
        case _clear:
        {
            assert(params.empty());
            file->clear();
            delete file;
            file = nullptr;
            return new VarVal();
            break;
        }
        case _produce_filter:
        {
            assert(params.size() == 1);
            VarVal* method_var_val = params[0]->eval(state);
            method_var_val->increment_shared_ptr();
            Method* condition_method = method_var_val->get_method();
            assert(condition_method->get_params()->size() == 1);
            assert(condition_method->get_params()->at(0)->get_var()->accepts_type("bool"));

            std::function<bool(VarStore*) > lambda_condition = [&state, &condition_method](VarStore* x) {
                vector<SL::VarVal*> local_inputs;
                local_inputs.push_back(new VarVal(new SolverLanguagePrimitives::InputAssignment(x, state->floats)));
                auto var_val = condition_method->eval(state, local_inputs);
                auto ret = var_val->get_bool(true, false);
                return ret;
            };

            File* ret = file->produce_filter(lambda_condition);

            method_var_val->decrement_shared_ptr();
            return new VarVal(ret);
        }
        case _relabel:
        {
            assert(params.size() == 1);
            VarVal* sk_func_var_val = params[0]->eval(state);
            sk_func_var_val->increment_shared_ptr();
            file->relabel(sk_func_var_val->get_function());
            sk_func_var_val->decrement_shared_ptr();
            return new VarVal();
        }
        default:
            assert(false);
    }
    assert(false);
}

SL::VarVal* SL::FunctionCall::eval_global(SolverProgramState *state)
{
    switch (method_id) {
        case _file: {
            assert(params.size() == 2);
            string file_name = params[0]->eval(state)->get_string();
            SketchFunction *harness = params[1]->eval(state)->get_harness();
            SL::VarVal *ret = new SL::VarVal(new File(harness, file_name, state->floats, state->args.seed));
            return ret;
            break;
        }
        case _sat_solver:
        {
            assert(params.size() == 2);
            SketchFunction* harness = params[0]->eval(state)->get_harness()->produce_inlined_dag();
            File* file = params[1]->eval(state)->get_file();
            using namespace SolverLanguagePrimitives;
            WrapperAssertDAG* solver =
                    new WrapperAssertDAG(state->floats, state->hc, state->args, state->hasGoodEnoughSolution);
            assert(file->get_counterexample_ids_over_time().empty());
            ProblemAE* problem = new ProblemAE(harness, file);
            HoleAssignment* sol = (solver)->
                    solve(problem);
            delete problem;
            harness->clear();
            solver->clear();
            return new SL::VarVal(sol);
            break;
        }
        case _Solution:
        {
            assert(params.empty());
            using namespace SolverLanguagePrimitives;
            return new VarVal(new HoleAssignment(false));
        }
        case _print:
        {
            assert(!params.empty());
            string print_str = "";
            cout << "SOLVER_PROGRAM_PRINT INIT" << endl;
            for(int i = 0;i<params.size();i++)
            {
                cout << params[i]->eval(state)->to_string(true, false) <<" ";
            }
            cout << endl << "SOLVER_PROGRAM_PRINT END" << endl;
            return new VarVal();
        }
        case _to_float:
        {
            assert(params.size() == 1);
            float the_float = (float)params[0]->eval(state)->get_int(true, false);
            return new VarVal(the_float);
            break;
        }
        case _assert:
        {
            assert(!params.empty());
            bool assert_true = params[0]->eval(state)->get_bool(true, false);
            string message_suffix = "\nassert(false); // triggered in Solver Program";
            if(params.size() == 1) {
                AssertDebug(assert_true, message_suffix);
            }
            else if(params.size() >= 2) {
                string message_prefix = "";
                for(int i = 1;i<params.size();i++) {
                    message_prefix += params[i]->eval(state)->to_string(true, false);
                }
                AssertDebug(assert_true, "MESSAGE: " + message_prefix + message_suffix);
            }
            else {
                assert(false);
            }
            return new VarVal();
            break;
        }
        case _not:
        {
            assert(params.size() == 1);
            bool val = params[0]->eval(state)->get_bool(true, false);
            return new VarVal((bool)!val);
        }
        default:
            assert(false);
    }
    assert(false);
}

template<>
SL::VarVal *SL::FunctionCall::eval<SolverLanguagePrimitives::HoleAssignment*>(
        SolverLanguagePrimitives::HoleAssignment*& the_solution, SolverProgramState *state, SL::VarVal* the_var_val) {
    assert(the_solution == the_var_val->get_solution(false));
    switch (method_id) {
        case _join: {
            assert(params.size() == 1);
            using namespace SolverLanguagePrimitives;
            HoleAssignment *other_solution = params[0]->eval(state)->get_solution();
            the_solution->join_with(other_solution);
            return new VarVal();
        }
        default:
            assert(false);
    }
    assert(false);
}

template<>
SL::VarVal* SL::FunctionCall::eval<SL::PolyPair*>(SL::PolyPair*& poly_pair, SolverProgramState* state, SL::VarVal* the_var_val)
{
    assert(poly_pair == the_var_val->get_poly_pair(false));
    switch (method_id) {
        case _first: {
            assert(params.empty());
            return poly_pair->first();
            break;
        }
        case _second: {
            assert(params.empty());
            return poly_pair->second();
            break;
        }
        default:
            assert(false);
    }
    assert(false);
}

SL::VarVal* SL::FunctionCall::eval(SolverProgramState *state)
{

    if(method_id != _unknown_method)
    {

        assert(method_id_to_type_str.find(method_id) != method_id_to_type_str.end());

        pair<SL::Var*, SL::VarVal* > var_and_var_val =
                get_var_and_var_val_and_assert_type(state, method_id_to_type_str[method_id]);

        VarVal* var_val = var_and_var_val.second;

        if(var_val != nullptr) {
            return var_val->eval(state, this);
        }
        else{
            return eval_global(state);
        }

        assert(false);
    }
    else
    {
        switch (method_meta_type) {
            case name_meta_type:
            {
                Var *method_var = state->name_to_var(method_name);
                VarVal *method_var_val = state->get_var_val(method_var);
                method_var_val->increment_shared_ptr();
                VarVal *ret = nullptr;
                if (method_var_val->is_method()) {
                    ret = method_var_val->get_method()->eval(state, params);
                    assert(ret != nullptr);
                }
                else {
                    assert(method_var_val->is_sketch_function());
                    assert(params.size() == 1);
                    VarVal* input_val = params[0]->eval(state);
                    input_val->increment_shared_ptr();
                    assert(input_val->is_input_holder());

                    SketchFunction* sk_func = method_var_val->get_function();
                    BooleanDAG* the_dag = sk_func->get_dag()->clone();
                    sk_func->get_env()->doInline(*the_dag);

                    const map<string, BooleanDAG *> * bool_dag_map = sk_func->get_env()->function_map.to_boolean_dag_map();
                    NodeEvaluator node_evaluator(*bool_dag_map, *the_dag, sk_func->get_env()->floats);

                    VarStore* the_var_store = input_val->get_input_holder()->to_var_store();
                    bool fails = node_evaluator.run(*the_var_store);
                    delete the_var_store;
                    the_var_store = nullptr;
                    assert(!fails);

                    auto after_run_dests = the_dag->getNodesByType(bool_node::DST);

                    assert(after_run_dests.size() == 1);

                    //SHOULD BE THIS BUT ISN'T
//                    ret = new VarVal(node_evaluator.getValue(after_run_dests[0]));
                    OutType* out_type = (*the_dag)[node_evaluator.getValue(after_run_dests[0])]->get_parent(0)->getOtype();
                    assert(out_type == OutType::BOOL);
                    //THIS IS A HACK BUT WORKS FOR NOW
                    ret = new VarVal((bool)node_evaluator.getValue((*the_dag)[node_evaluator.getValue(after_run_dests[0])]->get_parent(0)));

                    delete bool_dag_map;
                    bool_dag_map = nullptr;

                    the_dag->clear();
                    input_val->decrement_shared_ptr();
                }
                method_var_val->decrement_shared_ptr();
                assert(ret != nullptr);
                return ret;
            }
            case type_constructor_meta_type:
                return eval_type_constructor(state);
                break;
            default:
                assert(false);
        }
        assert(false);
    }
    assert(false);
}

void SL::FunctionCall::run(SolverProgramState *state) {
    SL::VarVal* ret = eval(state);
    assert(ret->is_void());
}

pair<SL::Var *, SL::VarVal* > SL::FunctionCall::get_var_and_var_val_and_assert_type(
        SolverProgramState* state, vector<string> type_names) {

    assert(type_names.size() >= 1);

    string type_name = type_names[0];

    if(type_names.size() == 1) {
        if (type_name == "namespace") {
            assert(expression == nullptr);
            return make_pair(nullptr, nullptr);
        }
    }
    else
    {
        assert(type_names.size() == 2);
        assert(type_names[0] != "namespace");
        assert(type_names[1] != "namespace");
    }

    Identifier* var_name = expression->get_var_name();
    //if there is no var_name, then there is nothing to assert the type of rn
    if(var_name == nullptr) {
        return make_pair(nullptr, expression->eval(state));
    }
    //otherwise assert the type;
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
    bool enter = false;
    for(int i = 0;i<type_names.size();i++) {
        if(type_names[i] == var_type_str) {
            enter = true;
            break;
        }
    }
    assert(enter);

    if(var_val == nullptr)
    {
        var_val = expression->eval(state);
    }

    assert(var_val != nullptr);
    return make_pair(var, var_val);
}

void SL::FunctionCall::clear() {
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

SL::FunctionCall::FunctionCall(SL::FunctionCall *to_copy): method_meta_type(to_copy->method_meta_type), method_id(to_copy->method_id)
{
    if(to_copy->expression != nullptr) {
        expression = new Expression(to_copy->expression);
    }
    switch(method_meta_type) {
        case name_meta_type:
            method_name = new Identifier(to_copy->method_name);
            break;
        case type_constructor_meta_type:
            type_constructor = new SLType(to_copy->type_constructor);
            break;
        default:
            assert(false);
    }
    for(auto it: to_copy->params) {
        params.push_back(new Param(it));
    }
}

void SL::add_to_method_str_to_method_id_map(const string& method_str, SL::MethodId method_id, string type_str_1, string type_str_2)
{
    assert(method_str_to_method_id_map.find(method_str) == method_str_to_method_id_map.end());
    method_str_to_method_id_map[method_str] = method_id;
    assert(method_id_to_type_str.find(method_id) == method_id_to_type_str.end());
    assert(type_str_1 != "");
    method_id_to_type_str[method_id] = vector<string>();
    method_id_to_type_str[method_id].push_back(type_str_1);
    if(type_str_2 != "") {
        method_id_to_type_str[method_id].push_back(type_str_2);
    }
}

void SL::init_method_str_to_method_id_map()
{
    assert(method_str_to_method_id_map_is_defined == false);
    add_to_method_str_to_method_id_map("File", _file, "namespace");
    add_to_method_str_to_method_id_map("produce_subset_file", _produce_subset_file, "File");
    add_to_method_str_to_method_id_map("SATSolver", _sat_solver, "namespace");
    add_to_method_str_to_method_id_map("produce_concretization", _produce_concretization, "SketchFunction");
    add_to_method_str_to_method_id_map("concretize", _concretize, "SketchFunction");
    add_to_method_str_to_method_id_map("size", _size, "File", "vector");
    add_to_method_str_to_method_id_map("get", _get, "File", "vector");
    add_to_method_str_to_method_id_map("passes", _passes, "SketchFunction");
    add_to_method_str_to_method_id_map("clear", _clear, "SketchFunction", "File");
    add_to_method_str_to_method_id_map("Solution", _Solution, "namespace");
    add_to_method_str_to_method_id_map("join", _join, "Solution");
    add_to_method_str_to_method_id_map("print", _print, "namespace");
    add_to_method_str_to_method_id_map("num_holes", _num_holes, "SketchFunction");
    add_to_method_str_to_method_id_map("append", _append, "vector");
    add_to_method_str_to_method_id_map("first", _first, "pair");
    add_to_method_str_to_method_id_map("second", _second, "pair");
    add_to_method_str_to_method_id_map("float", _to_float, "namespace");
    add_to_method_str_to_method_id_map("sort", _sort_vec, "vector");
    add_to_method_str_to_method_id_map("clone", _clone, "SketchFunction", "Solution");
    add_to_method_str_to_method_id_map("assert", _assert, "namespace");
    add_to_method_str_to_method_id_map("reverse", _reverse, "vector");
    add_to_method_str_to_method_id_map("produce_replace", _produce_replace, "SketchFunction");
    add_to_method_str_to_method_id_map("replace", _replace, "SketchFunction");
    add_to_method_str_to_method_id_map("get_solution", _get_solution, "SketchFunction");
    add_to_method_str_to_method_id_map("produce_filter", _produce_filter, "File");
    add_to_method_str_to_method_id_map("not", _not, "namespace");
    add_to_method_str_to_method_id_map("relabel", _relabel, "File");
    method_str_to_method_id_map_is_defined = true;
}

SL::MethodId SL::FunctionCall::get_method_id() {
    if(!method_str_to_method_id_map_is_defined){
        init_method_str_to_method_id_map();
    }

    string method_str;

    switch (method_meta_type) {
        case name_meta_type:
            method_str = method_name->to_string();
            break;
        case type_constructor_meta_type:
            method_str = type_constructor->get_head()->to_string();
            break;
        default:
            assert(false);
    }

    if(method_str_to_method_id_map.find(method_str) != method_str_to_method_id_map.end()) {
        return method_str_to_method_id_map[method_str];
    }
    else{
        return _unknown_method;
    }
}

void
eval__sketch_function_replace(SL::VarVal *ret_var_val, SketchFunction *ret_sk_func, SolverProgramState *state,
                              const vector<SL::Param *> &params)
{
    AssertDebug(ret_var_val->get_function(false) == ret_sk_func, "ret_sk_func must be the same as what ret_var_val is holding.");
    assert(params.size() == 2);

    string str_to_replace = params[0]->eval(state)->get_string(true, false);
    SL::VarVal* to_replace_with_var_val = params[1]->eval(state);

    ret_var_val->add_responsibility(to_replace_with_var_val);

    SketchFunction* to_replace_with_sk_func = to_replace_with_var_val->get_function();
    const string& to_replace_with_name = to_replace_with_sk_func->get_dag()->get_name();

    state->add_to_function_map(to_replace_with_name, to_replace_with_sk_func);

    ret_sk_func->replace(str_to_replace, to_replace_with_name);
}

template<>
SL::VarVal *SL::FunctionCall::eval<SketchFunction*>(SketchFunction*& sk_func, SolverProgramState *state, VarVal* the_var_val) {

    assert(sk_func == the_var_val->get_function(false));
    switch (method_id) {
        case _produce_concretization:
        {
            assert(params.size() == 1);
            using namespace SolverLanguagePrimitives;
            VarVal* var_val_sol = params[0]->eval(state);
            var_val_sol->increment_shared_ptr();
            HoleAssignment* sol = var_val_sol->get_solution();
            SketchFunction* harness = sk_func;
            for(auto it : harness->get_env()->function_map)
            {
                assert(it.second->get_dag()->getNodesByType(bool_node::CTRL).size() >= 0);
            }
            SketchFunction* concretized_function =
                    harness->produce_with_concretized_holes(sol, true);
            var_val_sol->decrement_shared_ptr();
            return new SL::VarVal(concretized_function);
            break;
        }
        case _concretize:
        {
            assert(params.size() == 1);
            using namespace SolverLanguagePrimitives;
            HoleAssignment* sol = params[0]->eval(state)->get_solution();
            SketchFunction* harness = sk_func;
            harness->concretize(sol, true);
            return new SL::VarVal();
            break;
        }
        case _passes:
        {
            assert(params.size() == 1);
            SketchFunction* program = sk_func;
            VarVal* input_holder_var_val = params[0]->eval(state);
            input_holder_var_val->increment_shared_ptr();
            SolverLanguagePrimitives::InputAssignment* input_holder = input_holder_var_val->get_input_holder();

            if(false) {
                int transformer_size = program->get_env()->function_map.get_program().size();
                SketchFunction *concretized_function = program->produce_with_concretized_inputs(input_holder);
                input_holder_var_val->decrement_shared_ptr();
                assert((concretized_function->get_dag()->size() == 0) ==
                       (concretized_function->get_dag()->get_failed_assert() == nullptr));
                bool ret = concretized_function->get_dag()->get_failed_assert() == nullptr;
                int interim_transformer_size = program->get_env()->function_map.get_program().size();
                assert(transformer_size + 2 == interim_transformer_size);
                concretized_function->clear();
                assert(transformer_size == program->get_env()->function_map.get_program().size());
                return new VarVal(ret);
            }
            else
            {
                VarStore* inputs = input_holder->to_var_store();
                input_holder_var_val->decrement_shared_ptr();

                ProgramEnvironment* env = program->get_env();

                BooleanDAG* to_concretize = program->get_dag()->clone();
                env->doInline(*to_concretize, *inputs, bool_node::SRC);

                inputs->clear();
                inputs = nullptr;

                assert((to_concretize->size() == 0) == (to_concretize->get_failed_assert() == nullptr));

                bool ret = to_concretize->get_failed_assert() == nullptr;

                to_concretize->clear();
                return new VarVal(ret);
            }
            break;
        }
        case _clear:
        {
            assert(params.empty());
            sk_func->clear();
            sk_func = nullptr;
            return new VarVal();
            break;
        }
        case _num_holes: {
            assert(params.empty());
            SketchFunction* to_clone = sk_func;
            SketchFunction* func_clone = to_clone->produce_inlined_dag();
            int num_ctrls = (int) func_clone->get_dag()->getNodesByType(bool_node::CTRL).size();
            func_clone->clear();
            return new VarVal(num_ctrls);
            break;
        }

        case _clone:
        {
            assert(params.empty());
            for(const auto& it : sk_func->get_env()->function_map)
            {
                assert(it.second->get_dag()->getNodesByType(bool_node::CTRL).size() >= 0);
            }
            SketchFunction* ret = sk_func->clone();
            for(const auto& it : ret->get_env()->function_map)
            {
                assert(it.second->get_dag()->getNodesByType(bool_node::CTRL).size() >= 0);
            }
            return new VarVal(ret);
            break;
        }

        case _produce_replace:
        {
            assert(params.size() == 2);
            SketchFunction* ret_sk_func = sk_func->clone();
            VarVal* ret_var_val = new VarVal(ret_sk_func);
            eval__sketch_function_replace(ret_var_val, ret_sk_func, state, params);
            return ret_var_val;
            break;
        }
        case _replace:
        {
            assert(params.size() == 2);
            the_var_val->increment_shared_ptr();
            eval__sketch_function_replace(the_var_val, the_var_val->get_function(), state, params);
            the_var_val->decrement_shared_ptr();
            return new VarVal();
            break;
        }
        case _get_solution:
        {
            if(params.empty()) {
                return new VarVal(sk_func->get_solution());
            }
            else if(params.size() == 1){
                string name = params[0]->eval(state)->get_string(true, false);
                return new VarVal(sk_func->get_solution(name));
            }
            else
            {
                assert(false);
            }
        }
        default:
            assert(false);
    }
    assert(false);
    return nullptr;
}

template<typename T>
SL::VarVal* SL::Method::eval(SolverProgramState *state, vector<T>& inputs)  {
    run(state, inputs);
    return state->get_return_var_val();
}

void SL::Method::run(SolverProgramState *state, vector<Param *> &input_params)  {
    assert(var != nullptr);
    assert(body != nullptr);

    state->new_stack_frame(*params, input_params, meta_params);

    body->run(state);

    state->pop_stack_frame();
}

void SL::Method::run(SolverProgramState *state, vector<SL::VarVal *> &input_params)  {
    assert(var != nullptr);
    assert(body != nullptr);

    state->new_stack_frame(*params, input_params, meta_params);

    body->run(state);

    state->pop_stack_frame();
}

void SL::Method::clear()
{
    body->clear();
    var->clear();
    for(int i = 0;i<params->size();i++)
    {
        params->at(i)->clear();
    }
    params->clear();
    delete params;
    if(meta_params != nullptr)
    {
        for(int i = 0;i<meta_params->size();i++)
        {
            meta_params->at(i)->clear();
        }
        meta_params->clear();
        delete meta_params;
    }
}

vector<SL::Param*>* copy_params(vector<SL::Param*>* to_copy)
{
    vector<SL::Param*>* ret = new vector<SL::Param*>();
    for(int i = 0;i<to_copy->size();i++)
    {
        ret->push_back(new SL::Param(to_copy->at(i)));
    }
    return ret;
}

SL::Method::Method(SL::Method *to_copy) :
var(new Var(to_copy->var)), body(new CodeBlock(to_copy->body)), params(copy_params(to_copy->params)){}

const vector<SL::Param*>* SL::Method::get_params() {
    return params;
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
        case binary_expr_meta_type:
            ret = binary_expression->eval(state);
            break;
        case func_call_meta_type:
            ret = function_call->eval(state);
            break;
        case identifier_meta_type:
            ret = state->get_var_val(state->name_to_var(identifier));
            break;
        case var_val_meta_type:
            ret = new VarVal(var_val);
            break;
        case lambda_expr_meta_type:
            ret = lambda_expression->eval(state);
            break;
        default:
            assert(false);
    }
    assert(ret != nullptr);
    return ret;
}

void SL::Expression::clear()
{
    switch (expression_meta_type) {
        case binary_expr_meta_type:
            binary_expression->clear();
            break;
        case func_call_meta_type:
            function_call->clear();
            break;
        case identifier_meta_type:
            identifier->clear();
            break;
        case var_val_meta_type:
            var_val->increment_shared_ptr();
            var_val->decrement_shared_ptr();
            break;
        case lambda_expr_meta_type:
            lambda_expression->clear();
            break;
        default:
            assert(false);
    }
    delete this;
}

SL::Expression::Expression(Expression* to_copy) : expression_meta_type(to_copy->expression_meta_type)
{
    switch (expression_meta_type) {
        case binary_expr_meta_type:
            binary_expression = new BinaryExpression(to_copy->binary_expression);
            break;
        case func_call_meta_type:
            function_call = new FunctionCall(to_copy->function_call);
            break;
        case identifier_meta_type:
            identifier = new Identifier(to_copy->identifier);
            break;
        case var_val_meta_type:
            var_val = new VarVal(to_copy->var_val);
            break;
        case lambda_expr_meta_type:
            lambda_expression = new LambdaExpression(to_copy->lambda_expression);
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
SL::VarVal::VarVal(SolverLanguagePrimitives::HoleAssignment* _solution) : solution(_solution), var_val_type(solution_val_type){}
SL::VarVal::VarVal(SolverLanguagePrimitives::InputAssignment* _input_holder) : input_holder(_input_holder), var_val_type(input_val_type){}

SL::VarVal::VarVal(string  _s) : s(new Identifier(_s)), var_val_type(string_val_type) {}

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

//SL::VarVal::VarVal(int val): i(val), var_val_type(int_val_type){}



SL::VarVal::VarVal(VarVal* _to_copy): var_val_type(_to_copy->var_val_type)
{
    for(int i = 0;i< _to_copy->is_responsible_for.size();i++)
    {
        add_responsibility(new VarVal(_to_copy->is_responsible_for[i]));
    }

    switch (_to_copy->var_val_type) {
        case string_val_type:
            s = new Identifier(_to_copy->get_string(false));
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
//            solution = new SolverLanguagePrimitives::HoleAssignment(_to_copy->get_solution(false));
            break;
        case input_val_type:
            input_holder = new SolverLanguagePrimitives::InputAssignment(_to_copy->get_input_holder(false));
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

SL::VarVal *SL::VarVal::eval(SolverProgramState *state, SL::FunctionCall *func_call) {
    switch (var_val_type) {
        case string_val_type:
            //do nothing
//            eval<Identifier*>(s, state, func_call);
            break;
        case int_val_type:
            //do nothing
            break;
        case file_val_type:
            return eval<File*>(file, state, func_call);
            break;
        case method_val_type:
            //nothing to do yet
//            eval<Method*>(method, state, func_call);
            break;
        case skfunc_val_type:
            return eval<SketchFunction*>(skfunc, state, func_call);
            break;
        case solution_val_type:
            return eval<SolverLanguagePrimitives::HoleAssignment*>(solution, state, func_call);
            break;
        case input_val_type:
            //nothing to do yet
//            eval<SolverLanguagePrimitives::InputAssignment*>(input_holder, state, func_call);
            break;
        case bool_val_type:
            //do nothing
            break;
        case void_val_type:
            //do nothing
            break;
        case no_type:
            assert(false);
            break;
        case float_val_type:
            //do nothing
            break;
        case poly_pair_type:
            return eval<SL::PolyPair*>(poly_pair, state, func_call);
            break;
        case poly_vec_type:
            return eval<SL::PolyVec*>(poly_vec, state, func_call);
            break;
        default:
            assert(false);
    }
    assert(false);
}

template<typename T>
SL::VarVal *SL::VarVal::eval(T& val, SolverProgramState *state, SL::FunctionCall *function_call)
{
    assert_type_invariant<T>();
    assert(val != nullptr);
    return function_call->eval<T>(val, state, this);
}

SL::VarVal *SL::VarVal::clone() {
    assert(is_sketch_function());
    return new VarVal(skfunc->clone());
}

void SL::VarVal::add_responsibility(SL::VarVal *new_child) {
    new_child->increment_shared_ptr();
    is_responsible_for.push_back(new_child);
}

bool SL::VarVal::is_input_holder() {
    return var_val_type == input_val_type;
}

bool SL::VarVal::is_solution_holder() {
    return var_val_type == solution_val_type;
}

void SL::VarVal::clear_assert_0_shared_ptrs() {
    assert(get_num_shared_ptr() == 0);
    _clear();
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
        case code_block_line:
            code_block = new SL::CodeBlock(to_copy->code_block);
            break;
        default:
            assert(false);
    }
}

void SL::UnitLine::run(SolverProgramState *state) {
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
        case code_block_line:
            code_block->run(state);
            break;
        default:
            assert(false);
    }
}

void SL::UnitLine::clear() {
    switch (line_type) {
        case var_line:
            var->clear();
            break;
        case assign_line:
            assignment->clear();
            break;
        case while_line:
            while_loop->clear();
            break;
        case if_line:
            if_block->clear();
            break;
        case return_line:
            return_stmt->clear();
            break;
        case expression_line:
            expression->clear();
            break;
        case for_line:
            for_loop->clear();
            break;
        case code_block_line:
            code_block->clear();
            break;
        default:
            assert(false);
    }
    delete this;
}

SL::BinaryExpression::BinaryExpression(SL::BinaryExpression *to_copy): op(to_copy->op)
{
    left_operand = new SL::Expression(to_copy->left_operand);
    right_operand = new SL::Expression(to_copy->right_operand);
}

SL::SLType::SLType(SL::Identifier *_name, SL::TypeParams *_type_params) : name(_name), type_params(new PolyType(_type_params)){}

SL::SLType::SLType(SL::SLType *to_copy) : name(new Identifier(to_copy->name)) {
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

void SL::CodeBlock::run(SolverProgramState *state)  {
    assert(head != nullptr);
    CodeBlock* at = this;
    state->open_subframe();
    while(at != nullptr)
    {
        at->head->run(state);
        if(state->has_return())
        {
            break;
        }
        at = at->rest;
    }
    state->close_subframe();
}

SL::LambdaExpression::LambdaExpression(SL::LambdaExpression *to_copy) :
code_block(new SL::CodeBlock(to_copy->code_block)), params(copy_params(to_copy->params)), meta_params(copy_params(to_copy->meta_params)) {}

SL::VarVal *SL::LambdaExpression::eval(SolverProgramState *state) {
    return new VarVal(
            new Method(
                    new Var(new SL::SLType(new Identifier("any")), new Identifier("lambda")),
                    copy_params(params),
                    new CodeBlock(code_block),
                    copy_params(meta_params)));
}

void SL::LambdaExpression::clear() {
    for(int i = 0;i<params->size();i++) {
        params->at(i)->clear();
    }
    params->clear();
    delete params;
    for(int i = 0;i<meta_params->size();i++) {
        meta_params->at(i)->clear();
    }
    meta_params->clear();
    delete meta_params;
    code_block->clear();
    delete this;
}

