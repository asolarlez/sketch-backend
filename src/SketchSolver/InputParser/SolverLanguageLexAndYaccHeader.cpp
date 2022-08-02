//
// Created by kliment on 12/19/21.
//

#include <utility>

#include "SolverLanguageLexAndYaccHeader.h"
#include "SketchFunction.h"

#include "GenericFile.h"
#include "SolverLanguagePrimitives.h"
#include "File.h"

#include "SolverLanguageYaccHeader.h"
#include "FunctionMapTransformerLanguage.h"


template<typename StateType>
void SL::Var::run(StateType *state)  {
    state->add_var(this);
}

template<typename StateType>
SL::VarVal* SL::Var::eval(StateType *state) {
    return state->get_var_val(this);
}

SL::SLType * SL::Var::get_type() {
    return type;
}


template<typename StateType>
void SL::While::run(StateType* state)
{
    int iteration_count = 0;
    while(expression->eval(state)->get_bool(true, false))
    {
        body->run(state);
        if(state->has_return()) {
            break;
        }
        iteration_count++;
    }
}

void SL::While::clear()
{
    //clears everything
    expression->clear();
    body->clear();
    delete this;
}

SL::While::While(While* to_copy){
    expression = new Expression(to_copy->expression);
    body = new CodeBlock(to_copy->body);
}

template<typename StateType>
void SL::For::run(StateType* state)
{
    state->open_subframe();
    def->run(state);
    int iteration_count = 0;
    while(expression->eval(state)->get_bool(true, false))
    {
        body->run(state);
        if(state->has_return()) {
            break;
        }
        plus_plus->run(state);
        iteration_count++;
    }
    state->close_subframe();
}

void SL::For::clear() {
    //clears everything
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

template<typename StateType>
void SL::If::run(StateType *state) {
    if(expression->eval(state)->get_bool(true, false))
    {
        body->run(state);
    } else{
        if(else_body != nullptr)
        {
            else_body->run(state);
        }
    }
}

void SL::If::clear() {
    //clears everything
    expression->clear();
    body->clear();
    if(else_body != nullptr)
    {
        else_body->clear();
    }
    delete this;
}

SL::If::If(SL::If *to_copy) {
    expression = new Expression(to_copy->expression);
    body = new CodeBlock(to_copy->body);
    if(to_copy->else_body != nullptr)
    {
        else_body = new CodeBlock(to_copy->else_body);
    }
}

template<typename StateType>
void SL::Return::run(StateType *state){
    state->set_return_var_val(expression->eval(state));
}

void SL::Return::clear() {
    //clears everything
    expression->clear();
    delete this;
}

template<typename StateType>
void SL::Assignment::run(StateType *state)
{
    Var* to_var = nullptr;
    switch (dest_type) {
        case name_dest_type: {
            try {
                to_var = state->name_to_var_throws(dest_name);
            }
            catch (exception &e) {
                SL::SLType *any_type = new SL::SLType(new SL::Identifier("any"));
                SL::Identifier *copy_name = new SL::Identifier(dest_name);
                SL::Var *var = new SL::Var(any_type, copy_name);
                state->add_var(var);
                var->clear();
                to_var = state->name_to_var(dest_name);
            }
            break;
        }
        case var_dest_type: {
            to_var = dest_var;
            state->add_var(to_var);
            break;
        }
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
    //clears everything
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

int SL::Identifier::global_identifier_id = 0;

template<typename StateType>
SL::VarVal* SL::Identifier::eval(StateType *state) {
    return state->get_var_val(state->name_to_var(this));
}

bool SL::Identifier::is_defined() const {
    return defined;
}

SL::VarVal* SL::FunctionCall::eval_type_constructor(ProgramState* state)
{
    string root_type_name = type_constructor->get_head()->to_string();

    enum RootType {_vector, _pair, _map, _unknown_type_constructor};

    RootType root_type = _unknown_type_constructor;

    if(root_type_name == "pair")
    {
        root_type = _pair;
    }
    else if(root_type_name == "vector")
    {
        root_type = _vector;
    }
    else if(root_type_name == "map")
    {
        root_type = _map;
    }

    switch (root_type) {
        case _pair: {
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
        case _vector: {
            PolyType* type_params = type_constructor->get_type_params();
            assert(type_params->size() == 1);
            PolyVec* ret = new PolyVec(new PolyType(type_params));
            for(int i = 0;i<params.size();i++) {
                ret->push_back(params[i]->eval(state));
            }
            return new SL::VarVal(ret);
            break;
        }
        case _map: {
            PolyType* type_params = type_constructor->get_type_params();
            assert(type_params->size() == 2);
            assert(*type_params->at(0) == SL::SLType(new SL::Identifier("string")));

            PolyMap* ret = new PolyMap(new PolyType(type_params));

            for(int i = 0;i<params.size();i++)
            {
                VarVal* pair_var_val = params[i]->eval(state);
                pair_var_val->increment_shared_ptr();
                PolyPair* poly_pair = pair_var_val->get_poly_pair();
                string key = poly_pair->first()->get_string();
                VarVal* element = poly_pair->second();

                ret->insert(key, element);

                pair_var_val->decrement_shared_ptr();
            }

            return new SL::VarVal(ret);
            break;
        }
        case _unknown_type_constructor:
            assert(false);
            break;
        default:
            assert(false);
    }
    assert(false);
}

template<typename StateType>
SL::VarVal *SL::FunctionCall::eval(SL::PolyVec*& poly_vec, StateType* state, const SL::VarVal* const the_var_val)
{
    assert(poly_vec == the_var_val->get_poly_vec_const(false));
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
            poly_vec->push_back(params[0]->eval(state));
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


//template SL::VarVal *SL::FunctionCall::eval<SolverProgramState, GenericFile>(
//        GenericFile*& file, SolverProgramState *state, const SL::VarVal* const the_var_val);

template<typename StateType, typename FileType>
SL::VarVal *SL::FunctionCall::eval(FileType*& file, StateType *state, const SL::VarVal* const the_var_val) {
    if(is_same<FileType, File>::value) {
        assert((File*)file == the_var_val->get_file_const(false));
    }
    else if(is_same<FileType, GenericFile>::value) {
        assert((GenericFile*)file == the_var_val->get_generic_file_const(false));
    }
    else {
        AssertDebug(false, "TODO: Integrate new type of file. Why do you need a new type of file? Must be interesting.");
    }
    switch (method_id) {
        case _set: {
            assert(params.size() == 3);
            int row_idx = params[0]->eval(state)->get_int(true, false);
            int col_idx = params[1]->eval(state)->get_int(true, false);
            int val = params[2]->eval(state)->get_int(true, false);
            if constexpr(is_same<FileType, File>::value) {
                File* _file = static_cast<File *>(file);
                _file->at(row_idx)->_getObj(col_idx).setVal(val);
                assert(_file->at(row_idx)->operator[](col_idx) == val);
            }
            else
            {
                assert(false);
            }
            return new VarVal();
            break;
        }
        case _produce_subset_file:
        {
            assert(params.size() == 1);
            int num_rows = params[0]->eval(state)->get_int(false);
            return new SL::VarVal(file->sample_sub_file(num_rows));
            break;
        }
        case _size:
        {
            assert(params.empty());
            return new SL::VarVal((int) file->size());
            break;
        }
        case _get: {
            if (params.size() == 1) {
                int row_id = params[0]->eval(state)->get_int(true, false);
                if (is_same<FileType, File>::value) {
                    return new SL::VarVal(new InputVarStore(*((File *) file)->at(row_id)));
                } else if (is_same<FileType, GenericFile>::value) {
                    return new SL::VarVal(((GenericFile *) file)->at(row_id));
                } else {
                    AssertDebug(false,
                                "TODO: Integrate new type of file. Why do you need a new type of file? Must be interesting.");
                }
            } else if (params.size() == 2) {
                assert((is_same<FileType, File>::value));
                int row_id = params[0]->eval(state)->get_int(true, false);
                int col_id = params[1]->eval(state)->get_int(true, false);
                auto obj = ((File*)file)->at(row_id)->getObjConst(col_id);
                assert(!obj.get_is_array());
                return new SL::VarVal((int)obj.getInt());
            }
            else if (params.size() == 3) {
                assert((is_same<FileType, File>::value));
                int row_id = params[0]->eval(state)->get_int(true, false);
                int col_id = params[1]->eval(state)->get_int(true, false);
                int vec_id = params[2]->eval(state)->get_int(true, false);
                auto obj = ((File*)file)->at(row_id)->getObjConst(col_id);
                assert(obj.get_is_array());
                int ret = obj.getInt(vec_id);
                assert(ret != -1);
                return new SL::VarVal((int)ret);
            }
            else {
                assert(false);
            }
            break;
        }
        case _clear:
        {
            assert(params.empty());
            file->clear();
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

            if(is_same<FileType, File>::value) {
                std::function<bool(const VarStore*) > lambda_condition = [&state, &condition_method](const VarStore* x) {
                    vector<SL::VarVal*> local_inputs;
                    local_inputs.push_back(new VarVal(new InputVarStore(*x)));
                    auto var_val = condition_method->eval(state, local_inputs);
                    auto ret = var_val->get_bool(true, false);
                    return ret;
                };

                File* ret = ((File*)file)->produce_filter(lambda_condition);

                method_var_val->decrement_shared_ptr();
                return new VarVal(ret);
            }
            else if(is_same<FileType, GenericFile>::value) {
                std::function<bool(string) > lambda_condition = [&state, &condition_method](string x) {
                    vector<SL::VarVal*> local_inputs;
                    local_inputs.push_back(new VarVal(std::move(x)));
                    auto var_val = condition_method->eval(state, local_inputs);
                    auto ret = var_val->get_bool(true, false);
                    return ret;
                };

                GenericFile* ret = ((GenericFile*)file)->produce_filter(lambda_condition);

                method_var_val->decrement_shared_ptr();
                return new VarVal(ret);
            }
            else {
                AssertDebug(false, "TODO: Integrate new type of file. Why do you need a new type of file? Must be interesting.");
            }
        }
        case _relabel:
        {
            assert((is_same<FileType, File>::value));
            assert(params.size() == 1);
            VarVal* skfunc_var_val = params[0]->eval(state);
            skfunc_var_val->increment_shared_ptr();
            ((File*)file)->relabel(skfunc_var_val->get_skfunc());
            skfunc_var_val->decrement_shared_ptr();
            return new VarVal();
        }
        case _clone:
        {
            return new VarVal(new FileType(file));
            break;
        }
        case _append:
        {
            assert(params.size() == 1);
            VarVal* var_store_var_val = params[0]->eval(state);
            assert((is_same<FileType, File>::value));
            ((File*)file)->push_back(new VarStore(*(var_store_var_val->get_input_holder_const(false))));
            return new VarVal();
            break;
        }
        default:
            assert(false);
    }
    assert(false);
}

template<>
SL::VarVal* SL::FunctionCall::eval_global<ProgramState>(ProgramState *state) {
    AssertDebug(false, "SHOULD NOT BE NECESSARY.")
}

template<>
SL::VarVal* SL::FunctionCall::eval_global<FMTL::FunctionMapTransformerState>(FMTL::FunctionMapTransformerState *state)
{
    switch (method_id) {
        case _declare:
        {
            assert(params.size() == 3);

            string skfunc_name_var_val = params[0]->eval(state)->get_string(true, false);

            SketchFunction* skfunc =
                    state->get_source_skfunc(skfunc_name_var_val)->
                    deep_exact_clone_and_fresh_function_map(state->get_env(), state->get_meta_map_dp());

            assert(skfunc_name_var_val == skfunc->get_dag_name());

//            state->function_map.insert(skfunc_name_var_val, skfunc);

            assert(state->function_map.find(skfunc->get_dag_name()) != state->function_map.end());

            { // assert holes invariant
                VarVal *holes_vec_var_val = params[1]->eval(state);
                holes_vec_var_val->increment_shared_ptr();
                PolyVec *holes = holes_vec_var_val->get_poly_vec();

                vector<string> hole_names = skfunc->get_unit_holes();
                assert(hole_names.size() == holes->size());
                for (int i = 0; i < hole_names.size(); i++) {
                    string predicted_hole_name = holes->at(i)->get_string(false);
                    assert(hole_names[i] == predicted_hole_name);
                }

                holes_vec_var_val->decrement_shared_ptr();
            }

            { // assert ports invariant
                VarVal *ports_map_var_val = params[2]->eval(state);
                ports_map_var_val->increment_shared_ptr();
                PolyMap *ports_map = ports_map_var_val->get_poly_map();

                auto ufuns_map = skfunc->get_unit_ufuns_map();
                assert(ufuns_map.size() == ports_map->size());
                for (const auto &it: ufuns_map) {
                    auto left = ports_map->at(it.first)->get_string(false);
                    auto right =  it.second;
                    assert(left == right);
                }

                ports_map_var_val->decrement_shared_ptr();
            }

            return new VarVal(skfunc);
        }
        default:
            assert(false);
    }
    assert(false);
}

void set_inlining_tree(VarStore* sol, BooleanDagUtility* harness)
{
    assert(sol->get_inlining_tree() == nullptr);

    VarStore* append_sol = harness->get_inlining_tree(true)->get_solution();
    LightInliningTree* harness_inlining_tree = new LightInliningTree(harness->get_inlining_tree());
    harness_inlining_tree->set_var_store(sol);
    sol->disjoint_join_with(*append_sol);
    sol->set_inlining_tree(harness_inlining_tree);
}


template<>
SL::VarVal* SL::FunctionCall::eval_global<SolverProgramState>(SolverProgramState *state)
{
    switch (method_id) {
        case _file: {
#ifdef USE_GENERIC_FILE
            assert(params.size() == 1 || params.size() == 2);
            string file_name = params[0]->eval(state)->get_string();
            SL::VarVal *ret = new SL::VarVal(new GenericFile(file_name, state->args.seed));
            return ret;
#else
            if(params.size() == 0) {
                return new SL::VarVal(new File());
            }
            else if(params.size() == 2) {
                VarVal* file_name_var_val = params[0]->eval(state);
                file_name_var_val->increment_shared_ptr();
                string file_name = file_name_var_val->get_string();
                file_name_var_val->decrement_shared_ptr();
                SketchFunction *harness = params[1]->eval(state)->get_skfunc();
                SL::VarVal *ret = new SL::VarVal(new File(harness, file_name, state->floats, state->args.seed));
                return ret;
            }
            else {
                assert(false);
            }
#endif
            break;
        }
        case _sat_solver:
        {
            assert(params.size() == 2);

            VarVal* param_var_val = params[0]->eval(state);
            param_var_val->increment_shared_ptr();
            SketchFunction* skfunc = param_var_val->get_skfunc();

            vector<string> prev_holes = skfunc->get_deep_holes();

//            BooleanDagUtility* harness = ((BooleanDagUtility*)skfunc)->produce_inlined_dag(true);
//            harness->increment_shared_ptr();

            SketchFunction* harness = skfunc->deep_exact_clone_and_fresh_function_map();
            harness->increment_shared_ptr();

            assert(harness->get_dag()->check_ctrl_node_source_dag_naming_invariant());

            harness->inline_this_dag(false);

//            assert(harness->get_dag()->check_ctrl_node_source_dag_naming_invariant());

            const bool concretize_after_solving = true;
            if(!concretize_after_solving) {
                param_var_val->decrement_shared_ptr();
            }

            vector<string> after_holes = harness->get_deep_holes();
            sort(prev_holes.begin(), prev_holes.end());
            sort(after_holes.begin(), after_holes.end());
//            assert(prev_holes.size() == after_holes.size());

            FILE_TYPE* file = nullptr;

#ifdef USE_GENERIC_FILE
            file = params[1]->eval(state)->get_generic_file();
#else
            file = params[1]->eval(state)->get_file();
            assert(file->like_unused());
#endif
            assert(file != nullptr);

            using namespace SolverLanguagePrimitives;
            auto* solver = new WrapperAssertDAG(state->floats, state->hc, state->args, state->hasGoodEnoughSolution);

//            assert(harness->get_dag()->check_ctrl_node_source_dag_naming_invariant());

            auto* problem = new ProblemAE(harness, file);

            HoleVarStore* sol = (solver)->solve(problem);

            set_inlining_tree(sol, harness);

//            assert(sol->get_inlining_tree() == nullptr);
//
//            VarStore* append_sol = harness->get_inlining_tree()->get_solution();
//            LightInliningTree* harness_inlining_tree = new LightInliningTree(harness->get_inlining_tree());
//            harness_inlining_tree->set_var_store(sol);
//            sol->disjoint_join_with(*append_sol);
//            sol->set_inlining_tree(harness_inlining_tree);

            if(concretize_after_solving) {
                //make sure produce_concretize-s
                SketchFunction *to_test = skfunc->produce_concretization(sol, bool_node::CTRL, true, true, true);
                to_test->increment_shared_ptr();
                to_test->clear();
                param_var_val->decrement_shared_ptr();
            }

#ifndef USE_GENERIC_FILE
            file->reset();
            assert(file->like_unused());
#endif
            harness->clear();
            solver->clear();
            delete problem;

            return new SL::VarVal(sol);
            break;
        }
        case _batch_evaluation_solver:
        {
            assert(params.size() == 2);

            VarVal* param_var_val = params[0]->eval(state);
            param_var_val->increment_shared_ptr();
            SketchFunction* skfunc = param_var_val->get_skfunc();

            vector<string> prev_holes = skfunc->get_deep_holes();

//            BooleanDagUtility* harness = ((BooleanDagUtility*)skfunc)->produce_inlined_dag(true);
//            harness->increment_shared_ptr();

            SketchFunction* harness = skfunc->deep_exact_clone_and_fresh_function_map();
            harness->increment_shared_ptr();

            assert(harness->get_dag()->check_ctrl_node_source_dag_naming_invariant());

            harness->inline_this_dag();

//            assert(harness->get_dag()->check_ctrl_node_source_dag_naming_invariant());

            const bool concretize_after_solving = true;
            if(!concretize_after_solving) {
                param_var_val->decrement_shared_ptr();
            }

            vector<string> after_holes = harness->get_deep_holes();
            sort(prev_holes.begin(), prev_holes.end());
            sort(after_holes.begin(), after_holes.end());
            assert(prev_holes.size() == after_holes.size());

            FILE_TYPE* file = nullptr;

#ifdef USE_GENERIC_FILE
            file = params[1]->eval(state)->get_generic_file();
#else
            file = params[1]->eval(state)->get_file();
            assert(file->like_unused());
#endif
            assert(file != nullptr);

            using namespace SolverLanguagePrimitives;
            auto* solver = new WrapperBatchEvaluatorSolver(state->floats, state->hc, state->args, state->hasGoodEnoughSolution);

//            assert(harness->get_dag()->check_ctrl_node_source_dag_naming_invariant());

            auto* problem = new ProblemAE(harness, file);

            HoleVarStore* sol = (solver)->solve(problem);

            set_inlining_tree(sol, harness);

//            assert(sol->get_inlining_tree() == nullptr);
//
//            VarStore* append_sol = harness->get_inlining_tree()->get_solution();
//            LightInliningTree* harness_inlining_tree = new LightInliningTree(harness->get_inlining_tree());
//            harness_inlining_tree->set_var_store(sol);
//            sol->disjoint_join_with(*append_sol);
//            sol->set_inlining_tree(harness_inlining_tree);

            if(concretize_after_solving) {
                //make sure produce_concretize-s
                SketchFunction *to_test = skfunc->produce_concretization(sol, bool_node::CTRL, true, true, true);
                to_test->increment_shared_ptr();
                to_test->clear();
                param_var_val->decrement_shared_ptr();
            }

#ifndef USE_GENERIC_FILE
            file->reset();
            assert(file->like_unused());
#endif
            harness->clear();
            solver->clear();
            delete problem;

            return new SL::VarVal(sol);
            break;
        }
        case _print:
        {
            assert(!params.empty());
            string print_str = "";
            cout << "print(";
            for(int i = 0;i<params.size();i++)
            {
                string out_str = params[i]->eval(state)->to_string(true, false);
                cout << out_str << " ";
                state->console_output << out_str << " ";
            }
            state->console_output << endl;
            cout << "); //console_output.out" << endl;
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
                    message_prefix += params[i]->eval(state)->to_string(true, false) + " ";
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
        case _timestamp:
        {
            assert(params.empty());
            auto at_time = std::chrono::steady_clock::now();
            auto elapsed = chrono::duration_cast<chrono::microseconds>(at_time - state->prev_timestep).count();
            state->console_output << "since_prev : " << elapsed << endl;
            state->console_output << "since_start: " << chrono::duration_cast<chrono::microseconds>(at_time - state->start_of_run).count() << endl;
            state->prev_timestep = std::chrono::steady_clock::now();
            return new VarVal();
        }
        default:
            assert(false);
    }
    assert(false);
}

template<typename StateType>
SL::VarVal *SL::FunctionCall::eval(
        HoleVarStore* & the_solution, StateType *state, const SL::VarVal* const the_var_val) {
    assert(the_solution == the_var_val->get_solution_const(false));
    switch (method_id) {
        case _join: {
            assert(params.size() == 1);
            using namespace SolverLanguagePrimitives;
            HoleVarStore *other_solution = params[0]->eval(state)->get_hole_var_store();
            the_solution->disjoint_join_with(*other_solution);
            return new VarVal();
        }
//        case _get: {
//            assert(params.size() == 1);
//            string subfunc_name = params[0]->eval(state)->get_string(true, false);
//            return new VarVal(the_solution->get_assignment()->get_inlining_tree()->get_sub_inlining_tree(subfunc_name)->get_hole_var_store());
//        }
        default:
            assert(false);
    }
    assert(false);
}

template<typename StateType>
SL::VarVal *SL::FunctionCall::eval(
        int& the_int, StateType *state, const SL::VarVal* const the_var_val) {
    assert(the_int == the_var_val->get_int_const(false));
    switch (method_id) {
        case _get_bit: {
            assert(params.size() == 1);
            int bit_idx = params[0]->eval(state)->get_int(false);
            assert(bit_idx <= 31);
            return new VarVal((int)((the_int & (1<<bit_idx)) != 0));
            break;
        }
        case _add: {
            assert(params.size() == 1);
            int add_delta = params[0]->eval(state)->get_int(false);
            the_int += add_delta;
            return new VarVal();
            break;
        }
        default: {
            assert(false);
            break;
        }
    }
    assert(false);
}

template<typename StateType>
SL::VarVal* SL::FunctionCall::eval(SL::PolyPair*& poly_pair, StateType* state, const SL::VarVal* const the_var_val)
{
    assert(poly_pair == the_var_val->get_poly_pair_const(false));
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

template<typename StateType>
SL::VarVal* SL::FunctionCall::eval(StateType *state)
{

    cout << "|ENTERING |" << to_string() + "|.SL::FunctionCall::eval(state)" << endl;

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
#ifdef USE_GENERIC_FILE
                    assert(input_val->is_string());
                    string input_assignment = input_val->get_string();
#else
                    assert(input_val->is_input_holder());
                    InputVarStore *input_assignment = input_val->get_input_holder();
#endif

                    SketchFunction* skfunc = method_var_val->get_skfunc();
                    ret = SketchFunctionEvaluator::eval(skfunc, input_assignment);
                    assert(ret != nullptr);

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

template<typename StateType>
void SL::FunctionCall::run(StateType *state) {
    SL::VarVal* ret = eval(state);
    assert(ret->is_void());
    ret->clear_assert_0_shared_ptrs();
}

pair<SL::Var *, SL::VarVal* > SL::FunctionCall::get_var_and_var_val_and_assert_type(
        ProgramState* state, vector<string> type_names) {

    assert(type_names.size() >= 1);

    string type_name = type_names[0];

    //if expression is nullptr, then one of the possible type_names must be "namespace"
    if(expression == nullptr) {
        bool one_possible_type_is_namespace = false;
        for (const auto& it: type_names) {
            if (it == "namespace") {
                one_possible_type_is_namespace = true;
            }
        }
        assert(one_possible_type_is_namespace);
        return make_pair(nullptr, nullptr);
    }

    assert(expression != nullptr);

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
    if(!enter)
    {
        cout << "IN get_var_and_var_val_and_assert_type" << endl;
        cout << "The function accepts only the types [";
        for(int i = 0;i<type_names.size();i++)
        {
            cout << type_names[i] <<" ";
        }
        cout << "]";
        cout << " But it's called on an object with type '" + var_type_str + "'" << endl;
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

void SL::add_to_method_str_to_method_id_map(
        const string& method_str, SL::MethodId method_id, string type_str_1, string type_str_2, string type_str_3)
{
    assert(method_str_to_method_id_map.find(method_str) == method_str_to_method_id_map.end());
    method_str_to_method_id_map[method_str] = method_id;
    assert(method_id_to_type_str.find(method_id) == method_id_to_type_str.end());
    assert(!type_str_1.empty());
    method_id_to_type_str[method_id] = vector<string>();
    method_id_to_type_str[method_id].push_back(type_str_1);
    if(!type_str_2.empty()) {
        method_id_to_type_str[method_id].push_back(type_str_2);
    }
    else{
        assert(type_str_3.empty());
    }
    if(!type_str_3.empty()){
        assert(!type_str_2.empty());
        method_id_to_type_str[method_id].push_back(type_str_3);
    }
}

void SL::init_method_str_to_method_id_map()
{
    assert(method_str_to_method_id_map_is_defined == false);
    add_to_method_str_to_method_id_map("File", _file, "namespace");
    add_to_method_str_to_method_id_map("SATSolver", _sat_solver, "namespace");
    add_to_method_str_to_method_id_map("BatchEnumerationSolver", _batch_evaluation_solver, "namespace");
//    add_to_method_str_to_method_id_map("Solution", _Solution, "namespace");
    add_to_method_str_to_method_id_map("print", _print, "namespace");
    add_to_method_str_to_method_id_map("timestamp", _timestamp, "namespace");
    add_to_method_str_to_method_id_map("float", _to_float, "namespace");
    add_to_method_str_to_method_id_map("assert", _assert, "namespace");
    add_to_method_str_to_method_id_map("not", _not, "namespace");

    add_to_method_str_to_method_id_map("join", _join, "Solution");
    add_to_method_str_to_method_id_map("clone", _clone,  "Solution", "File");
    add_to_method_str_to_method_id_map("set", _set,  "File");
    add_to_method_str_to_method_id_map("get_bit", _get_bit,  "int");
    add_to_method_str_to_method_id_map("add", _add,  "int");

    add_to_method_str_to_method_id_map("first", _first, "pair");
    add_to_method_str_to_method_id_map("second", _second, "pair");
    add_to_method_str_to_method_id_map("sort", _sort_vec, "vector");
    add_to_method_str_to_method_id_map("reverse", _reverse, "vector");
    add_to_method_str_to_method_id_map("append", _append, "vector", "File");
    add_to_method_str_to_method_id_map("size", _size, "File", "vector", "SketchFunction");
    add_to_method_str_to_method_id_map("produce_filter", _produce_filter, "File");
    add_to_method_str_to_method_id_map("relabel", _relabel, "File");
    add_to_method_str_to_method_id_map("produce_subset_file", _produce_subset_file, "File");

    add_to_method_str_to_method_id_map("clear", _clear, "SketchFunction", "File");
    add_to_method_str_to_method_id_map("get", _get, "File", "vector", "SketchFunction");

    add_to_method_str_to_method_id_map("passes", _passes, "SketchFunction");
    add_to_method_str_to_method_id_map("num_holes", _num_holes, "SketchFunction");
    add_to_method_str_to_method_id_map("get_solution", _get_solution, "SketchFunction");
    add_to_method_str_to_method_id_map("reset", _reset, "SketchFunction");

    add_to_method_str_to_method_id_map("produce_executable", _produce_executable, "SketchFunction");
    add_to_method_str_to_method_id_map("make_executable", _make_executable, "SketchFunction");

    add_to_method_str_to_method_id_map("produce_deep_concretize", _produce_deep_concretize, "SketchFunction");
//    add_to_method_str_to_method_id_map("_produce_unit_concretize", _produce_unit_concretize, "SketchFunction");
    add_to_method_str_to_method_id_map("inplace_deep_concretize", _inplace_deep_concretize, "SketchFunction");
    add_to_method_str_to_method_id_map("inplace_unit_concretize", _inplace_unit_concretize, "SketchFunction");

    add_to_method_str_to_method_id_map("deep_clone", _deep_clone, "SketchFunction");
    add_to_method_str_to_method_id_map("unit_clone", _unit_clone, "SketchFunction");

    add_to_method_str_to_method_id_map("produce_replace", _produce_unit_replace, "SketchFunction");
    add_to_method_str_to_method_id_map("replace", _inplace_unit_replace, "SketchFunction");
//    add_to_method_str_to_method_id_map("produce_deep_replace", _produce_deep_replace, "SketchFunction");
//    add_to_method_str_to_method_id_map("inplace_deep_replace", _inplace_deep_replace, "SketchFunction");

/*
        _produce_executable,
        _make_executable,
        //--
        _produce_deep_concretize,
        //produce_unit_concretize
        _inplace_deep_concretize,
        //inplace_unit_concretize,
        /--
        _deep_clone,
        _unit_clone,
        /--
        //_inline_deep_replace,
        _replace, // _inline_unit_replace
        //_produce_deep_replace,
        _produce_replace, // _produce_unit_replace
 */

    //FMTL Primitives:

    add_to_method_str_to_method_id_map("declare", _declare, "namespace");
    add_to_method_str_to_method_id_map("vectorized_count_passing_inputs", _vectorized_count_passing_inputs, "SketchFunction");
    add_to_method_str_to_method_id_map("evaluate_inputs", _evaluate_inputs, "SketchFunction");


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

string SL::FunctionCall::to_string(){
    string ret;
    if(expression != nullptr)
    {
        ret += expression->to_string() + ".";
    }
    switch (method_meta_type) {

        case name_meta_type:
            ret += method_name->to_string();
            break;
        case type_constructor_meta_type:
            ret += type_constructor->to_string();
            break;
        default:
            assert(false);
    }
    ret += "(";
    bool first = true;
    for(auto param:params)
    {
        if(first) {
            first = false;
        }
        else {
            ret+=",";
        }
        ret += param->to_string();
    }
    ret += ")";
    return ret;
}
void
eval__sketch_function_replace(const SL::VarVal * const ret_var_val, SketchFunction *ret_skfunc, ProgramState *state,
                              const vector<SL::Param *> &params)
{
    AssertDebug(ret_var_val->get_function_const(false) == ret_skfunc, "ret_skfunc must be the same as what ret_var_val is holding.");
    assert(params.size() == 2);

    string str_to_replace = params[0]->eval(state)->get_string(true, false);
    SL::VarVal* to_replace_with_var_val = params[1]->eval(state);

//    assert(to_replace_with_var_val->get_skfunc(false)->get_dag_name() != ret_skfunc->get_dag_name());

//    ret_var_val->add_dependency(str_to_replace, to_replace_with_var_val);

    to_replace_with_var_val->increment_shared_ptr();
    SketchFunction* to_replace_with_skfunc = to_replace_with_var_val->get_skfunc();
    const string& to_replace_with_name = to_replace_with_skfunc->get_dag()->get_name();

    state->add_to_function_map(to_replace_with_name, to_replace_with_skfunc);
    assert(state->function_map.find(to_replace_with_name) != state->function_map.end());

    ret_skfunc->replace(str_to_replace, to_replace_with_name);
    to_replace_with_var_val->decrement_shared_ptr();
}


template<typename StateType>
SL::VarVal *SL::FunctionCall::eval(SketchFunction*& skfunc, StateType *state, const VarVal* const the_var_val) {

    assert(skfunc == the_var_val->get_function_const(false));
    switch (method_id) {
        case _inplace_unit_concretize:
        {
            assert(params.size() == 1);
            VarVal* poly_map_var_val = params[0]->eval(state);
            poly_map_var_val->increment_shared_ptr();
            map<string, string> hole_assignment = poly_map_var_val->get_poly_map()->get_cpp_map<string>();

            auto unit_holes = skfunc->get_unit_holes();
            assert(hole_assignment.size() == unit_holes.size());

            VarStore* var_store = nullptr;

            if(!unit_holes.empty()) {
                string line;
                for (int i = 0; i < unit_holes.size(); i++) {
                    assert(hole_assignment.find(unit_holes[i]) != hole_assignment.end());
                    if (i >= 1) {
                        line += " ";
                    }
                    line += hole_assignment[unit_holes[i]];
                }

                line += "\n";

                var_store = string_to_var_store(line, skfunc, bool_node::CTRL);

                SketchFunction* harness = skfunc->deep_exact_clone_and_fresh_function_map();
                harness->increment_shared_ptr();
                harness->inline_this_dag();

                set_inlining_tree(var_store, harness);
                harness->clear();
            }
            else {
                //nothing to concretize;
            }

            if(state->function_map.find(skfunc->get_dag_name()) == state->function_map.end()) {
                state->function_map.insert(skfunc->get_dag_name(), skfunc);
            }

            skfunc->_inplace_recursive_concretize(var_store, bool_node::CTRL, true);

            if(var_store != nullptr) {
                var_store->clear();
            }

            return new VarVal();
        }
        case _produce_executable:
        {
            if(params.size() >= 1) {
                using namespace SolverLanguagePrimitives;
                VarVal *var_val_sol = params[0]->eval(state);
                var_val_sol->increment_shared_ptr();
                HoleVarStore *var_store = var_val_sol->get_hole_var_store();
                SketchFunction* concretized_function = skfunc->produce_concretization(var_store, bool_node::CTRL, true);
                var_val_sol->decrement_shared_ptr();
                if(params.size() == 2)
                {
                    int dag_id_from_the_user = params[1]->eval(state)->get_int();
                    concretized_function->set_dag_id_from_the_user(dag_id_from_the_user);
                }
                else
                {
                    assert(params.size() == 1);
                }
                return new SL::VarVal(concretized_function);
            }
            else if(params.empty()) {
                SketchFunction *concretized_function = skfunc->produce_executable();
                return new SL::VarVal(concretized_function);
            }
            else {
                assert(false);
            }
            break;
        }
        case _make_executable:
        {
            if(params.size() == 1) {
                using namespace SolverLanguagePrimitives;
                VarVal *sol_var_val = params[0]->eval(state);
                sol_var_val->increment_shared_ptr();
                HoleVarStore *sol = sol_var_val->get_hole_var_store();
                skfunc->concretize(sol);
                sol_var_val->decrement_shared_ptr();
                return new SL::VarVal();
            }
            else if(params.empty()) {
                skfunc->make_executable();
                return new SL::VarVal();
            }
            else {
                assert(false);
            }
            break;
        }
        case _passes:
        {
//            assert(params.size() == 1);
            assert(params.size() >= 1);
            bool do_assert = false;
            if(params.size() == 2) {
                do_assert = params[1]->eval(state)->get_bool();
            }
            else {
                assert(params.size() == 1);
            }

            VarVal* input_holder_var_val = params[0]->eval(state);
            input_holder_var_val->increment_shared_ptr();
#ifdef USE_GENERIC_FILE
            string input_holder = input_holder_var_val->get_string();
#else
            InputVarStore* input_holder = input_holder_var_val->get_input_holder();
#endif
            auto ret_predicted = SketchFunctionEvaluator::new_passes(skfunc, input_holder, do_assert);
            input_holder_var_val->decrement_shared_ptr();
            return ret_predicted;
            break;
        }
        case _clear:
        {
            assert(params.empty());
            skfunc->clear();
            skfunc = nullptr;
            return new VarVal();
            break;
        }
        case _num_holes: {
            assert(params.empty());
            int init_num_global_dags = BooleanDAG::get_allocated().size();

            SketchFunction* func_clone = skfunc->deep_exact_clone_and_fresh_function_map();
            func_clone->increment_shared_ptr();
            func_clone->inline_this_dag(false);
            int num_ctrls = (int) func_clone->get_dag()->getNodesByType(bool_node::CTRL).size();
            func_clone->clear();

            int dags_diff = BooleanDAG::get_allocated().size() - init_num_global_dags;
            assert(dags_diff == 0);

            return new VarVal(num_ctrls);
            break;
        }
        case _size: {
            assert(params.empty());
            return new VarVal((int)skfunc->get_dag()->size());
            break;
        }
        case _deep_clone:
        {
            assert(params.size() <= 1);
            SketchFunction* ret = skfunc->deep_clone();
            if(params.size() == 1)
            {
                int dag_id_from_user_id = params[0]->eval(state)->get_int();
                ret->set_dag_id_from_the_user(dag_id_from_user_id);
            }
            return new VarVal(ret);
        }
        case _unit_clone:
        {
            if(params.empty()) {
                SketchFunction* ret = skfunc->unit_clone();
                return new VarVal(ret);
            }
            else if(params.size() == 2) {
                string clone_name = params[0]->eval(state)->get_string(true, false);

                VarVal* hole_rename_poly_map_var_val = params[1]->eval(state);
                hole_rename_poly_map_var_val->increment_shared_ptr();

                PolyMap* hole_rename_poly_map = hole_rename_poly_map_var_val->get_poly_map();

                map<string, string> hole_rename_map = hole_rename_poly_map->get_cpp_map<string>();

                SketchFunction* ret = skfunc->unit_clone(clone_name, &hole_rename_map);

                hole_rename_poly_map_var_val->decrement_shared_ptr();

                assert(ret->get_dag_name() == clone_name);

                for(auto node: ret->get_dag()->getNodesByType(bool_node::CTRL))
                {
                    CTRL_node* ctrl = (CTRL_node*)node;
                    string original_name = ctrl->get_original_name();
                    string new_name = ctrl->get_name();

                    if(original_name != "#PC") {
                        assert(hole_rename_map.find(original_name) != hole_rename_map.end());
                        assert(hole_rename_map[original_name] == new_name);
                    }
                }

                return new VarVal(ret);
            }
            else {
                AssertDebug(false, "WRONG NUMBER OF PARAMETERS.");
            }
            break;
        }
        case _produce_unit_replace:
        {
            assert(params.size() == 2);
            SketchFunction* ret_skfunc = skfunc->unit_clone();
            VarVal* ret_var_val = new VarVal(ret_skfunc);
            eval__sketch_function_replace(ret_var_val, ret_skfunc, state, params);
            return ret_var_val;
            break;
        }
        case _inplace_unit_replace:
        {
            assert(params.size() == 2);
            eval__sketch_function_replace(the_var_val, skfunc, state, params);
            return new VarVal();
            break;
        }
        case _get_solution:
        {
            if(params.empty()) {
                return new VarVal((HoleVarStore*)skfunc->get_solution());
            }
            else if(params.size() == 1){
                AssertDebug(false, "TODO: equivalent to skfunc.get(name).get_hole_var_store()");
            }
            else
            {
                assert(false);
            }
        }
        case _get:
        {
            assert(params.size() == 1);

            string subfunc_name = params[0]->eval(state)->get_string(true, false);

            return new VarVal(skfunc->produce_get(subfunc_name));
        }
        case _reset:
        {
            string subfunc_name = params[0]->eval(state)->get_string(true, false);
            skfunc->reset(subfunc_name);
            return new VarVal();
        }
        case _vectorized_count_passing_inputs:
        {
            assert(params.size() == 1);

            VarVal *file_var_val = params[0]->eval(state);
            file_var_val->increment_shared_ptr();
            File* file = file_var_val->get_file();

            int ret = skfunc->count_passing_inputs(file, false);
            file_var_val->decrement_shared_ptr();

            return new VarVal(ret);
            break;
        }
        case _evaluate_inputs:
        {
            assert(params.size() == 1);

            VarVal *file_var_val = params[0]->eval(state);
            file_var_val->increment_shared_ptr();
            File* file = file_var_val->get_file();

            PolyVec* ret = skfunc->evaluate_inputs(file);
            file_var_val->decrement_shared_ptr();

            return new VarVal(ret);
            break;
        }
        default:
            assert(false);
    }
    assert(false);
    return nullptr;
}

template<typename StateType, typename T>
SL::VarVal* SL::Method::eval(StateType *state, vector<T>& inputs)  {
    run(state, inputs);
    return state->get_return_var_val();
}

template void SL::Method::run<SolverProgramState>(SolverProgramState *state, vector<Param *> &input_params);

template<typename StateType>
void SL::Method::run(StateType *state, vector<Param *> &input_params)
{
    assert(var != nullptr);
    assert(body != nullptr);

    state->new_stack_frame(*params, input_params, meta_params);

    body->run(state);

    state->pop_stack_frame();
}

template<typename StateType>
void SL::Method::run(StateType *state, vector<SL::VarVal *> &input_params)  {
    assert(var != nullptr);
    assert(body != nullptr);

    state->new_stack_frame(*params, input_params, meta_params);

    body->run(state);

    state->pop_stack_frame();
}

void SL::Method::clear()
{
    //clear everything
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
    delete this;
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

template SL::VarVal* SL::Param::eval<ProgramState>(ProgramState *state);

template<typename StateType>
SL::VarVal* SL::Param::eval(StateType *state)  {
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
    //clears everything
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

string SL::Param::to_string() {
    switch (meta_type) {
        case is_var:
            return var->to_string();
            break;
        case is_expression:
            return expression->to_string();
            break;
        default:
            assert(false);
    }
    assert(false);

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
#ifdef USE_GENERIC_FILE
                assert(var_val_type == SL::generic_file_val_type);
#else
                assert(var_val_type == SL::file_val_type);
#endif
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
#ifdef USE_GENERIC_FILE
                assert(var_val_type == SL::string_val_type);
#else
                assert(var_val_type == SL::input_val_type);
#endif
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
                auto type_param = *type_params->at(0);
                auto var_param = *var_val->get_poly_vec(false)->get_type_params()->at(0);
                assert(type_param == var_param);
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


void SL::PolyVec::push_back(SL::VarVal* new_element) {
    if(new_element != nullptr) {
        assert(SL::var_val_invariant(get_type_params()->at(0), new_element));
        new_element->increment_shared_ptr();
    }
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
    for(int i = 0;i<size();i++)
    {
        if(at(i) != nullptr) {
            at(i)->decrement_shared_ptr();
        }
    }
    vector<SL::VarVal*>::clear();
    PolyType::soft_clear();
    delete this;
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

vector<bool> SL::PolyVec::to_vector_bool() {
    vector<bool> ret;
    for(int i = 0;i<size();i++) {
        ret.push_back(at(i)->get_bool());
    }
    return ret;
}

SL::PolyMap::PolyMap(SL::PolyMap* to_copy): PolyType(to_copy)
{
    for(auto it: *to_copy)
    {
        (*this)[it.first] = new VarVal(it.second);
    }
}

void SL::PolyMap::clear()
{
    for(auto it: *this)
    {
        if(it.second != nullptr) {
            it.second->decrement_shared_ptr();
        }
    }
    map<string, SL::VarVal*>::clear();
    PolyType::soft_clear();
    delete this;
}


template<>
map<string, string> SL::PolyMap::get_cpp_map<string>() {
    map<string, string> ret;
    for(auto it: *this) {
        ret[it.first] = it.second->get_string();
    }
    return ret;
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
    PolyType::soft_clear();
    delete this;
}

SL::PolyPair::PolyPair(SL::PolyPair *to_copy): PolyType(to_copy), pair<SL::VarVal*, SL::VarVal*>(
        new SL::VarVal(to_copy->first()), new SL::VarVal(to_copy->second())) {};

template<typename StateType>
SL::VarVal* SL::Expression::eval(StateType *state)
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
            assert(ret != nullptr);
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
    //clears all
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
            assert(var_val->get_num_shared_ptr() == 1);
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

string SL::Expression::to_string() {
    switch (expression_meta_type) {
        case binary_expr_meta_type:
            return "TODO binary_expr_meta_type in SL::Expression::to_string()";
            break;
        case func_call_meta_type:
            return function_call->to_string();
            break;
        case identifier_meta_type:
            return identifier->to_string();
            break;
        case var_val_meta_type:
            return var_val->to_string(false);
            break;
        case lambda_expr_meta_type:
            return "TODO lambda_expr_meta_type in SL::Expression::to_string()";
            break;
        case no_meta_type:
            assert(false);
            break;
    }
    assert(false);
}

SL::VarVal::VarVal(float _float_val) : float_val(_float_val) , var_val_type(float_val_type){}
SL::VarVal::VarVal(GenericFile* _generic_file) : generic_file(_generic_file) , var_val_type(generic_file_val_type){
#ifndef USE_GENERIC_FILE
    AssertDebug(false, "NOT USING GENERIC_FILE");
#endif
}
SL::VarVal::VarVal(File* _file) : file(_file) , var_val_type(file_val_type){
#ifdef USE_GENERIC_FILE
    AssertDebug(false, "USING ONLY GENERIC_FILE");
#endif
}
SL::VarVal::VarVal(Method* _method) : method(_method) , var_val_type(method_val_type){}
SL::VarVal::VarVal(SketchFunction* _harness) : skfunc(_harness) , var_val_type(skfunc_val_type){
    skfunc->increment_shared_ptr();
}
SL::VarVal::VarVal(PolyPair* _poly_pair) : poly_pair(_poly_pair) , var_val_type(poly_pair_type){}
SL::VarVal::VarVal(PolyVec* _poly_vec) : poly_vec(_poly_vec) , var_val_type(poly_vec_type){}
SL::VarVal::VarVal(PolyMap* _poly_map) : poly_map(_poly_map) , var_val_type(poly_map_type){}
SL::VarVal::VarVal(HoleVarStore * _solution) : solution(_solution), var_val_type(solution_val_type){}
SL::VarVal::VarVal(InputVarStore * _input_holder) : input_holder(_input_holder), var_val_type(input_val_type){}
SL::VarVal::VarVal(string  _s) : s(new Identifier(std::move(_s))), var_val_type(string_val_type) {}

SL::VarVal::VarVal(VarVal* _to_copy): var_val_type(_to_copy->var_val_type)
{
    switch (_to_copy->var_val_type) {
        case string_val_type:
            s = new Identifier(_to_copy->get_string(false));
            break;
        case int_val_type:
            i = _to_copy->get_int(false);
            break;
        case generic_file_val_type:
            generic_file = new GenericFile(_to_copy->get_generic_file(false));
            break;
        case method_val_type:
            method = new Method(_to_copy->get_method(false));
            break;
        case skfunc_val_type:
            AssertDebug(false, "not sure how to handle this cloning, better leave it for later. (whether or not to make an exact clone, renamed clone / rename holes.");
            skfunc = _to_copy->get_skfunc(false)->unit_clone();
            break;
        case solution_val_type:
//            solution = new SolverLanguagePrimitives::HoleAssignment(_to_copy->get_hole_var_store(false));
            break;
        case input_val_type:
            input_holder = new InputVarStore(*_to_copy->get_input_holder(false));
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
        default:
            AssertDebug(false, "TODO")
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
//    cout << "SET RETURN" << endl;
}

void SL::VarVal::complete_return()
{
    assert(is_return);
    num_shared_ptr--;
    assert(num_shared_ptr >= 0);
    is_return = false;
//    cout << "COMPLETE RETURN" << endl;
}

bool SL::VarVal::get_is_return() const {
    return is_return;
}

template<typename StateType>
SL::VarVal *SL::VarVal::eval(StateType *state, SL::FunctionCall *func_call) {
    switch (var_val_type) {
        case string_val_type:
            AssertDebug(false, "string has no methods (yet).");
            //do nothing
            break;
        case int_val_type:
            return eval<StateType, int>(i, state, func_call);
            //do nothing
            break;
        case generic_file_val_type:
            return eval<StateType, GenericFile*>(generic_file, state, func_call);
            break;
        case file_val_type:
            return eval<StateType, File*>(file, state, func_call);
            break;
        case method_val_type:
            AssertDebug(false, "method has no methods (yet).");
            //nothing to do yet
            break;
        case skfunc_val_type:
            return eval<StateType, SketchFunction*>(skfunc, state, func_call);
            break;
        case solution_val_type:
            return eval<StateType, HoleVarStore *>(solution, state, func_call);
            break;
        case input_val_type:
            AssertDebug(false, "string has no methods (yet).");
//            return eval<StateType, InputVarStore *>(input_holder, state, func_call);
            //nothing to do yet
            break;
        case bool_val_type:
            AssertDebug(false, "bool has no methods (yet).");
            //do nothing
            break;
        case void_val_type:
            AssertDebug(false, "void has no methods (yet).");
            //do nothing
            break;
        case no_type:
            assert(false);
            break;
        case float_val_type:
            AssertDebug(false, "float has no methods (yet).");
            //do nothing
            break;
        case poly_pair_type:
            return eval<StateType, PolyPair*>(poly_pair, state, func_call);
            break;
        case poly_vec_type:
            return eval<StateType, PolyVec*>(poly_vec, state, func_call);
            break;
        default:
            assert(false);
    }
    AssertDebug(false, "MISSING CASES OR WRONG INPUT.");
}

template<typename StateType, typename T>
SL::VarVal *SL::VarVal::eval(T& val, StateType *state, SL::FunctionCall *function_call)
{
    assert_type_invariant<T>();
    if constexpr(is_pointer<T>::value) {
        assert(val != nullptr);
    }
    else
    {
        static_assert(is_same<int, T>::value);
    }
    increment_shared_ptr();
    SL::VarVal* ret = function_call->eval(val, state, this);
    decrement_shared_ptr();
    return ret;
}

bool SL::VarVal::is_input_holder()  const{
    return var_val_type == input_val_type;
}

bool SL::VarVal::is_string()  const{
    return var_val_type == string_val_type;
}

bool SL::VarVal::is_solution_holder()  const{
    return var_val_type == solution_val_type;
}

void SL::VarVal::clear_assert_0_shared_ptrs() {
    assert(num_shared_ptr == 0);
    _clear();
}

template void SL::VarVal::clear<SketchFunction*>(SketchFunction* &val, bool do_delete);
template void SL::VarVal::clear<File*>(File* &val, bool do_delete);
template void SL::VarVal::clear<GenericFile*>(GenericFile* &val, bool do_delete);
template void SL::VarVal::clear<SL::PolyMap*>(SL::PolyMap* &val, bool do_delete);
template void SL::VarVal::clear<SL::PolyPair*>(SL::PolyPair* &val, bool do_delete);
template void SL::VarVal::clear<SL::PolyVec*>(SL::PolyVec* &val, bool do_delete);
template void SL::VarVal::clear<SL::Identifier*>(SL::Identifier* &val, bool do_delete);
template void SL::VarVal::clear<SL::Method*>(SL::Method* &val, bool do_delete);
template void SL::VarVal::clear<HoleVarStore*>(HoleVarStore* &val, bool do_delete);
template void SL::VarVal::clear<InputVarStore*>(InputVarStore* &val, bool do_delete);

template<typename T>
void SL::VarVal::clear(T &val, bool do_delete) {
    assert_type_invariant<T>();
    if(val != nullptr) {
        val->clear();
        if(do_delete) {
            delete val;
        }
        val = nullptr;
    }
}

bool SL::VarVal::operator<(const SL::VarVal &other) const
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
            return false;
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

string SL::VarVal::to_string(bool do_count, bool do_assert)
{
    switch (var_val_type) {

        case string_val_type:
            return "\"" + get_string(do_count, do_assert) + "\"";
            break;
        case int_val_type:
            return std::to_string(get_int(do_count, do_assert));
            break;
        case file_val_type:
            return file->to_string();
            assert(false);
            break;
        case method_val_type:
            assert(false);
            break;
        case skfunc_val_type:
            return skfunc->to_string();
            assert(false);
            break;
        case solution_val_type:
            assert(false);
            break;
        case input_val_type:
            return input_holder->to_string();
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
            return poly_vec->to_string();
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

SL::VarVal *SL::VarVal::plus_op(SL::VarVal *other)
{
    assert(var_val_type == other->var_val_type);
    switch (var_val_type) {
        case int_val_type:
            return new VarVal((int)(get_int(true, false) + other->get_int(true, false)));
            break;
        case float_val_type:
            return new VarVal((float)(get_float(true, false) + other->get_float(true, false)));
            break;
        case poly_vec_type: {
            PolyVec *new_poly_vec =
                    new SL::PolyVec(
                            new SL::PolyType("any"),
                            get_poly_vec_const(false)->size() + other->get_poly_vec_const(false)->size());
            for(int i = 0;i<get_poly_vec_const(false)->size();i++) {
                new_poly_vec->set(i, get_poly_vec_const(false)->at(i));
            }
            for(int i = 0;i<other->get_poly_vec_const(false)->size();i++) {
                new_poly_vec->set(i+get_poly_vec_const(false)->size(), other->get_poly_vec_const(false)->at(i));
            }
            return new VarVal(new_poly_vec);
            break;
        }

        default:
            assert(false);
    }
    assert(false);
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


template<typename StateType>
void SL::UnitLine::run(StateType *state) {
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
    //clears everything
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

template<typename StateType>
SL::VarVal *SL::BinaryExpression::eval(StateType *state)
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

SL::SLType::SLType(SL::Identifier *_name, SL::TypeParams *_type_params) : name(_name),
type_params(new PolyType(_type_params)){}

SL::SLType::SLType(SL::SLType *to_copy) : name(new Identifier(to_copy->name)) {
    if(to_copy->type_params != nullptr) {
        type_params = new PolyType(to_copy->type_params);
    }
}

void SL::SLType::clear()
{
    //clears everything
    assert(name->is_defined()) ;
    name->clear();
    name = nullptr;
    if(type_params != nullptr) {
        type_params->clear();
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

template
void SL::CodeBlock::run<FMTL::FunctionMapTransformerState>(FMTL::FunctionMapTransformerState *state);

template<typename StateType>
void SL::CodeBlock::run(StateType *state)  {
    assert(head != nullptr);
    CodeBlock* at = this;
    state->open_subframe();
    int unit_count = 0;
    while(at != nullptr)
    {
        at->head->run(state);
        if(state->has_return())
        {
            break;
        }
        at = at->rest;
        unit_count++;
    }
    state->close_subframe();
}

SL::LambdaExpression::LambdaExpression(SL::LambdaExpression *to_copy) :
code_block(new SL::CodeBlock(to_copy->code_block)), params(copy_params(to_copy->params)), meta_params(copy_params(to_copy->meta_params)) {}

template<typename StateType>
SL::VarVal *SL::LambdaExpression::eval(StateType *state) {
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
