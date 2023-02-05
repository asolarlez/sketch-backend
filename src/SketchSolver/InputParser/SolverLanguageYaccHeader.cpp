//
// Created by kliment on 12/20/21.
//

#include "SolverLanguageYaccHeader.h"
#include "SolverLanguage.h"
#include "SolverLanguageLexAndYaccHeader.h"

#include "BenchmarkScore.h"

SL::VarVal * HyperSketchState::eval(){
    assert(init_root != nullptr);

    start_of_run = std::chrono::steady_clock::now();
    prev_timestep = std::chrono::steady_clock::now();

    init_root->populate_state(global);

    global.add_var_and_set_var_val_and_clear_var(new SL::Var(new SL::Identifier("bool"), new SL::Identifier("true")),
                                                 new SL::VarVal(true));
    global.add_var_and_set_var_val_and_clear_var(new SL::Var(new SL::Identifier("bool"), new SL::Identifier("false")),
                                                 new SL::VarVal(false));
    global.add_var_and_set_var_val_and_clear_var(new SL::Var(new SL::Identifier("namespace"), new SL::Identifier("global")),
                                                 nullptr);
    global.add_var_and_set_var_val_and_clear_var(new SL::Var(new SL::Identifier("int"), new SL::Identifier("seed")),
                                                 new SL::VarVal(args.seed));
    global.add_var_and_set_var_val_and_clear_var(new SL::Var(new SL::Identifier("string"), new SL::Identifier("file_name")),
                                                 new SL::VarVal(file_name));

    SL::Var* init_f = new SL::Var(new SL::Identifier("Solution"), new SL::Identifier("main"));
    assert(*global.name_to_var(init_f->get_name()) == *init_f);
    auto input_params = vector<SL::Param*>();

    assert(!function_map.empty());
    for(const auto& it: function_map){
        global.add_var_and_set_var_val_and_clear_var(
                new SL::Var(new SL::Identifier("SketchFunction"),
                            new SL::Identifier(it.first)),
                new SL::VarVal(function_map[it.first]));
    }

    new_stack_frame();
    assert(frames.size() == 1);

    SL::Method* to_run = global.get_var_val(init_f)->get_method();

    to_run->run(this, input_params);

    init_f->clear();

    SL::VarVal* var_val_ret = get_return_var_val();

    assert(var_val_ret != nullptr);
    var_val_ret->increment_shared_ptr();

    assert(frames.size() == 1);
    pop_stack_frame();
    assert(frames.empty());

    global.clear(true);

    auto end_eval = timestamp(start_of_run, "hypersketch::eval");

    auto elapsed = chrono::duration_cast<chrono::microseconds>(end_eval - start_of_run).count();
    console_output << "BENCH[HyperSketchState::eval()]: " << elapsed/1000 << " (s) ~ " << elapsed << " (us)" << endl;

    return var_val_ret;
}

void ProgramState::new_stack_frame(vector<SL::Param *> &vars, vector<SL::Param *> &vals, vector<SL::Param*>* meta_vars) {
    assert(vars.size() == vals.size());

    vector<SL::VarVal* > var_vals;
    var_vals.reserve(vals.size());
    for(int i = 0;i<vals.size();i++){
        var_vals.emplace_back(vals[i]->eval(this));
    }

    new_stack_frame(vars, var_vals, meta_vars);
}

void ProgramState::new_stack_frame(vector<SL::Param *> &vars, vector<SL::VarVal *> &var_vals, vector<SL::Param*>* meta_vars) {
    assert(vars.size() == var_vals.size());

    vector<SL::VarVal* > meta_var_vals;
    if(meta_vars != nullptr)
    {
        for(int i = 0;i<meta_vars->size();i++)
        {
            SL::Var* the_var = meta_vars->at(i)->get_var();
            meta_var_vals.push_back(this->get_var_val(the_var));
        }
    }

    new_stack_frame();

    assert(vars.size() == var_vals.size());
    for(int i = 0;i < vars.size();i++)
    {
        add_and_set_var_val(vars[i]->get_var(), var_vals[i]);
    }

    if(meta_vars != nullptr) {
        assert(meta_var_vals.size() == meta_vars->size());
        for (int i = 0; i < meta_vars->size();i++)
        {
            add_and_set_var_val(meta_vars->at(i)->get_var(), meta_var_vals[i]);
        }
    }
}

void ProgramState::set_var_val(SL::Var *var, SL::VarVal *var_val) {
    assert(!frames.empty());
    assert(var_val_invariant(var, var_val));
    try {
        frames.rbegin()->set_var_val_throws(var, var_val);
    }
    catch (exception& e)
    {
        string var_type_str = var->get_type()->to_string();
        if(var_type_str == "SketchFunction") {
            global.set_var_val(var, var_val);
            string var_name = var->get_name()->to_string();
            SketchFunction* skfunc = var_val->get_skfunc(false);
            FunctionMap& _function_map = skfunc->get_env()->function_map;
            assert(&_function_map == &function_map);
            auto new_primitive = function_map.insert(var_name, skfunc);
            skfunc->set_rep(new_primitive);
        }
        else
        {
//            //...
//            else if(var_type_str == "int") {
//                assert(var->get_name()->to_string() == "seed");
//                global.set_var_val(var, var_val);
//
//                args.seed = var_val->get_int(false);
//            }
//            else
//            {
//                assert(false);
//            }
            AssertDebug(false, "METHOD NOT IMPLEMENTED IN DERIVED CLASS.");
        }
    }
}


void ProgramState::add_to_function_map(const string &skfunc_name, SketchFunction *skfunc) {
    assert(skfunc->get_dag()->getNodesByType(bool_node::CTRL).size() >= 0);
    FunctionMap& _function_map = skfunc->get_env()->function_map;
    assert(&_function_map == &function_map);
    if(function_map.find(skfunc_name) == function_map.end()) {
        auto new_or_existing_primitive= function_map.insert(skfunc_name, skfunc);
        skfunc->set_rep(new_or_existing_primitive);
    }
    else {
        assert(function_map.find(skfunc_name) != function_map.end());
        assert(function_map[skfunc_name] == skfunc);
    }
}

ProgramState::ProgramState(ProgramEnvironment *program_environment):
        function_map(program_environment->function_map), global() {
    assert(program_environment != nullptr);
}

