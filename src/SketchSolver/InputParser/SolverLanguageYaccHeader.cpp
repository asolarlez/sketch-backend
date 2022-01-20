//
// Created by kliment on 12/20/21.
//

#include "SolverLanguageYaccHeader.h"
#include "SolverLanguage.h"

SolverLanguagePrimitives::SolutionHolder* SolverProgramState::eval(){
    assert(init_root != nullptr);

    init_root->populate_state(global);

    global.add_var_and_set_var_val_and_clear_var(new SL::Var(new SL::Identifier("bool"), new SL::Identifier("true")),
                                                 new SL::VarVal(true));
    global.add_var_and_set_var_val_and_clear_var(new SL::Var(new SL::Identifier("bool"), new SL::Identifier("false")),
                                                 new SL::VarVal(false));
    global.add_var_and_set_var_val_and_clear_var(new SL::Var(new SL::Identifier("namespace"), new SL::Identifier("global")),
                                                 nullptr);
    global.add_var_and_set_var_val_and_clear_var(new SL::Var(new SL::Identifier("int"), new SL::Identifier("seed")),
                                                 new SL::VarVal(args.seed));

    SL::Var* init_f = new SL::Var(new SL::Identifier("Solution"), new SL::Identifier("main"));
    assert(*global.name_to_var(init_f->get_name()) == *init_f);
    auto input_params = vector<SL::Param*>();



    if(harness_ != nullptr)
    {
        assert(function_map.empty());
        global.set_var_val(new SL::Var(new SL::Identifier("SketchFunction"), new SL::Identifier("harness")),
                           new SL::VarVal(harness_));

        input_params.emplace_back(new SL::Param(new SL::Expression(new SL::VarVal(harness_))));

    }
    else
    {
        assert(!function_map.empty());
        for(const auto& it: function_map){
            global.add_var_and_set_var_val_and_clear_var(
                    new SL::Var(new SL::Identifier("SketchFunction"),
                                new SL::Identifier(it.first)),
                    new SL::VarVal(function_map[it.first]));
        }
    }

    new_stack_frame();
    assert(frames.size() == 1);

    SL::Method* to_run = global.get_var_val(init_f)->get_method();

    to_run->run(this, input_params);

    init_f->clear();

    SL::VarVal* var_val_ret = get_return_var_val();
    assert(var_val_ret != nullptr);
    SolverLanguagePrimitives::SolutionHolder* ret = var_val_ret->get_solution(false);

    delete var_val_ret;

    assert(frames.size() == 1);
    pop_stack_frame();

    assert(frames.empty());

    global.clear(true);

    return ret;
}

void SolverProgramState::new_stack_frame(vector<SL::Param *> &vars, vector<SL::Param *> &vals) {
    assert(vars.size() == vals.size());

    vector<SL::VarVal* > var_vals;
    var_vals.reserve(vals.size());
    for(int i = 0;i<vals.size();i++)
    {
        var_vals.emplace_back(vals[i]->eval(this));
    }

    new_stack_frame();

    for(int i = 0;i < vars.size();i++)
    {
        add_and_set_var_val(vars[i]->get_var(), var_vals[i]);
    }
}

void SolverProgramState::open_subframe() {
    assert(!frames.empty());
    frames.rbegin()->open_subframe();
}

void SolverProgramState::close_subframe() {
    assert(!frames.empty());
    frames.rbegin()->close_subframe();
}

void SolverProgramState::add_to_function_map(const string &sk_func_name, SketchFunction *sk_func) {
    assert(sk_func->get_dag()->getNodesByType(bool_node::CTRL).size() >= 0);
    if(function_map.find(sk_func_name) == function_map.end()) {
        function_map[sk_func_name] = sk_func;
        sk_func->in_function_map(&function_map);

        map<string, BooleanDAG*>& functionMap = sk_func->get_env()->functionMap;
        assert(functionMap.find(sk_func_name) == functionMap.end());
        functionMap[sk_func_name] = sk_func->get_dag();
    }
    else {
        assert(function_map[sk_func_name] == sk_func);

        map<string, BooleanDAG*>& functionMap = sk_func->get_env()->functionMap;
        assert(functionMap.find(sk_func_name) != functionMap.end());
        assert(functionMap[sk_func_name] == sk_func->get_dag());
    }
}
