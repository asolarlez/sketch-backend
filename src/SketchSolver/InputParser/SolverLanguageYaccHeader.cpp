//
// Created by kliment on 12/20/21.
//

#include "SolverLanguageYaccHeader.h"
#include "SolverLanguage.h"

SolverLanguagePrimitives::SolutionHolder* SolverProgramState::eval(){
    assert(init_root != nullptr);

    init_root->populate_state(global);

    global.set_var_val(new SL::Var(new SL::Name("Harness"), new SL::Name("harness")), new SL::VarVal(harness_));
    global.set_var_val(new SL::Var(new SL::Name("string"), new SL::Name("file_name")), new SL::VarVal(file_name));
    global.set_var_val(new SL::Var(new SL::Name("bool"), new SL::Name("true")), new SL::VarVal(true));
    global.set_var_val(new SL::Var(new SL::Name("namespace"), new SL::Name("global")), new SL::VarVal(true));

    SL::Var* init_f = new SL::Var(new SL::Name("Solution"), new SL::Name("main"));
    assert(*global.name_to_var(init_f->get_name()) == *init_f);
    auto input_params = vector<SL::Param*>();

    input_params.emplace_back(new SL::Param(new SL::Var(new SL::Name("Harness"), new SL::Name("harness"))));
    input_params.emplace_back(new SL::Param(new SL::Var(new SL::Name("string"), new SL::Name("file_name"))));

    new_stack_frame();
    assert(frames.size() == 1);
    global.get_var_val(init_f)->get_method()->run(this, input_params);

    SolverLanguagePrimitives::SolutionHolder* ret = get_return_var_val()->get_solution();

    ret = new SolverLanguagePrimitives::SolutionHolder(ret);

    assert(frames.size() == 1);
    pop_stack_frame();

    return ret;
}