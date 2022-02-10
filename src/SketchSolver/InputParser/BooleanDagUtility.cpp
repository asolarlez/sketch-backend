//
// Created by kliment on 2/9/22.
//

#include "BooleanDagUtility.h"

void BooleanDagUtility::swap_env(ProgramEnvironment *new_env) {
    assert(original_program_env == nullptr);
    original_program_env = env;
    assert(new_env != env);
    env = new_env;
}

void BooleanDagUtility::reset_env_to_original() {
    assert(original_program_env != nullptr);
    env = original_program_env;
    original_program_env = nullptr;
}

bool BooleanDagUtility::soft_clear_assert_num_shared_ptr_is_0()
{
    assert(shared_ptr == 0);
    int prev_num = BooleanDAG::get_allocated().size();
    assert(root_dag != nullptr);
    root_dag->clear();
    assert(prev_num - 1 == BooleanDAG::get_allocated().size());
//    root_dag = nullptr;
    return true;
}

bool BooleanDagUtility::has_it_been_at_least_inlined() {
    return at_least_inlined;
}
