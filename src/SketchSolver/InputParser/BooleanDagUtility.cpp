//
// Created by kliment on 2/9/22.
//

#include "BooleanDagUtility.h"
#include "SketchFunction.h"

void BooleanDagUtility::swap_env(ProgramEnvironment *new_env) {
    assert(original_program_env == nullptr);
    original_program_env = get_env();
    assert(new_env != get_env());
    get_env_ref() = new_env;
}

void BooleanDagUtility::reset_env_to_original() {
    assert(original_program_env != nullptr);
    get_env_ref() = original_program_env;
    original_program_env = nullptr;
}


bool BooleanDagUtility::is_inlining_tree_nonnull() {
    bool ret = inlining_tree != nullptr;
    if(inlining_tree != nullptr) {
        assert(inlining_tree->get_skfunc() == this);
    }
    return ret;
}

InliningTree *& BooleanDagUtility::get_inlining_tree(bool assert_nonnull) {
    if(assert_nonnull) {
        assert(inlining_tree != nullptr);
    }
    if(inlining_tree != nullptr) {
        assert(inlining_tree->get_skfunc() == this);
    }
    return inlining_tree;
}

bool BooleanDagUtility::get_has_been_concretized() {
    return has_been_concretized;
}

void BooleanDagUtility::clear_inlining_tree() {
    assert(inlining_tree != nullptr);
    inlining_tree->clear();
    inlining_tree = nullptr;
}
