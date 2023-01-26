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
        assert(inlining_tree->get_dag_id() == get_dag_id());
    }
    return ret;
}

vector<string> BooleanDagUtility::get_deep_holes() const{
    vector<string> ret;
    set<string>* subf_names = get_inlined_functions();

    if(get_has_been_inlined()) {
//        assert(subf_names->empty());
        for (auto it: get_dag()->getNodesByType(bool_node::CTRL)) {
            if (it->get_name() != "#PC") {
                ret.push_back(it->get_name());
            }
        }
    }
    else {
        if(!subf_names->empty()) {
            assert(get_env()->function_map.find(get_dag_name()) != get_env()->function_map.end());
            assert(get_env()->function_map.find(get_dag_name())->second == this);
            subf_names->insert(get_dag_name());
            for (const auto &f_name: *subf_names) {
                if(get_env()->function_map.find(f_name) != get_env()->function_map.end()) {
                    assert(get_env()->function_map.find(f_name) != get_env()->function_map.end());
                    SketchFunction *subf = get_env()->function_map.find(f_name)->second;

                    for (auto it: subf->get_dag()->getNodesByType(bool_node::CTRL)) {
                        if (it->get_name() != "#PC") {
                            ret.push_back(it->get_name());
                        }
                    }
                }
            }
        }
    }
    return ret;
}

const LightInliningTree * BooleanDagUtility::get_inlining_tree(bool assert_nonnull) const {
    return get_inlining_tree_non_const(assert_nonnull);
}

LightInliningTree * BooleanDagUtility::get_inlining_tree_non_const(bool assert_nonnull) const {
    if(assert_nonnull) {
        assert(inlining_tree != nullptr);
    }
    if(inlining_tree != nullptr) {
        assert(inlining_tree->get_dag_id() == get_dag_id());
    }
    return inlining_tree;
}


bool BooleanDagUtility::get_has_been_inlined() const {
    return inlining_tree != nullptr;
}

void BooleanDagUtility::clear_inlining_tree() {
    assert(inlining_tree != nullptr);
    inlining_tree->clear();
    inlining_tree = nullptr;
}
