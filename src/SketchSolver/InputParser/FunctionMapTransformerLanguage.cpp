//
// Created by kliment on 3/20/22.
//

#include "FunctionMapTransformerLanguage.h"
#include "ProgramEnvironment.h"


FMTL::FunctionMapTransformerState::FunctionMapTransformerState(FunctionMap &_source_function_map) :
    ProgramState(_source_function_map.get_env()->shallow_copy_w_new_blank_function_map()),
    source_function_map(_source_function_map),
    env(ProgramState::function_map.get_env()){}

SketchFunction *FMTL::FunctionMapTransformerState::get_source_skfunc(string name) {
    assert(source_function_map.find(name) != source_function_map.end());
    return source_function_map[name];
}

ProgramEnvironment *FMTL::FunctionMapTransformerState::get_env() {
    return env;
}

map<SketchFunction *, SketchFunction *> *FMTL::FunctionMapTransformerState::get_meta_map_dp() {
    return meta_map_dp;
}
