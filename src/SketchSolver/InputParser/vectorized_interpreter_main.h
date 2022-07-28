//
// Created by Kliment Serafimov on 7/27/22.
//

#ifndef SKETCH_VECTORIZED_INTERPRETER_MAIN_H
#define SKETCH_VECTORIZED_INTERPRETER_MAIN_H

#include "FileForVecInterp.h"

namespace VectorizedInterpreter {

    vector<BaseType> main(string dag_string, const VectorizedInterpreter::FileForVecInterp& file_for_vecinterp);

    int main_get_passing_input_idx(
            string dag_string,
            const VectorizedInterpreter::FileForVecInterp &file_for_vecinterp);
}

#endif //SKETCH_VECTORIZED_INTERPRETER_MAIN_H
