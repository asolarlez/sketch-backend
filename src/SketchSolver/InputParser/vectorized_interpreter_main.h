//
// Created by Kliment Serafimov on 7/27/22.
//

#ifndef SKETCH_VECTORIZED_INTERPRETER_MAIN_H
#define SKETCH_VECTORIZED_INTERPRETER_MAIN_H

#include "FileForVectorizedInterpreter.h"

namespace VectorizedInterpreter {

    vector<BaseType> main(string dag_string, const VectorizedInterpreter::FileForVectorizedInterpreter& file_from_vectorized_intepreter);
}

#endif //SKETCH_VECTORIZED_INTERPRETER_MAIN_H
