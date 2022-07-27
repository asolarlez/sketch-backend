//
// Created by Kliment Serafimov on 7/27/22.
//
#include "FileForVectorizedInterpreter.h"
#include "File.h"
namespace VectorizedInterpreter {
    FileForVectorizedInterpreter::FileForVectorizedInterpreter(const File *file) :
    vector<vector<Var *> >(file->size(), vector<Var *>(file->num_inputs_per_row(), nullptr))
    {
        for (int i = 0; i < file->size(); i++) {
            LightVarStore &inputs = *file->at(i);
            for (int j = 0; j < inputs.size(); j++) {
                auto obj = inputs.getObjConst(j);
                if(!obj.get_is_array()) {
                    at(i)[j] = new IntVar(obj.getInt());
                }
                else
                {
                    at(i)[j] = new IntArrVar(obj.get_array_as_supertype()->get_vector_of_ints());
                }
            }
        }
    }
}