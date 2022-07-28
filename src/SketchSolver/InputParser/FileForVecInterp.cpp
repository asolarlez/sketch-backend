//
// Created by Kliment Serafimov on 7/27/22.
//
#include "FileForVecInterp.h"
#include "File.h"
namespace VectorizedInterpreter {
    FileForVecInterp::FileForVecInterp(const File *file) :
    vector<vector<const Var *> >(file->size(), vector<const Var *>(file->num_inputs_per_row(), nullptr))
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

//    FileForVecInterp::FileForVecInterp(const FileForVecInterp* file, int slice_start, int slice_width) :
//            vector<vector<const Var *> >(slice_width, vector<const Var *>(file->num_inputs_per_row(), nullptr)) {
//        for (int i = slice_start; i < min((int)file->size(), slice_start+slice_width); i++) {
//            operator[](i) = file->at(i);
//        }
//    }
}