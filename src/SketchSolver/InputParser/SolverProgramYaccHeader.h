//
// Created by kliment on 12/18/21.
//

#ifndef SKETCH_SOURCE_SOLVERPROGRAMYACCHEADER_H
#define SKETCH_SOURCE_SOLVERPROGRAMYACCHEADER_H

#include "SolverLanguageLexAndYaccHeader.h"
#include "string"

class Harness;
class FloatManager;
class CommandLineArgs;
class HoleHardcoder;

class SolverProgramState
{
public:
    Harness* harness{};
    const string& file_name;
    FloatManager& floats;
    CommandLineArgs& args;
    HoleHardcoder& hc;
    bool hasGoodEnoughSolution;
    SolverProgramState(Harness* _harness, const string& _file_name, FloatManager& _floats, CommandLineArgs& _args,
                       HoleHardcoder& _hc, bool _hasGoodEnoughSolution):
            harness(_harness), file_name(_file_name), floats(_floats), args(_args), hc(_hc), hasGoodEnoughSolution(_hasGoodEnoughSolution)
    {

    }

};


void run_solver_langauge_program(SolverProgramState* _state);


#endif //SKETCH_SOURCE_SOLVERPROGRAMYACCHEADER_H
