//
// Created by kliment on 12/18/21.
//

#ifndef SKETCH_SOURCE_SOLVERLANGUAGEYACCHEADER_H
#define SKETCH_SOURCE_SOLVERLANGUAGEYACCHEADER_H

#include <utility>

#include "string"
#include "SolverLanguageLexAndYaccHeader.h"
#include "SketchFunction.h"

class HyperSketchState: public ProgramState
{
public:
    FloatManager& floats;
    CommandLineArgs& args;
    HoleHardcoder& hc;

    const string& file_name;

    ofstream console_output = ofstream("hypersketch_console.out");

    SL::Methods* init_root = nullptr;

    std::chrono::steady_clock::time_point start_of_run;
    std::chrono::steady_clock::time_point prev_timestep;

    void clear(bool conscious_call = true) override
    {
        console_output.close();
        init_root->clear();
        ProgramState::clear(true);
    }

    HyperSketchState(FunctionMap& _function_map, const string& _file_name, FloatManager& _floats, CommandLineArgs& _args,
                       HoleHardcoder& _hc, bool _hasGoodEnoughSolution):
            ProgramState(_function_map), file_name(_file_name),
            floats(_floats), args(_args), hc(_hc) {}

    void add_root(SL::Methods* _init_root) {
        init_root = _init_root;
    }

    SL::VarVal * eval();

};


void parse_solver_langauge_program(HyperSketchState* _state, string hypersketch_file);


#endif //SKETCH_SOURCE_SOLVERLANGUAGEYACCHEADER_H
