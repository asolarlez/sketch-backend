//
// Created by kliment on 12/18/21.
//

#ifndef SKETCH_SOURCE_SOLVERLANGUAGEYACCHEADER_H
#define SKETCH_SOURCE_SOLVERLANGUAGEYACCHEADER_H

#include "string"
#include "SolverLanguageLexAndYaccHeader.h"

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

    Methods* init_root = nullptr;
    void add_root(Methods* _init_root)
    {
        init_root = _init_root;
        cout << "HERE!!!" << endl;
    }

    map<string, Method*> methods;

    void run()
    {
        assert(init_root != nullptr);

        init_root->populate_state(methods);

        for(auto it : methods)
        {
            cout << it.first << " "<< it.second << endl;
        }
    }
};


void run_solver_langauge_program(SolverProgramState* _state);


#endif //SKETCH_SOURCE_SOLVERLANGUAGEYACCHEADER_H
