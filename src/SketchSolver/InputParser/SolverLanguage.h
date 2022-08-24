//
// Created by kliment on 6/22/21.
//

#ifndef SKETCH_SOURCE_SOLVERLANGUAGE_H
#define SKETCH_SOURCE_SOLVERLANGUAGE_H

#include <string>
#include <map>
using namespace std;
class FunctionMap;
class FloatManager;
class CommandLineArgs;
class HoleHardcoder;

class SolverLanguage {
public:
    SolverLanguage() = default;
    map<string, string> eval(string hypersketch_file_path, FunctionMap &function_map, const string &file_name,
                             FloatManager &floats, CommandLineArgs &_args, HoleHardcoder &_hc);
};

#endif //SKETCH_SOURCE_SOLVERLANGUAGE_H
