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
namespace SL {
    class SketchFunction;
}
using namespace SL;

class SolverLanguage {
public:
    SolverLanguage() = default;
    map<string, string> eval(const string& harness_name,  const string &hypersketch_file_path,
                             const string& file_name, FunctionMap &function_map,
                             FloatManager &floats, CommandLineArgs &_args, HoleHardcoder &_hc);
};

#endif //SKETCH_SOURCE_SOLVERLANGUAGE_H
