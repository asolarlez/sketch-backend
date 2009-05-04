#include "InputReader.h"
#include "BooleanDAG.h"
#include "BooleanDAGCreator.h"
#include "InterpreterEnvironment.h"

#define YYERROR_VERBOSE 1

#include <string>
#include <sstream>
#include <stack>
#include <vector>
#include <list>

using namespace std;

namespace INp{

int isatty(int i);
  InterpreterEnvironment* envt;
  int global_filterid = 0;
  int NCTRLS=5;
  bool overrideNCtrls=false;
  int NINPUTS=5;
  bool overrideInputs=false;
#include "InputLexer.cpp"
#include "InputParser.cpp"

}

