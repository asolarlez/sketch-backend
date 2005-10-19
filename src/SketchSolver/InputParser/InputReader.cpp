#include "Pipeline.h"
#include "SplitJoin.h"
#include "MatrixFilter.h"
#include "TableFilter.h"
#include "InputReader.h"
#include "BooleanDAG.h"
#include "NativeFilter.h"
#include "SParameter.h"

#define YYERROR_VERBOSE 1

#include <string>
#include <sstream>

#include <list>

using namespace std;

namespace INp{

int isatty(int i);

  map<string, BooleanDAG*> functionMap;
  int global_filterid = 0;

#include "InputLexer.c"
#include "InputParser.c"

}

