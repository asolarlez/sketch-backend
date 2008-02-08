#include "driver.h"

CommandLineArgs* PARAMS;

int main(int argc, char** argv){
  ABCSolverStart();
  
  CommandLineArgs params(argc, argv);
  PARAMS = &params;
  Driver m(params);
  m.parseInput();
  return m.resolveSketches();
}


