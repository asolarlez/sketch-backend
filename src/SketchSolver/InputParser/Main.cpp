#include "driver.h"



int main(int argc, char** argv){
  ABCSolverStart();
  
  CommandLineArgs params(argc, argv);
  
  Driver m(params);
  m.parseInput();
  return m.resolveSketches();
}


