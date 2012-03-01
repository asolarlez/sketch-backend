#include <iostream>
#include <cassert>
#include "CegisMainHelpers.h"
#include "driver.h"
#include "memory_sampler.h"
#include "timerclass.h"

#include <signal.h>

using namespace std;

//using namespace statistics;

//CommandLineArgs* PARAMS;

// Statistics -- could print these periodically
extern timerclass totalElapsed;
extern timerclass modelBuilding;
extern timerclass solution;
extern statistics::MemorySampler mem;

int
main(int argc, char** argv)
{
    totalElapsed.start ();
    mem.run ();
	
	int rv = 0;

	try{

	signal(SIGABRT, AssertionHandler);
	signal(SIGINT, CtrlCHandler);
	signal(SIGTERM, TerminationHandler);	

	CommandLineArgs params(argc, argv);

	if(params.synthtype == SATSolver::ABC || params.veriftype == SATSolver::ABC){
	   cout<<"This makes no sense"<<endl;
       ABCSolverStart();
	}
  
         // too coarse of a timing?
    
    PARAMS = &params;
    Driver m(params);
    m.parseInput();
    

    
    //rv = m.resolveSketches();
    

	}catch(...){
		cerr<<"Unusual Termination."<<endl;
		rv = 3;
	}
    mem.stop ();
    totalElapsed.stop ();

    printStats ();
	1;
    return rv;
}


