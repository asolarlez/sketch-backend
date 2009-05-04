#include <iostream>
#include <cassert>
#include "driver.h"
#include "memory_sampler.h"
#include "timerclass.h"

#include <signal.h>

using namespace std;

//using namespace statistics;

CommandLineArgs* PARAMS;

// Statistics -- could print these periodically
timerclass totalElapsed;
timerclass modelBuilding;
timerclass solution;
statistics::MemorySampler mem;

void
printStats ()
{
    statistics::MemoryStatistics ms = mem.getMemStats ();

    cout << endl << "----- Statistics -----" << endl
         << "Total elapsed time (ms):  " << totalElapsed.get_cur_ms () << endl
         << "Model building time (ms): " << modelBuilding.get_cur_ms () << endl
         << "Solution time (ms):       " << solution.get_cur_ms () << endl
        // we might get more memory stats, but these are most interesting
         << "Max virtual mem (bytes):  " << ms.total << endl
         << "Max resident mem (bytes): " << ms.resident << endl
         << "Max private mem (bytes):  " << ms.total - ms.shared << endl
        ;
}


void AssertionHandler(int signal){
	cerr<<"Someone threw a C-style assertion."<<endl;
	mem.stop ();
    totalElapsed.stop ();
    printStats ();
	exit(3);
}

void CtrlCHandler(int signal){
	cerr<<"I've been killed."<<endl;
	mem.stop ();
    totalElapsed.stop ();
    printStats ();
	exit(3);
}

void TerminationHandler(int signal){
	cerr<<"I've been terminated."<<endl;
	mem.stop ();
    totalElapsed.stop ();
    printStats ();
	exit(3);
}


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


    ABCSolverStart();
  
    modelBuilding.start ();     // too coarse of a timing?
    CommandLineArgs params(argc, argv);
    PARAMS = &params;
    Driver m(params);
    m.parseInput();
    modelBuilding.stop ();

    solution.start ();
    //rv = m.resolveSketches();
    solution.stop ();

	}catch(...){
		cerr<<"Unusual Termination."<<endl;
		rv = 3;
	}
    mem.stop ();
    totalElapsed.stop ();

    printStats ();

    return rv;
}


