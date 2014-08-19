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

void runDriver() {
	PyDriver m(*PARAMS);
	m.parseInput();
}

InterpreterEnvironment* getEnvt() {
	return INp::envt;
}

void
printStats ()
{
    statistics::MemoryStatistics ms = mem.getMemStats ();

    cout << endl << "----- Statistics -----" << endl
         << "Total elapsed time (ms):  " << totalElapsed.get_tot_ms () << endl
         << "Model building time (ms): " << modelBuilding.get_tot_ms () << endl
         << "Solution time (ms):       " << solution.get_tot_ms () << endl
        // we might get more memory stats, but these are most interesting
         << "Max virtual mem (bytes):  " << ms.total << endl
         << "Max resident mem (bytes): " << ms.resident << endl
         << "Max private mem (bytes):  " << ms.total - ms.shared << endl
        ;
}



void DumpSignalHandler(int signal){
	cerr<<"DUMP OUTPUT!!!."<<endl;
	cout<<"DUMP OUTPUT!!!."<<endl;
	flush(cout);
	INp::envt->printControls("");
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
