// #include "config.h"
#include <iostream>
#include <cassert>
#include "CegisMainHelpers.h"
#include "driver.h"
#include "memory_sampler.h"
#include "timerclass.h"

#include <signal.h>

#ifdef HAVE_SYS_RESOURCE_H
#include <sys/resource.h>
#endif

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

        signal(16, DumpSignalHandler);
        signal(SIGABRT, AssertionHandler);
        signal(SIGINT, CtrlCHandler);
        signal(SIGTERM, TerminationHandler);

        CommandLineArgs params(argc, argv);

#ifdef HAVE_SYS_RESOURCE_H
        if (params.memLimit > 0) {
            struct rlimit rl;
            getrlimit(RLIMIT_AS, &rl);
            //cout<<"Memory limit was "<<rl.rlim_cur<<endl;
            rl.rlim_cur = params.memLimit;
            setrlimit(RLIMIT_AS, &rl);
            //getrlimit(RLIMIT_AS, &rl);
            //cout<<"Memory limit is "<<rl.rlim_cur<<endl;
        }
#endif

        if (params.synthtype != SATSolver::MINI || params.veriftype != SATSolver::MINI) {
           cout<<"Only MiniSAT solver is supported in this version!"<<endl;
           //ABCSolverStart();
        }
  
        // too coarse of a timing?
    
        PARAMS = &params;
        Driver m(params);
        m.parseInput();

        //rv = m.resolveSketches();

    } catch(...) {
        cerr<<"Unusual Termination."<<endl;
        rv = 3;
    }
    mem.stop ();
    totalElapsed.stop ();

    printStats ();
    if (rv==0) {
        cout<<"ALL CORRECT"<<endl;
    }
    return rv;
}

