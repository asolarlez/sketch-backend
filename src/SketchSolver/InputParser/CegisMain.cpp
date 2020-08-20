// #include "config.h"
#include <iostream>
#include <cassert>
#include "CegisMainHelpers.h"
#include "driver.h"
#include "memory_sampler.h"
#include "timerclass.h"
#include <signal.h>
#include <thread>

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

bool CONTINUE_THREAD = true;

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

        if (params.dumpPeriodically > 0) {
            cout << "Outputing most recent controls every "<< params.dumpPeriodically<< " minutes" << endl;
            std::thread dumper([=]() {                
                int i = 0;
                while (CONTINUE_THREAD) {
                    std::this_thread::sleep_for(std::chrono::seconds(60* params.dumpPeriodically));
                    
                    DumpSignalHandler(16);
                }

                });

            dumper.detach();
        }

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
    CONTINUE_THREAD = false;
    printStats ();
    if (rv==0) {
        cout<<"ALL CORRECT"<<endl;
    }
	
    return rv;
}

