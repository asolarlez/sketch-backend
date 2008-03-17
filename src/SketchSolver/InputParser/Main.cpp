#include <iostream>

#include "driver.h"
#include "memory_sampler.h"
#include "timerclass.h"

using namespace std;

using namespace statistics;

CommandLineArgs* PARAMS;

// Statistics -- could print these periodically
timerclass totalElapsed;
timerclass modelBuilding;
timerclass solution;
MemorySampler mem;

void
printStats ()
{
    MemoryStatistics ms = mem.getMemStats ();

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

int
main(int argc, char** argv)
{
    totalElapsed.start ();
    mem.run ();

    ABCSolverStart();
  
    modelBuilding.start ();     // too coarse of a timing?
    CommandLineArgs params(argc, argv);
    PARAMS = &params;
    Driver m(params);
    m.parseInput();
    modelBuilding.stop ();

    solution.start ();
    int rv = m.resolveSketches();
    solution.stop ();

    mem.stop ();
    totalElapsed.stop ();

    printStats ();

    return rv;
}


