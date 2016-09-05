#ifndef __MEMORY_STATISTICS_H_
#define __MEMORY_STATISTICS_H_


#include <cassert>
#include <cstdlib>
#include <cstdio>
#include <cstring>

#if defined (__linux)
#include <sys/user.h>
#include <unistd.h>

#elif defined (_MSC_VER)
#include <windows.h>
#include <psapi.h>

#else

#warning "Can't get memory stats on this system.  They will show up as '0'."

#endif

#ifdef __Cplusplus
extern "C" {
#endif


/**
 * All quantities represent memory sizes in units of bytes.
 *
 * The values reported are as follows:
 *
 *  - total:
 *  - resident:
 *  - shared:
 *  - code:
 *  - library:
 *  - data:
 *  - pageSize:
 */
typedef struct _MemoryStatistics {
    long  total;
    long  resident;
    long  shared;
    long  code;
    long  library;
    long  data;
    long  dirty;
    long  pageSize;
} MemoryStatistics;


/** Returns 0 iff fetching memory stats was successful. */

static int
getMemoryStatistics (MemoryStatistics *ms)
{
//-----------------------------------------------------------------------------
#if defined (__linux)

    long pageSize;
    char statFileName[128];
    FILE *statFile;

    assert (NULL != ms);
    memset (ms, 0, sizeof (*ms));

    ms->pageSize = pageSize = PAGE_SIZE;

    if (0 >= snprintf (statFileName,
                       sizeof (statFileName), "/proc/%d/statm", getpid ()))
        return -1;
    if (NULL == (statFile = fopen (statFileName, "r")))
        return -1;

    if (7 != fscanf (statFile, "%ld %ld %ld %ld %ld %ld %ld",
                     &ms->total,
                     &ms->resident, &ms->shared, &ms->code,
                     &ms->library, &ms->data, &ms->dirty))
        return -1;
    if (0 != fclose (statFile))
        return -1;

    ms->total *= pageSize;
    ms->resident *= pageSize;
    ms->shared *= pageSize;
    ms->code *= pageSize;
    ms->library *= pageSize;
    ms->data *= pageSize;
    ms->dirty *= pageSize;

//-----------------------------------------------------------------------------
#elif defined (_MSC_VER)

    DWORD myPID;
    HANDLE hProcess;
    PROCESS_MEMORY_COUNTERS pmc;

    assert (NULL != ms);
    memset (ms, 0, sizeof (*ms));

    myPID = GetCurrentProcessId ();
    if (NULL ==
        (hProcess = OpenProcess (PROCESS_QUERY_INFORMATION | PROCESS_VM_READ,
                                 FALSE, myPID)))
        return -1;
    if (0 == GetProcessMemoryInfo (hProcess, &pmc, sizeof (pmc)))
        return -1;
    if (0 == CloseHandle (hProcess))
        return -1;

    ms->total = pmc.PeakPagefileUsage;
    ms->resident = pmc.PeakWorkingSetSize;
    ms->shared = 0; // pmc.PagefileUsage - pmc.PrivateUsage;

#else
    assert (NULL != ms);
    memset (ms, 0, sizeof (*ms));

#endif

    return 0;
}


#ifdef __Cplusplus
}
#endif

#endif /* __MEMORY_STATISTICS_H_ */
