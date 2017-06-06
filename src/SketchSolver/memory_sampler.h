#ifndef __MEMORY_SAMPLER_H_
#define __MEMORY_SAMPLER_H_


//-----------------------------------------------------------------------------


#if defined (__linux)
#include <pthread.h>
#endif

#include <cassert>
#include <cstdlib>
#include <cstdio>
#include <cstring>

namespace statistics {



#include "memory_statistics.h"



class MemorySampler {
private:
	    statistics::MemoryStatistics memstats_;
#if defined (__linux)

    pthread_t sampler_thread_;
    pthread_mutex_t lock_;
    pthread_cond_t cond_;
    bool done_;

    unsigned int intervalSecs_;

    static void *mem_sampler_thread (void *arg);
#endif
public:
    MemorySampler (unsigned int intervalSecs = 1);
    virtual ~MemorySampler ();

    void run ();
    void stop ();
	MemorySampler& poll();
    statistics::MemoryStatistics getMemStats ();
};





}; /* namespace statistics */

#endif /* __MEMORY_SAMPLER_H_ */
