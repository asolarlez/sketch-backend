#include <cassert>
#include <cerrno>
#include <ctime>

#include "memory_sampler.h"


//-----------------------------------------------------------------------------
// class MemorySampler


#if defined (__linux)
void *
statistics::MemorySampler::mem_sampler_thread (void * /* MemorySampler * */ arg)
{
    statistics::MemorySampler *ms = (statistics::MemorySampler *) arg;
    struct timespec waitTime;
    int err;

    assert (0 == pthread_mutex_lock (&(ms->lock_)));

    do {
        statistics::MemoryStatistics curr;

        assert (0 == statistics::getMemoryStatistics (&curr));
        if (curr.total > ms->memstats_.total)
            ms->memstats_ = curr;

        assert (0 == clock_gettime (CLOCK_REALTIME, &waitTime));
        waitTime.tv_sec += ms->intervalSecs_;

        err = pthread_cond_timedwait (&(ms->cond_), &(ms->lock_), &waitTime);
        assert (0 == err || ETIMEDOUT == err);
    } while (!ms->done_);
    
    pthread_mutex_unlock (&(ms->lock_));

    return arg;
}

#endif

statistics::MemorySampler::MemorySampler (unsigned int intervalSecs)
{
    memset (&memstats_, 0, sizeof (memstats_));

#if defined (__linux)
    assert (0 == pthread_mutex_init (&lock_, NULL));
    assert (0 == pthread_cond_init (&cond_, NULL));
    done_ = false;
    intervalSecs_ = intervalSecs;
#endif
}

statistics::MemorySampler::~MemorySampler ()
{
#if defined (__linux)
     (pthread_mutex_destroy (&lock_));
     (pthread_cond_destroy (&cond_));
#endif
}

void 
statistics::MemorySampler::run ()
{
#if defined (__linux)
    assert (0 == pthread_create (&sampler_thread_, NULL,
                                 mem_sampler_thread, (void *) this));
#endif
}

void
statistics::MemorySampler::stop ()
{
    void *rv;
#if defined (__linux)
    assert (0 == pthread_mutex_lock (&lock_));
    done_ = true;
    assert (0 == pthread_cond_signal (&cond_));
    assert (0 == pthread_mutex_unlock (&lock_));

    assert (0 == pthread_join (sampler_thread_, &rv));

    assert (rv == (void *) this);

#else
	statistics::MemoryStatistics curr;

    assert (0 == statistics::getMemoryStatistics (&curr));
    if (curr.total > memstats_.total)
        memstats_ = curr;

#endif

}

statistics::MemorySampler&
statistics::MemorySampler::poll (){
	statistics::MemoryStatistics curr;
    assert (0 == statistics::getMemoryStatistics (&curr));
    if (curr.total > memstats_.total)
        memstats_ = curr;
	return *this;
}



statistics::MemoryStatistics
statistics::MemorySampler::getMemStats ()
{
    statistics::MemoryStatistics tmp;

#if defined (__linux)
	assert (0 == pthread_mutex_lock (&lock_));
    tmp = memstats_;
    assert (0 == pthread_mutex_unlock (&lock_));
#else
    tmp = memstats_;
#endif

    return tmp;
}
