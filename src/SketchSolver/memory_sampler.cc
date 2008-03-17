#include <cassert>
#include <cerrno>
#include <ctime>

#include "memory_sampler.h"


//-----------------------------------------------------------------------------
// class MemorySampler

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

statistics::MemorySampler::MemorySampler (unsigned int intervalSecs)
{
    memset (&memstats_, 0, sizeof (memstats_));
    assert (0 == pthread_mutex_init (&lock_, NULL));
    assert (0 == pthread_cond_init (&cond_, NULL));
    done_ = false;
    intervalSecs_ = intervalSecs;
}

statistics::MemorySampler::~MemorySampler ()
{
    assert (0 == pthread_mutex_destroy (&lock_));
    assert (0 == pthread_cond_destroy (&cond_));
}

void 
statistics::MemorySampler::run ()
{
    assert (0 == pthread_create (&sampler_thread_, NULL,
                                 mem_sampler_thread, (void *) this));
}

void
statistics::MemorySampler::stop ()
{
    void *rv;

    assert (0 == pthread_mutex_lock (&lock_));
    done_ = true;
    assert (0 == pthread_cond_signal (&cond_));
    assert (0 == pthread_mutex_unlock (&lock_));

    assert (0 == pthread_join (sampler_thread_, &rv));

    assert (rv == (void *) this);
}

statistics::MemoryStatistics
statistics::MemorySampler::getMemStats ()
{
    statistics::MemoryStatistics tmp;

    assert (0 == pthread_mutex_lock (&lock_));
    tmp = memstats_;
    assert (0 == pthread_mutex_unlock (&lock_));

    return tmp;
}
