#ifndef RTM_LOCK_HPP
#define RTM_LOCK_HPP

// #define DEBUG_RTM

#ifdef __x86_64__
#include "external/tsx/rtm.h"
#include "external/tsx/tsx-cpuid.h"
#endif // __x86_64__

#if defined(__x86_64__) || defined(__i686__)
#include <xmmintrin.h>
#define _MM_PAUSE _mm_pause()
#else
#define _MM_PAUSE
#endif // __x86_64__ || __i686__

#include <assert.h>

#define RTM_MAX_RETRY 6

#ifdef DEBUG_RTM
#include <iostream>
#endif // DEBUG_RTM

namespace lunar {

class rtm_transaction;

class rtm_lock {
  public:
#ifdef __x86_64__
#ifdef DEBUG_RTM
    rtm_lock(bool is_rtm)
        : m_is_rtm(is_rtm), m_lock(0), m_nlock(0), m_nrtm(0) {}
    rtm_lock() : m_is_rtm(cpu_has_rtm()), m_lock(0), m_nlock(0), m_nrtm(0) {}
#else
    rtm_lock(bool is_rtm) : m_is_rtm(is_rtm), m_lock(0) {}
    rtm_lock() : m_is_rtm(cpu_has_rtm()), m_lock(0) {}
#endif // DEBUG_RTM
#else
    rtm_lock(bool is_rtm) : m_lock(0) {}
    rtm_lock() : m_lock(0) {}
#endif // __x86_64__

    ~rtm_lock() {}

  private:
#ifdef __x86_64__
    bool m_is_rtm;
#endif // __x86_64__

    volatile int m_lock;
#ifdef DEBUG_RTM
    volatile int m_nlock;
    volatile int m_nrtm;
#endif // DEBUG_RTM

    friend class rtm_transaction;
};

class rtm_transaction {
  public:
    rtm_transaction(rtm_lock &lock) : m_rtm_lock(lock) {
#ifdef __x86_64__
        if (lock.m_is_rtm) {
            unsigned status;
            int i;

            for (i = 0; i < RTM_MAX_RETRY; i++) {
                status = _xbegin_rtm();
                if (status == _XBEGIN_STARTED) {
                    if (!lock.m_lock) {
                        return;
                    }
                    _xabort(0xff);
                }

                if ((status & _XABORT_EXPLICIT) &&
                    _XABORT_CODE(status) == 0xff &&
                    !(status & _XABORT_NESTED)) {

                    while (lock.m_lock)
                        _MM_PAUSE; // busy-wait
                } else if (!(status & _XABORT_RETRY)) {
                    break;
                }
            }
        }
#endif // __x86_64__

        while (__sync_lock_test_and_set(&lock.m_lock, 1)) {
            while (lock.m_lock)
                _MM_PAUSE; // busy-wait
        }
    }

    ~rtm_transaction() {
#ifdef __x86_64__
        if (m_rtm_lock.m_lock) {
#ifdef DEBUG_RTM
            m_rtm_lock.m_nlock++;
#endif // DEBUG_RTM
            __sync_lock_release(&m_rtm_lock.m_lock);
        } else {
#ifdef DEBUG_RTM
            m_rtm_lock.m_nrtm++;
#endif // DEBUG_RTM
            _xend_rtm();
        }
#else
        __sync_lock_release(&m_rtm_lock.m_lock);
#endif // __x86_64__

#if defined(__x86_64__) && defined(DEBUG_RTM)
        if (((m_rtm_lock.m_nlock + m_rtm_lock.m_nrtm) % 10000000) == 0) {
            std::cout << "m_nlock = " << m_rtm_lock.m_nlock
                      << ", m_nrtm = " << m_rtm_lock.m_nrtm << std::endl;
        }
#endif // DEBUG_RTM
    }

  private:
    rtm_lock &m_rtm_lock;
};

} // namespace lunar

#endif // RTM_LOCK_HPP
