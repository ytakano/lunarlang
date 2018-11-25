#ifndef LUNAR_GREEN_THREAD_HPP
#define LUNAR_GREEN_THREAD_HPP

#include <setjmp.h>

#include <deque>
#include <memory>

#include "lunar_common.hpp"

extern "C" {
void init_thread();
} // extern "C"

namespace lunar {

class green_thread {
public:
    green_thread();
    virtual ~green_thread();

    struct context {
        // STATE := READY
        //        | RUNNING
        //        | TERMINATED
        //        | SUSPENDING
        //        | WAITING+
        //
        // WAITING := WAITING_TIMEOUT
        //          | WAITING_CHAN_READ
        //          | WAITING_CHAN_WRITE
        //          | WAITING_CHAN_MT_READ
        //          | WAITING_CHAN_MT_WRITE
        //          | WAITING_FD
        static const int READY                 = 0x0001;
        static const int RUNNING               = 0x0002;
        static const int TERMINATED            = 0x0004;
        static const int SUSPENDING            = 0x0008;
        static const int WAITING_TIMEOUT       = 0x0010;
        static const int WAITING_CHAN_READ     = 0x0020;
        static const int WAITING_CHAN_WRITE    = 0x0040;
        static const int WAITING_CHAN_MT_READ  = 0x0080;
        static const int WAITING_CHAN_MT_WRITE = 0x0100;
        static const int WAITING_FD            = 0x0200;

        uint32_t   m_state;
        uint32_t   m_stack_size;
        uint64_t   m_id; // m_id must not be less than or equal to 0
        uint64_t  *m_stack;
        sigjmp_buf m_jmp_buf;
    };

    typedef std::unique_ptr<context> ptr_context;

    void yield();
    void run();
    void spawn();

private:
    sigjmp_buf  m_jmp_buf;
    ptr_context m_running;

    std::deque<ptr_context> m_suspend;
};

} // namespace lunar

#endif // LUNAR_GREEN_THREAD_HPP