#ifndef LUNAR_GREEN_THREAD_HPP
#define LUNAR_GREEN_THREAD_HPP

#include <setjmp.h>

#include "lunar_common.hpp"

namespace lunar {

class green_thread {
public:
    green_thread();
    virtual ~green_thread();

    class context {
    public:
        // STATE := READY
        //        | RUNNING
        //        | STOP
        //        | SUSPENDING WAITING_TIMEOUT? WAITING_CHAN? WAITING_CHAN_MT? WAITING_FD?
        static const int READY           = 0x0001;
        static const int RUNNING         = 0x0002;
        static const int STOP            = 0x0004;
        static const int SUSPENDING      = 0x0008;
        static const int WAITING_TIMEOUT = 0x0010;
        static const int WAITING_CHAN    = 0x0020;
        static const int WAITING_CHAN_MT = 0x0040;
        static const int WAITING_FD      = 0x0080;

        context();
        virtual ~context();

        sigjmp_buf m_jmp_buf;

        int       m_state;
        uint64_t  m_id; // m_id must not be less than or equal to 0
        uint64_t *m_stack;
        int       m_stack_size;
    };

private:
    sigjmp_buf m_jmp_buf;

    void yield();
};

void init_thread();
void cleanup_thread();

} // namespace lunar

#endif // LUNAR_GREEN_THREAD_HPP