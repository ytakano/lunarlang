#ifndef LUNAR_GREEN_THREAD_HPP
#define LUNAR_GREEN_THREAD_HPP

#include <setjmp.h>

#include <deque>
#include <memory>

#include <unordered_map>
#include <unordered_set>

#include "lunar_channel.hpp"
#include "lunar_common.hpp"

extern "C" {

void init_thread();
void run_green_thread();
void yield_green_thread();
void spawn_green_thread(void (*func)(void *), void *arg,
                        int stack_size = 4096 * 32);
void *make_ch(int bucket_size, int len);
lunar::CH_RESULT send_ch(void *ch, const void *val);
lunar::CH_RESULT recv_ch(void *ch, void *val);

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
        static const int READY = 0x0001;
        static const int RUNNING = 0x0002;
        static const int TERMINATED = 0x0004;
        static const int SUSPENDING = 0x0008;
        static const int WAITING_TIMEOUT = 0x0010;
        static const int WAITING_CHAN_READ = 0x0020;
        static const int WAITING_CHAN_WRITE = 0x0040;
        static const int WAITING_CHAN_MT_READ = 0x0080;
        static const int WAITING_CHAN_MT_WRITE = 0x0100;
        static const int WAITING_FD = 0x0200;

        uint32_t m_state;
        uint32_t m_stack_size;
        uint64_t m_id; // m_id must not be less than or equal to 0
        uint64_t *m_stack;
        sigjmp_buf m_jmp_buf;
    };

    typedef std::unique_ptr<context> ptr_context;

    void yield();
    void run();
    uint64_t spawn(void (*func)(void *), void *arg, int stack_size = 4096 * 32);

  private:
    sigjmp_buf m_jmp_buf;

    context *m_running;
    std::deque<context *> m_suspend;
    std::unordered_map<channel *, context *> m_rch2ctx;
    std::unordered_map<channel *, std::unordered_set<context *>> m_wch2ctx;

    std::unordered_map<uint64_t, ptr_context> m_id2ctx;

    ptr_context m_remove;

    void remove_context(context *ctx);
    void first_switch(context *pre, context *ctx);
};

} // namespace lunar

#endif // LUNAR_GREEN_THREAD_HPP