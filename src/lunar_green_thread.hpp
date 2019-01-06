#ifndef LUNAR_GREEN_THREAD_HPP
#define LUNAR_GREEN_THREAD_HPP

#include <setjmp.h>

#include <deque>
#include <memory>

#include "external/tsl/hopscotch_map.h"
#include "external/tsl/hopscotch_set.h"
#include "lunar_channel.hpp"
#include "lunar_common.hpp"
#include "lunar_print.hpp"

extern "C" {

void init_thread();
void run_green_thread();
void yield_green_thread();
void spawn_green_thread(void (*func)(void *), void *arg,
                        uint32_t stack_size = 4096 * 32);
void *make_ch(int bucket_size, int len);
lunar::CH_RESULT send_ch(void *ch, const void *val);
lunar::CH_RESULT recv_ch(void *ch, void *val);

// TODO: float, double
void print_unum(uint64_t num);
void print_snum(int64_t num);
void print_boolean(bool num);
void print_utf8(const char *str);
void print_ptr(const void *ptr);
void print_flush();
void print_endl();

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
    uint64_t spawn(void (*func)(void *), void *arg,
                   uint32_t stack_size = 4096 * 32);

    print m_print;

  private:
    sigjmp_buf m_jmp_buf;

    context *m_running;
    std::deque<context *> m_suspend;
    tsl::hopscotch_map<channel *, context *> m_rch2ctx;
    tsl::hopscotch_map<channel *, tsl::hopscotch_set<context *>> m_wch2ctx;

    tsl::hopscotch_map<uint64_t, context *> m_id2ctx;

    context *m_remove;

    void remove_context(context *ctx);
    void first_switch(context *pre, context *ctx);
};

} // namespace lunar

#endif // LUNAR_GREEN_THREAD_HPP