#include "lunar_green_thread.hpp"

#include <unistd.h>

#include <sys/mman.h>

// stack layout:
//
// 24(%rsp) | [empty]
// 16(%rsp) | context  : struct context*
//  8(%rsp) | argument : void*
//    %rsp  | function : void* -> void*
asm(".global ___INVOKE;"
    "___INVOKE:"
    "movq 8(%rsp), %rdi;"  // set the argument
    "callq *(%rsp);"       // call function()
    "movq 16(%rsp), %rax;" // %rax = context
    "movl $4, (%rax);"     // context->m_state = TERMINATED
#ifdef __APPLE__
    "call _yield_green_thread;" // call _yield_green_thread
#else                           // *BSD, Linux
    "call yield_green_thread;" // call yield_green_thread
#endif                          // __APPLE__
);

static const long pagesize = sysconf(_SC_PAGE_SIZE);

__thread static lunar::green_thread *p_green;
__thread static uint64_t current_id;

extern "C" {

void yield_green_thread() { p_green->yield(); }

void init_thread() {
    p_green = new lunar::green_thread;
    current_id = 0;
}

void run_green_thread() { p_green->run(); }

void spawn_green_thread(void (*func)(void *), void *arg, uint32_t stack_size) {
    p_green->spawn(func, arg, stack_size);
}

void *make_ch(int bucket_size, int len) {
    return new lunar::channel(bucket_size, len);
}

lunar::CH_RESULT send_ch(void *ch, const void *val) {
    return ((lunar::channel *)ch)->push((const char *)val);
}

lunar::CH_RESULT recv_ch(void *ch, void *val) {
    return ((lunar::channel *)ch)->pop((char *)val);
}

void print_unum(uint64_t num) { p_green->m_print.unum(num); }
void print_snum(int64_t num) { p_green->m_print.snum(num); }
void print_boolean(bool num) { p_green->m_print.boolean(num); }
void print_utf8(const char *str) { p_green->m_print.utf8(str); }
void print_ptr(const void *ptr) { p_green->m_print.ptr(ptr); }
void print_flush() { p_green->m_print.flush(); }
void print_endl() { p_green->m_print.endl(); }

} // extern "C"

namespace lunar {

green_thread::green_thread() : m_running(nullptr) {}

green_thread::~green_thread() {}

void green_thread::remove_context(context *ctx) {}

void green_thread::first_switch(context *pre, context *ctx) {}

void green_thread::yield() {
    auto previous = m_running;

    if (previous) {
        if (previous->m_state == context::RUNNING) {
            previous->m_state = context::SUSPENDING;
            m_suspend.push_back(previous);
        } else if (previous->m_state == context::TERMINATED) {
            auto it = m_id2ctx.find(previous->m_id);
            m_remove = std::move(it->second);
            m_id2ctx.erase(it);
            previous = nullptr;
        }
    }

    if (!m_suspend.empty()) {
        m_running = m_suspend.front();
        m_suspend.pop_front();
        auto state = m_running->m_state;
        m_running->m_state = context::RUNNING;

        if (state == context::READY) {
            if (previous) {
                if (sigsetjmp(previous->m_jmp_buf, 0) == 0) {
                    auto p = &m_running->m_stack[m_running->m_stack_size - 4];
                    asm("movq %0, %%rsp;" // set stack pointer
                        "movq %0, %%rbp;" // set frame pointer
                        "jmp ___INVOKE;"
                        :
                        : "r"(p));
                } else {
                    return;
                }
            } else {
                auto p = &m_running->m_stack[m_running->m_stack_size - 4];
                asm("movq %0, %%rsp;" // set stack pointer
                    "movq %0, %%rbp;" // set frame pointer
                    "jmp ___INVOKE;"
                    :
                    : "r"(p));
            }
        } else if (state == context::SUSPENDING) {
            if (previous) {
                if (sigsetjmp(previous->m_jmp_buf, 0) == 0)
                    siglongjmp(m_running->m_jmp_buf, 1);
                else
                    return;
            } else {
                siglongjmp(m_running->m_jmp_buf, 1);
            }
        }
    }

    siglongjmp(m_jmp_buf, 1);
}

uint64_t green_thread::spawn(void (*func)(void *), void *arg,
                             uint32_t stack_size) {
    auto ctx = std::unique_ptr<context>(new context);

    for (;;) {
        current_id++;
        if (!HASKEY(m_id2ctx, current_id))
            break;
    }

    ctx->m_id = current_id;
    ctx->m_state = context::READY;

    stack_size += pagesize;
    stack_size -= stack_size % pagesize;

    if (stack_size < pagesize * 2)
        stack_size = pagesize * 2;

    void *addr;

    if (posix_memalign(&addr, pagesize, stack_size) != 0) {
        PRINTERR("failed posix_memalign!: %s", strerror(errno));
        exit(-1);
    }

    ctx->m_stack = (uint64_t *)addr;
    ctx->m_stack_size = stack_size / sizeof(uint64_t);

    auto s = ctx->m_stack_size;
    ctx->m_stack[s - 2] = (uint64_t)ctx.get(); // push context
    ctx->m_stack[s - 3] = (uint64_t)arg;       // push argument
    ctx->m_stack[s - 4] = (uint64_t)func;      // push func

    // see /proc/sys/vm/max_map_count for Linux
    if (mprotect(&ctx->m_stack[0], pagesize, PROT_NONE) < 0) {
        PRINTERR("failed mprotect!: %s", strerror(errno));
        exit(-1);
    }

    m_suspend.push_back(ctx.get());
    m_id2ctx[current_id] = std::move(ctx);

    return current_id;
}

void green_thread::run() {
    if (sigsetjmp(m_jmp_buf, 0) == 0)
        yield();
    else
        delete this;
}

} // namespace lunar