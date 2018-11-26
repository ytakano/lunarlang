#include "lunar_green_thread.hpp"

#include <unistd.h>

#include <sys/mman.h>

// stack layout:
//
// 24(%rsp) | [empty]
// 16(%rsp) | context  : struct context*
//  8(%rsp) | argument : void*
//    %rsp  | function : void* -> void*
asm (
    ".global ___INVOKE;"
    "___INVOKE:"
    "movq 8(%rsp), %rdi;"  // set the argument
    "callq *(%rsp);"       // call function()
    "movq 16(%rsp), %rax;" // %rax = context
    "movl $4, (%rax);"     // context->m_state = TERMINATED
#ifdef __APPLE__
    "call _yield_green_thread;"  // call _yield_green_thread
#else  // *BSD, Linux
    "call yield_green_thread;"   // call yield_green_thread
#endif // __APPLE__
);

static const long pagesize = sysconf(_SC_PAGE_SIZE);

__thread static lunar::green_thread *p_green;
__thread static uint64_t current_id;

extern "C" {

void
yield_green_thread()
{
    p_green->yield();
}

void
init_thread()
{
    p_green = new lunar::green_thread;
    current_id = 0;

    p_green->run();
}

} // extern "C"

namespace lunar {

green_thread::green_thread() : m_running(nullptr)
{

}

green_thread::~green_thread()
{
    printf("delete\n");
}

void
green_thread::yield()
{
    printf("yield\n");
    siglongjmp(m_jmp_buf, 1);
}

uint64_t
green_thread::spawn(void (*func)(void*), void *arg, int stack_size)
{
    auto ctx = std::unique_ptr<context>(new context);

    for (;;) {
        current_id++;
        if (!HASKEY(m_id2ctx, current_id))
            break;
    }

    ctx->m_id    = current_id;
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

    ctx->m_stack = (uint64_t*)addr;
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

void
green_thread::run()
{
    if (sigsetjmp(m_jmp_buf, 0) == 0)
        yield();
    else
        delete this;
}

} // namespace lunar