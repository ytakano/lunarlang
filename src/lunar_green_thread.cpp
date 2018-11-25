#include "lunar_green_thread.hpp"

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

__thread lunar::green_thread *p_green;
__thread uint64_t id_green;

extern "C" {

void
yield_green_thread()
{
    p_green->yield();
}

void
init_thread()
{
    p_green  = new lunar::green_thread;
    id_green = 0;

    p_green->run();
}

} // extern "C"

namespace lunar {

green_thread::green_thread()
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

void
green_thread::spawn()
{

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