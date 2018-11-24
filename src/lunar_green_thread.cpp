#include "lunar_green_thread.hpp"

// stack layout:
//    [empty]
//    context
//    argument
//    func     <- %rsp
asm (
    ".global ___INVOKE;"
    "___INVOKE:"
    "movq 8(%rsp), %rdi;" // set the argument
    "callq *(%rsp);"      // call func()
    "movq 16(%rsp), %rax;"
    "movl $128, (%rax);"    // context.m_state = STOP
#ifdef __APPLE__
    "call _schedule_green_thread;"  // call _schedule_green_thread
#else  // *BSD, Linux
    "call schedule_green_thread;"   // call schedule_green_thread
#endif // __APPLE__
);

namespace lunar {

__thread green_thread *p_green;
__thread uint64_t id_green;

void
init_thread()
{
    p_green  = new green_thread;
    id_green = 0;
}

void
cleanup_thread()
{
    delete p_green;
}

green_thread::green_thread()
{

}

green_thread::~green_thread()
{

}

void
green_thread::yield()
{

}

} // namespace lunar