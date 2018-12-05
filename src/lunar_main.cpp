#include "lunar_green_thread.hpp"
#include "lunar_ir.hpp"

#include <stdio.h>

void world(void *arg) {
    //    printf("World!\n");
    std::list<lunar::ptr_ir_defun> defuns;
    lunar::ir ir("test.ir", "(defun fun (bool u32 u64) ((u64 arg1)))");
    auto result = ir.parse(defuns);
    if (result) {
        printf("parsed!\n");
    }
    yield_green_thread();
}

void hello(void *arg) {
    //    printf("Hello ");
    spawn_green_thread(world, nullptr);
    yield_green_thread();
}

int main(int argc, char *argv[]) {
    init_thread();
    spawn_green_thread(hello, nullptr);
    run_green_thread();

    return 0;
}