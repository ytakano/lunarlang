#include "lunar_green_thread.hpp"
#include "lunar_ir.hpp"

#include <stdio.h>

void world(void *arg) {
    std::list<lunar::ptr_ir_defun> defuns;
    std::string s =
        "(defun fun (bool u32 u64) ((u64 arg1) (u32 arg2)) ( e1 e2 ))";
    lunar::ir ir("test.ir", s);
    auto result = ir.parse(defuns);
    if (result) {
        std::cout << "{\"input\":\"" << s << "\",\"AST\":";
        for (auto &p : defuns) {
            p->print();
        }
        std::cout << "}" << std::endl;
    } else {
        std::cout << "false" << std::endl;
    }
    yield_green_thread();
}

void hello(void *arg) {
    spawn_green_thread(world, nullptr);
    yield_green_thread();
}

int main(int argc, char *argv[]) {
    init_thread();
    spawn_green_thread(hello, nullptr);
    run_green_thread();

    return 0;
}