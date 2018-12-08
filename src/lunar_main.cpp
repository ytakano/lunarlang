#include "lunar_green_thread.hpp"
#include "lunar_ir.hpp"

#include <sstream>

#include <stdio.h>

static std::string escapeJsonString(const std::string &input) {
    std::ostringstream ss;
    for (auto iter = input.cbegin(); iter != input.cend(); iter++) {
        // C++98/03:
        // for (std::string::const_iterator iter = input.begin(); iter !=
        // input.end(); iter++) {
        switch (*iter) {
        case '\\':
            ss << "\\\\";
            break;
        case '"':
            ss << "\\\"";
            break;
        case '/':
            ss << "\\/";
            break;
        case '\b':
            ss << "\\b";
            break;
        case '\f':
            ss << "\\f";
            break;
        case '\n':
            ss << "\\n";
            break;
        case '\r':
            ss << "\\r";
            break;
        case '\t':
            ss << "\\t";
            break;
        default:
            ss << *iter;
            break;
        }
    }
    return ss.str();
}

const char *e1 = "(defun fun (u64) ((u32 arg1) (u32 arg2))\n"
                 "                  (let ((u32 x (+ arg1 arg2))\n"
                 "                        (u32 y (* arg1 arg2))) (- y x)))";

void world(void *arg) {
    std::list<lunar::ptr_ir_defun> defuns;
    std::string s = e1;

    lunar::ir ir("test.ir", s);
    auto result = ir.parse(defuns);

    if (result) {
        std::cout << "{\"input\":\"" << escapeJsonString(s) << "\",\"AST\":";
        for (auto &p : defuns) {
            p->print();
        }

        std::cout << ",\"LLVM\":\"" << escapeJsonString(ir.codegen(defuns))
                  << "\"}" << std::endl;
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