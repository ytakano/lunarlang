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

const char *e1 = "(defun fun u32 ((u32 arg1) (u32 arg2))\n"
                 "    (let ((u32 x (+ arg1 arg2))\n"
                 "         (u32 y (* arg1 arg2))) (/ (- y x) arg1)))";

const char *e2 = "(defun fun u32 ((u64 arg)) arg)";

const char *e3 = "(defun fun u32 ((u64 arg1) (bool arg2)) (+ arg1 arg2))";

const char *e4 = "(defun fun u32 ((bool arg1) (u32 arg2) (u32 arg3))\n"
                 "    (if arg1\n"
                 "        (+ arg2 arg3 1)\n"
                 "        (* arg2 arg3 2)))";

const char *e5 = "(defun fun bool ((u64 arg1) (u64 arg2)) (!= arg1 arg2))";

void world(void *arg) {
    std::string s = e5;

    lunar::ir ir("test.ir", s);

    if (ir.parse() && ir.check_type()) {
        std::cout << "{\"input\":\"" << escapeJsonString(s) << "\",\"AST\":";
        ir.print();
        std::cout << ",\"LLVM\":\"" << escapeJsonString(ir.codegen()) << "\"}"
                  << std::endl;
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