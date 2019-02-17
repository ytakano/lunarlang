#include "lunar_green_thread.hpp"
#include "lunar_ir.hpp"
#include "lunar_opt.hpp"
#include "lunar_parser.hpp"
#include "lunar_string.hpp"

#include <sstream>

#include <getopt.h>
#include <stdio.h>

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

const char *e6 = "(defun fun bool ((u32 arg1)) (fun arg1))";

const char *e7 = "(defun fact u64 ((u64 num))\n"
                 "    (if (= num 0)\n"
                 "     1\n"
                 "     (* num (fact (- num 1)))))\n";

const char *e8 = "(defun fun u64 ((u64 num))\n"
                 "    (if (= num 0)\n"
                 "     1\n"
                 "     (fun (- num 1))))\n";

const char *e9 = "(defun fun s64 () (fact 5))\n"
                 "(defun fact s64 ((s64 num))\n"
                 "    (if (= num 0)\n"
                 "     1\n"
                 "     (* num (fact (- num 1)))))\n";

const char *e10 =
    "(struct mystruct (u64 foo) (u32 bar))\n"
    "(struct mystruct2 (mystruct foo) (u32 buz))\n"
    "(defun fun (struct mystruct u32) (((struct u64 u32 bool) arg))\n"
    "    (mystruct2 (mystruct 10 20) 30))";

const char *e11 = "(struct mystruct (u64 foo) (mystruct2 bar))\n"
                  "(struct mystruct2 (u64 foo) (mystruct bar))";

const char *e12 = "(defun fun (ref u32) (((ref u32) foo)) foo)";

const char *e13 = "(struct mystruct (u64 hoge) (u64 huga))\n"
                  "(struct mystruct2 (mystruct m) (u64 foo) (u32 bar))\n"
                  "(defun fun (ref mystruct2) ((u32 foo))\n"
                  "    (let (((ref (struct mystruct u64 u32)) x\n"
                  "                (mystruct2 (mystruct 222 333) 111 foo))\n"
                  "          ((ref mystruct2) y x))\n"
                  "        y))";

const char *e14 = "(defun fun u32 () (let ((u32 x 20)) 10))";

const char *e15 = "(struct st (u64 foo))\n"
                  "(struct mystruct ((ref st) bar)  )\n"
                  "(defun fun u32 ((u32 foo))\n"
                  "    (let (((ref mystruct) x (mystruct (st 10)))) 20))";

const char *e16 = "(defun fun void ((u32 arg1) (u32 arg2) (void arg3))\n"
                  "    (let ((u32 x (+ arg1 arg2))\n"
                  "          (u32 y (* arg1 arg2))) ()))\n"
                  "(defun fun2 void ()\n"
                  "    (let ((void x (fun 1 2 (fun 3 4 ())))) x))";

const char *e17 = "(defun main void () (print_unum 10))";

const char *e18 = "(struct st (u64 foo))\n"
                  "(defun func void (((fun u32 ((ref st))) arg)) ())";

const char *e19 = "(defun func (ref utf8) () \"Hello world!\\n\")";

const char *e20 = "(defun main void ()\n"
                  "    (print \"Hello world!\" 100 -200 0 10.7 () true false))";

const char *e21 = "(defun main void () (print 100.1e-3f))";

const char *e22 = "(struct st (s32 foo))\n"
                  "(defun func void ((s32 arg))\n"
                  "    (let (((ref st) v (st arg)))\n"
                  "        (if (= arg 0)\n"
                  "            ()\n"
                  "            (func (- arg 1)))))";

const char *e23 = "(defun fun (ref (vec u32)) (((ref (vec u32)) arg))\n"
                  "    (let (((ref (vec u32)) x (vec u32 10))) x))";

const char *e24 = "(struct s ((vec u32 10) x))\n"
                  "(defun fun void (((ref s) arg)) ())";

const char *e25 = "(struct s ((vec u32 2) x))\n"
                  "(defun fun (ref s) ()\n"
                  "    (let (((ref (vec u32)) x (vec u32 2))\n"
                  "          ((ref s) y (s x))) y))";

const char *e26 = "(defun fun (ref (vec u32)) ()\n"
                  "    (let (((ref (vec u32)) x (vec u32 20))) x))";

const char *e27 = "(struct st1 (s32 bar) (u64 foo))\n"
                  "(struct st2 (u8 fiz) (st1 buz))\n"
                  "(defun fun (ref u64) (((ref st2) arg))\n"
                  "    (elm (elm arg buz) foo))";

const char *e28 = "(defun fun (ref u32) (((ref (vec u32)) arg))\n"
                  "    (elm arg 1))";

const char *e29 = "(defun fun (ref (fun void ()))\n"
                  "           (((ref (ref (fun void ()))) arg))\n"
                  "    (load arg))";

const char *e30 = "(defun fun void (((ref u32) arg))\n"
                  "    (store arg 10))";

const char *e31 = "(struct st (u32 foo))\n"
                  "(defun fun void (((ref st) arg)) ())\n"
                  "(defun fun2 void () (fun (st 10)))\n"
                  "(defun fun3 void ()\n"
                  "    (let (((ref st) a (st 20))) (fun a)))";

const char *e32 = "(defun fun void (((ref (vec u32)) arg)) ())\n"
                  "(defun fun2 void () (fun (vec u32 10)))\n"
                  "(defun fun3 void ()\n"
                  "    (let (((ref (vec u32)) a (vec u32 20))) (fun a)))";

const char *e33 =
    "(defun fun void (((ref u32) arg)) ())\n"
    "(defun fun2 void () (fun (elm (vec u32 10) 0)))\n"
    "(defun fun3 void ()\n"
    "    (let (((ref (vec u32)) a (vec u32 20))) (fun (elm a 1))))";

const char *e34 = "(struct st (u32 foo))\n"
                  "(defun fun void (((ref u32) arg)) ())\n"
                  "(defun fun2 void () (fun (elm (st 10) foo)))\n"
                  "(defun fun3 void ()\n"
                  "    (let (((ref st) a (st 20))) (fun (elm a foo))))";

void world(void *arg) {
    std::string s = e9;

    lunar::ir ir("test.ir", s);

    if (ir.parse() && ir.check_type()) {
        std::cout << "{\"input\":\"" << lunar::escape_json(s) << "\",\"AST\":";
        ir.print();
        std::cout << ",\"LLVM\":\"" << lunar::escape_json(ir.codegen()) << "\"}"
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
    //    init_thread();
    //    spawn_green_thread(hello, nullptr);
    //    run_green_thread();

    lunar::opt opt;
    if (!opt.parse(argc, argv))
        return 1;

    lunar::parser parser;

    for (auto &filename : opt.m_files) {
        parser.add_module(filename);
    }

    if (!parser.parse())
        return 1;

    if (opt.m_is_ast) {
        parser.print();
    }

    return 0;
}
