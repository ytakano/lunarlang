#include "lunar_opt.hpp"

#include <string.h>

#include <iostream>

#define C(s1, s2) strcmp((s1), (s2)) == 0

namespace lunar {

bool opt::parse(int argc, char *argv[]) {
    for (int n = 1; n < argc; n++) {
        if (C(argv[n], "-i") || C(argv[n], "--ir")) {
            m_is_lisp = true;
        } else if (C(argv[n], "-a") || C(argv[n], "--ast")) {
            m_is_ast = true;
        } else if (C(argv[n], "-l") || C(argv[n], "--llvm")) {
            m_is_llvm = true;
        } else if (C(argv[n], "-c") || C(argv[n], "--class")) {
            m_is_classenv = true;
        } else if (C(argv[n], "-h") || C(argv[n], "--help")) {
            print_help(argv);
            return false;
        } else {
            if (argv[n][0] == '-') {
                std::cerr << argv[n] << " is illegal option" << std::endl;
                print_help(argv);
                return false;
            }

            m_files.insert(argv[n]);
        }
    }

    return true;
}

void opt::print_help(char *argv[]) {
    std::cerr << argv[0] << "\n    -i,--ir:\tprint intermediate language\n"
              << "    -l,--llvm:\tprint LLVM IR\n"
              << "    -a,--ast:\tprint AST\n"
              << "    -h,--help:\tshow this help" << std::endl;
}

} // namespace lunar