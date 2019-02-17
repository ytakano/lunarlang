#include "lunar_opt.hpp"

#include <string.h>

#include <iostream>

#define C(s1, s2) strcmp((s1), (s2)) == 0

namespace lunar {

bool opt::parse(int argc, char *argv[]) {
    for (int n = 1; n < argc; n++) {
        if (C(argv[n], "-l") || C(argv[n], "--lisp")) {
            m_is_lisp = true;
        }
        if (C(argv[n], "-a") || C(argv[n], "--ast")) {
            m_is_ast = true;
        } else {
            if (argv[n][0] == '-') {
                std::cerr << argv[n] << " is illegal" << std::endl;
                return false;
            }

            m_files.insert(argv[n]);
        }
    }

    return true;
}

} // namespace lunar