#ifndef LUNAR_OPT_HPP
#define LUNAR_OPT_HPP

#include <string>
#include <unordered_set>

namespace lunar {

class opt {
  public:
    opt()
        : m_is_lisp(false), m_is_ast(false), m_is_llvm(false), m_is_env(false) {
    }
    virtual ~opt() {}

    bool parse(int argc, char *argv[]);
    void print_help(char *argv[]);

    bool m_is_lisp;
    bool m_is_ast;
    bool m_is_llvm;
    bool m_is_env;
    std::unordered_set<std::string> m_files;
};

}; // namespace lunar

#endif // LUNAR_OPT_HPP