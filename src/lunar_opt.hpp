#ifndef LUNAR_OPT_HPP
#define LUNAR_OPT_HPP

#include <string>
#include <unordered_set>

namespace lunar {

class opt {
  public:
    opt() : m_is_lisp(false) {}
    virtual ~opt() {}

    bool parse(int argc, char *argv[]);

    bool m_is_lisp;
    std::unordered_set<std::string> m_files;
};

}; // namespace lunar

#endif // LUNAR_OPT_HPP