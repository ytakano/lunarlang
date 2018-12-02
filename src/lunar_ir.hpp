#ifndef LUNAR_IR_HPP
#define LUNAR_IR_HPP

#include "lunar_parsec.hpp"

namespace lunar {
class ir_type {};
class ir_defun {};

class ir {
  public:
    ir(const std::string &str) : m_parsec(str) {}
    virtual ~ir() {}

    bool parse();

  private:
    parsec m_parsec;

    bool parse_expr();
};
} // namespace lunar

#endif // LUNAR_IR_HPP