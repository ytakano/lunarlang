#ifndef LUNAR_IR_HPP
#define LUNAR_IR_HPP

#include "lunar_parsec.hpp"

namespace lunar {
struct ir_expr {
    std::string m_name;
};
class ir_type {};
class ir_defun {};

class ir {
  public:
    ir(const std::string &str);
    virtual ~ir() {}

    std::unique_ptr<ir_expr> parse();

  private:
    parsec m_parsec;
    std::unordered_set<char> m_no_id_char_head;
    std::unordered_set<char> m_no_id_char;

    bool parse_expr();
    std::unique_ptr<ir_defun> parse_defun();
    std::string parse_id();
};
} // namespace lunar

#endif // LUNAR_IR_HPP