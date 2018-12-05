#ifndef LUNAR_IR_HPP
#define LUNAR_IR_HPP

#include <list>

#include "lunar_common.hpp"
#include "lunar_parsec.hpp"

namespace lunar {

struct ir_expr {
    ir_expr() {}
    virtual ~ir_expr() {}
};

typedef std::unique_ptr<ir_expr> ptr_ir_expr;

struct ir_type {
    ir_type() {}
    virtual ~ir_type() {}
};

typedef std::unique_ptr<ir_type> ptr_ir_type;

struct ir_scalar : public ir_type {
    type_spec m_type;
};

struct ir_defun : public ir_expr {
    ir_defun() {}
    virtual ~ir_defun() {}
    std::string m_name;
    std::list<ptr_ir_type> m_ret;
    std::list<std::unique_ptr<std::pair<ptr_ir_type, std::string>>> m_args;
    ptr_ir_expr m_expr;
};

typedef std::unique_ptr<ir_defun> ptr_ir_defun;

class ir {
  public:
    ir(const std::string &filename, const std::string &str);
    virtual ~ir() {}

    bool parse(std::list<ptr_ir_defun> &defuns);

  private:
    parsec m_parsec;
    std::string m_filename;
    std::unordered_set<char> m_no_id_char_head;
    std::unordered_set<char> m_no_id_char;

    ptr_ir_expr parse_expr();
    ptr_ir_defun parse_defun();
    ptr_ir_type parse_type();
    std::string parse_id();
};
} // namespace lunar

#endif // LUNAR_IR_HPP