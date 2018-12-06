#ifndef LUNAR_IR_HPP
#define LUNAR_IR_HPP

#include <list>
#include <unordered_map>

#include "lunar_common.hpp"
#include "lunar_parsec.hpp"

namespace lunar {

struct ir_expr {
    ir_expr() {}
    virtual ~ir_expr() {}
    virtual void print() {}
};

typedef std::unique_ptr<ir_expr> ptr_ir_expr;

struct ir_type {
    ir_type() {}
    virtual ~ir_type() {}
    virtual void print() {}
};

typedef std::unique_ptr<ir_type> ptr_ir_type;

struct ir_scalar : public ir_type {
    type_spec m_type;

    void print();
};

struct ir_statement {
    ir_statement() {}
    virtual ~ir_statement() {}
    virtual void print() {}
};

struct ir_defun : public ir_statement {
    ir_defun() {}
    virtual ~ir_defun() {}

    std::string m_name;
    std::list<ptr_ir_type> m_ret;
    std::list<std::unique_ptr<std::pair<ptr_ir_type, std::string>>> m_args;
    ptr_ir_expr m_expr;

    void print();
};

typedef std::unique_ptr<ir_defun> ptr_ir_defun;

struct ir_id : public ir_expr {
    std::string m_id;

    void print() { std::cout << "{\"id\":\"" << m_id << "\"}"; }
};

typedef std::unique_ptr<ir_id> ptr_ir_id;

struct ir_apply : public ir_expr {
    ir_apply() {}
    virtual ~ir_apply() {}

    std::list<ptr_ir_expr> m_expr;

    void print();
};

typedef std::unique_ptr<ir_apply> ptr_ir_apply;

struct ir_decimal : public ir_expr {
    ir_decimal() {}
    virtual ~ir_decimal() {}

    std::string m_num;

    void print();
};

typedef std::unique_ptr<ir_decimal> ptr_ir_decimal;

struct ir_let : public ir_expr {
    ir_let() {}
    virtual ~ir_let() {}

    std::list<std::unique_ptr<std::pair<std::string, ptr_ir_expr>>> m_def;
    std::list<ptr_ir_expr> m_expr;

    void print();
};

typedef std::unique_ptr<ir_let> ptr_ir_let;

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
    std::unordered_set<char> m_0to9;
    std::unordered_set<char> m_1to9;

    ptr_ir_expr parse_expr();
    ptr_ir_defun parse_defun();
    ptr_ir_type parse_type();
    ptr_ir_decimal parse_decimal();
    ptr_ir_let parse_let();
    std::string parse_id();
};
} // namespace lunar

#endif // LUNAR_IR_HPP