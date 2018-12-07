#ifndef LUNAR_IR_HPP
#define LUNAR_IR_HPP

#include <deque>
#include <list>
#include <unordered_map>

#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/LLVMContext.h>
#include <llvm/IR/Module.h>

#include "lunar_common.hpp"
#include "lunar_parsec.hpp"

namespace lunar {

class ir;

struct ir_ast {
    ir_ast() : m_line(0), m_column(0) {}
    virtual ~ir_ast() {}
    virtual void print() {}

    std::size_t m_line;
    std::size_t m_column;
};

struct ir_expr : public ir_ast {
    ir_expr() : m_type(EXPRVAL) {}
    virtual ~ir_expr() {}

    enum EXPRTYPE { EXPRNOP, EXPRID, EXPRVAL };

    EXPRTYPE m_type;

    virtual llvm::Value *codegen(
        ir &ref,
        std::unordered_map<std::string, std::deque<llvm::Value *>> &vals) = 0;
};

typedef std::unique_ptr<ir_expr> ptr_ir_expr;

struct ir_type : public ir_ast {
    ir_type() {}
    virtual ~ir_type() {}

    virtual llvm::Type *codegen(ir &ref) = 0;
};

typedef std::unique_ptr<ir_type> ptr_ir_type;

struct ir_scalar : public ir_type {
    type_spec m_type;
    void print();

    llvm::Type *codegen(ir &ref);
};

struct ir_statement : public ir_ast {
    ir_statement() {}
    virtual ~ir_statement() {}
};

struct ir_defun : public ir_statement {
    ir_defun() {}
    virtual ~ir_defun() {}

    std::string m_name;
    std::vector<ptr_ir_type> m_ret;
    std::vector<std::unique_ptr<std::pair<ptr_ir_type, std::string>>> m_args;
    ptr_ir_expr m_expr;

    void print();

    llvm::Function *codegen(ir &ref);
};

typedef std::unique_ptr<ir_defun> ptr_ir_defun;

struct ir_id : public ir_expr {
    ir_id() { m_type = EXPRID; }

    std::string m_id;

    void print() { std::cout << "{\"id\":\"" << m_id << "\"}"; }
    llvm::Value *
    codegen(ir &ref,
            std::unordered_map<std::string, std::deque<llvm::Value *>> &vals);
};

typedef std::unique_ptr<ir_id> ptr_ir_id;

struct ir_apply : public ir_expr {
    ir_apply() {}
    virtual ~ir_apply() {}

    std::vector<ptr_ir_expr> m_expr;

    llvm::Value *
    codegen(ir &ref,
            std::unordered_map<std::string, std::deque<llvm::Value *>> &vals);

    void print();
};

typedef std::unique_ptr<ir_apply> ptr_ir_apply;

struct ir_decimal : public ir_expr {
    ir_decimal() {}
    virtual ~ir_decimal() {}

    std::string m_num;

    llvm::Value *
    codegen(ir &ref,
            std::unordered_map<std::string, std::deque<llvm::Value *>> &vals);

    void print();
};

typedef std::unique_ptr<ir_decimal> ptr_ir_decimal;

struct ir_let : public ir_expr {
    ir_let() {}
    virtual ~ir_let() {}

    std::vector<std::unique_ptr<std::pair<std::string, ptr_ir_expr>>> m_def;
    ptr_ir_expr m_expr;

    llvm::Value *
    codegen(ir &ref,
            std::unordered_map<std::string, std::deque<llvm::Value *>> &vals);

    void print();
};

typedef std::unique_ptr<ir_let> ptr_ir_let;

class ir {
  public:
    ir(const std::string &filename, const std::string &str);
    virtual ~ir() {}

    bool parse(std::list<ptr_ir_defun> &defuns);
    std::string codegen(std::list<ptr_ir_defun> &defuns);

    llvm::LLVMContext &get_llvm_ctx() { return m_llvm_ctx; }
    llvm::Module &get_llvm_module() { return m_llvm_module; }
    llvm::IRBuilder<> &get_llvm_builder() { return m_llvm_builder; }

    std::string get_filename() { return m_filename; }

  private:
    parsec m_parsec;
    std::string m_filename;
    std::unordered_set<char> m_no_id_char_head;
    std::unordered_set<char> m_no_id_char;
    std::unordered_set<char> m_0to9;
    std::unordered_set<char> m_1to9;
    llvm::LLVMContext m_llvm_ctx;
    llvm::IRBuilder<> m_llvm_builder;
    llvm::Module m_llvm_module;

    ptr_ir_expr parse_expr();
    ptr_ir_defun parse_defun();
    ptr_ir_type parse_type();
    ptr_ir_decimal parse_decimal();
    ptr_ir_let parse_let();
    std::string parse_id();
};
} // namespace lunar

#endif // LUNAR_IR_HPP