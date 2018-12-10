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

struct ir_type : public ir_ast {
    enum IRTYPE {
        IRTYPE_SCALAR,
    };

    ir_type() {}
    virtual ~ir_type() {}

    virtual llvm::Type *codegen(ir &ref) = 0;
    virtual ir_type *clone() = 0;
    virtual std::string str() = 0;

    IRTYPE m_irtype;
};

typedef std::unique_ptr<ir_type> ptr_ir_type;

struct ir_scalar : public ir_type {
    ir_scalar() { m_irtype = IRTYPE_SCALAR; }

    void print();
    ir_type *clone() { return (new ir_scalar(*this)); };
    virtual std::string str();
    llvm::Type *codegen(ir &ref);

    type_spec m_type;
};

struct ir_statement : public ir_ast {
    ir_statement() {}
    virtual ~ir_statement() {}
};

struct ir_expr;
struct ir_id;
typedef std::unique_ptr<ir_expr> ptr_ir_expr;
typedef std::unique_ptr<ir_id> ptr_ir_id;

struct ir_defun : public ir_statement {
    ir_defun() {}
    virtual ~ir_defun() {}

    void print();
    bool check_type(const ir &ref);
    llvm::Function *codegen(ir &ref);

    std::string m_name;
    ptr_ir_type m_ret;
    std::vector<ptr_ir_id> m_args;
    ptr_ir_expr m_expr;
};

typedef std::unique_ptr<ir_defun> ptr_ir_defun;

struct ir_expr : public ir_ast {
    ir_expr() : m_expr_type(EXPRVAL) {}
    virtual ~ir_expr() {}

    enum EXPRTYPE { EXPRNOP, EXPRID, EXPRVAL };

    typedef std::shared_ptr<ir_type> shared_type;
    typedef std::unordered_map<std::string, std::deque<shared_type>> id2type;
    typedef std::unordered_map<std::string, std::deque<llvm::Value *>> id2val;

    EXPRTYPE m_expr_type;

    virtual shared_type check_type(const ir &ref, id2type &vars) = 0;
    virtual llvm::Value *codegen(ir &ref, id2val &vals) = 0;

    std::shared_ptr<ir_type> m_type;
};

struct ir_id : public ir_expr {
    ir_id() { m_expr_type = EXPRID; }

    std::string m_id;

    shared_type check_type(const ir &ref, id2type &vars);
    void print() { std::cout << "{\"id\":\"" << m_id << "\"}"; }

    llvm::Value *codegen(ir &ref, id2val &vals);
};

struct ir_apply : public ir_expr {
    ir_apply() {}
    virtual ~ir_apply() {}

    shared_type check_type(const ir &ref, id2type &vars);
    llvm::Value *codegen(ir &ref, id2val &vals);
    void print();

    std::vector<ptr_ir_expr> m_expr;

  private:
    shared_type check_ifexpr(const ir &ref, id2type &vars);
    shared_type check_magnitude(const ir &ref, id2type &vars,
                                const std::string &id);
    shared_type check_eq(const ir &ref, id2type &vars);
    llvm::Value *codegen_ifexpr(ir &ref, id2val vals);
};

typedef std::unique_ptr<ir_apply> ptr_ir_apply;

struct ir_decimal : public ir_expr {
    ir_decimal() {}
    virtual ~ir_decimal() {}

    shared_type check_type(const ir &ref, id2type &vars);
    llvm::Value *codegen(ir &ref, id2val &vals);
    void print();

    std::string m_num;
};

typedef std::unique_ptr<ir_decimal> ptr_ir_decimal;

struct ir_bool : public ir_expr {
    ir_bool() {}
    virtual ~ir_bool() {}

    shared_type check_type(const ir &ref, id2type &vars);
    llvm::Value *codegen(ir &ref, id2val &vals);
    void print();

    bool m_bool;
};

typedef std::unique_ptr<ir_bool> ptr_ir_bool;

struct ir_let : public ir_expr {
    ir_let() {}
    virtual ~ir_let() {}

    struct var {
        ptr_ir_id m_id;
        ptr_ir_expr m_expr;
    };

    shared_type check_type(const ir &ref, id2type &vars);
    llvm::Value *codegen(ir &ref, id2val &vals);
    void print();

    std::vector<std::unique_ptr<var>> m_def;
    ptr_ir_expr m_expr;
};

typedef std::unique_ptr<ir_let> ptr_ir_let;

class ir {
  public:
    ir(const std::string &filename, const std::string &str);
    virtual ~ir() {}

    bool parse();
    bool check_type();
    std::string codegen();
    void print();
    void print_err(std::size_t line, std::size_t column) const;

    llvm::LLVMContext &get_llvm_ctx() { return m_llvm_ctx; }
    llvm::Module &get_llvm_module() { return m_llvm_module; }
    llvm::IRBuilder<> &get_llvm_builder() { return m_llvm_builder; }

    std::string get_filename() const { return m_filename; }

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
    std::list<ptr_ir_defun> m_defuns;

    ptr_ir_expr parse_expr();
    ptr_ir_defun parse_defun();
    ptr_ir_type parse_type();
    ptr_ir_decimal parse_decimal();
    ptr_ir_let parse_let();
    std::string parse_id();
};
} // namespace lunar

#endif // LUNAR_IR_HPP