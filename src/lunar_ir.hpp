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
        IRTYPE_REF,
        IRTYPE_FUN,
        IRTYPE_STRUCT,
        IRTYPE_USER,
    };

    ir_type() {}
    virtual ~ir_type() {}

    virtual llvm::Type *codegen(ir &ref) = 0;
    virtual ir_type *clone() const = 0;
    virtual std::string str() const = 0;

    IRTYPE m_irtype;
};

typedef std::unique_ptr<ir_type> ptr_ir_type;
typedef std::shared_ptr<ir_type> shared_ir_type;

struct ir_scalar : public ir_type {
    ir_scalar() { m_irtype = IRTYPE_SCALAR; }

    void print();
    ir_type *clone() const { return (new ir_scalar(*this)); };
    std::string str() const;
    llvm::Type *codegen(ir &ref);

    type_spec m_type;
};

struct ir_funtype : public ir_type {
    ir_funtype() { m_irtype = IRTYPE_FUN; }

    void print();
    ir_type *clone() const { return (new ir_funtype(*this)); }
    std::string str() const;
    llvm::Type *codegen(ir &ref) { return nullptr; };

    shared_ir_type m_ret;
    std::vector<shared_ir_type> m_args;
};

typedef std::unique_ptr<ir_funtype> ptr_ir_funtype;

struct ir_struct : public ir_type {
    ir_struct() { m_irtype = IRTYPE_STRUCT; }

    void print();
    ir_type *clone() const { return (new ir_struct(*this)); }
    std::string str() const;
    llvm::Type *codegen(ir &ref);

    std::string m_name;
    std::unordered_map<std::string, int> m_id2idx;
    std::vector<shared_ir_type> m_member;
};

typedef std::unique_ptr<ir_struct> ptr_ir_struct;

struct ir_usertype : public ir_type {
    ir_usertype() { m_irtype = IRTYPE_USER; }

    void print();
    ir_type *clone() const { return (new ir_usertype(*this)); }
    std::string str() const { return m_name; }
    llvm::Type *codegen(ir &ref);

    std::string m_name;
    shared_ir_type m_type;
};

typedef std::unique_ptr<ir_usertype> ptr_ir_usertype;

struct ir_ref : public ir_type {
    ir_ref() { m_irtype = IRTYPE_REF; }

    void print();
    ir_type *clone() const { return (new ir_ref(*this)); }
    std::string str() const;
    llvm::Type *codegen(ir &ref);

    shared_ir_type m_type;
};

typedef std::unique_ptr<ir_ref> ptr_ir_ref;

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
    llvm::Function *mkproto(ir &ref);
    llvm::Function *codegen(ir &ref);

    std::shared_ptr<ir_funtype> m_funtype;

    std::string m_name;
    ptr_ir_type m_ret;
    std::vector<ptr_ir_id> m_args;
    ptr_ir_expr m_expr;
    llvm::Function *m_fun;

    void resolve_funtype();
};

typedef std::unique_ptr<ir_defun> ptr_ir_defun;

struct ir_expr : public ir_ast {
    ir_expr() : m_expr_type(EXPRVAL) {}
    virtual ~ir_expr() {}

    enum EXPRTYPE {
        EXPRNOP,
        EXPRID,
        EXPRVAL,
        EXPRAPPLY,
        EXPRDECIMAL,
        EXPRBOOL,
        EXPRLET
    };

    typedef std::unordered_map<std::string, std::deque<shared_ir_type>> id2type;
    typedef std::unordered_map<std::string, std::deque<llvm::Value *>> id2val;

    EXPRTYPE m_expr_type;

    virtual shared_ir_type check_type(const ir &ref, id2type &vars) = 0;
    virtual llvm::Value *codegen(ir &ref, id2val &vals) = 0;

    std::shared_ptr<ir_type> m_type;
};

struct ir_id : public ir_expr {
    ir_id() { m_expr_type = EXPRID; }

    std::string m_id;

    shared_ir_type check_type(const ir &ref, id2type &vars);
    void print() { std::cout << "{\"id\":\"" << m_id << "\"}"; }

    llvm::Value *codegen(ir &ref, id2val &vals);
};

struct ir_apply : public ir_expr {
    ir_apply() { m_expr_type = EXPRAPPLY; }
    virtual ~ir_apply() {}

    shared_ir_type check_type(const ir &ref, id2type &vars);
    llvm::Value *codegen(ir &ref, id2val &vals);
    void print();

    std::vector<ptr_ir_expr> m_expr;

  private:
    shared_ir_type check_ifexpr(const ir &ref, id2type &vars);
    shared_ir_type check_magnitude(const ir &ref, id2type &vars,
                                   const std::string &id);
    shared_ir_type check_eq(const ir &ref, id2type &vars);
    shared_ir_type check_ref(const ir &ref, id2type &vars);
    shared_ir_type check_call(const ir &ref, id2type &vars,
                              const std::string &id);
    llvm::Value *struct_gen(ir &ref, id2val vals, llvm::StructType *type);
    void struct_gen2(ir &ref, id2val vals, llvm::StructType *type,
                     std::vector<ptr_ir_expr> &exprs, llvm::Value *gep);
    llvm::Value *codegen_ifexpr(ir &ref, id2val vals);
    llvm::Value *codegen_call(ir &ref, id2val vals, const std::string &id);
};

typedef std::unique_ptr<ir_apply> ptr_ir_apply;

struct ir_decimal : public ir_expr {
    ir_decimal() { m_expr_type = EXPRDECIMAL; }
    virtual ~ir_decimal() {}

    shared_ir_type check_type(const ir &ref, id2type &vars);
    llvm::Value *codegen(ir &ref, id2val &vals);
    void print();

    std::string m_num;
};

typedef std::unique_ptr<ir_decimal> ptr_ir_decimal;

struct ir_bool : public ir_expr {
    ir_bool() { m_expr_type = EXPRBOOL; }
    virtual ~ir_bool() {}

    shared_ir_type check_type(const ir &ref, id2type &vars);
    llvm::Value *codegen(ir &ref, id2val &vals);
    void print();

    bool m_bool;
};

typedef std::unique_ptr<ir_bool> ptr_ir_bool;

struct ir_let : public ir_expr {
    ir_let() { m_expr_type = EXPRLET; }
    virtual ~ir_let() {}

    struct var {
        ptr_ir_id m_id;
        ptr_ir_expr m_expr;
    };

    shared_ir_type check_type(const ir &ref, id2type &vars);
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
    shared_ir_type resolve_type(shared_ir_type type) const;
    std::string codegen();
    void print();
    void print_err(std::size_t line, std::size_t column) const;

    llvm::LLVMContext &get_llvm_ctx() { return m_llvm_ctx; }
    llvm::Module &get_llvm_module() { return m_llvm_module; }
    llvm::IRBuilder<> &get_llvm_builder() { return m_llvm_builder; }
    llvm::DataLayout &get_llvm_datalayout() { return m_llvm_datalayout; }
    llvm::Function *get_llvm_memcpy() { return m_memcpy; }

    std::string get_filename() const { return m_filename; }
    const std::unordered_map<std::string, std::shared_ptr<ir_funtype>> &
    get_funs() const {
        return m_id2fun;
    }
    const std::unordered_map<std::string, ptr_ir_struct> &
    get_id2struct() const {
        return m_id2struct;
    }
    const std::unordered_map<std::string, llvm::StructType *> &
    get_struct_proto() const {
        return m_struct_prot;
    }
    llvm::Function *get_function(const std::string &name) {
        auto it = m_funs_prot.find(name);
        if (it == m_funs_prot.end())
            return nullptr;

        return it->second;
    }
    bool is_structgen(ir_expr *expr);
    void llvm_memcpy(llvm::Value *dst, llvm::Value *src, size_t size);

  private:
    parsec m_parsec;
    std::string m_filename;
    std::unordered_set<char> m_no_id_char_head;
    std::unordered_set<char> m_no_id_char;
    std::unordered_set<char> m_0to9;
    std::unordered_set<char> m_1to9;
    std::unordered_map<std::string, std::shared_ptr<ir_funtype>> m_id2fun;
    std::unordered_map<std::string, ptr_ir_struct> m_id2struct;
    std::unordered_map<std::string, llvm::Function *> m_funs_prot;
    std::unordered_map<std::string, llvm::StructType *> m_struct_prot;
    llvm::LLVMContext m_llvm_ctx;
    llvm::IRBuilder<> m_llvm_builder;
    llvm::Module m_llvm_module;
    llvm::DataLayout m_llvm_datalayout;
    llvm::Function *m_memcpy;
    std::vector<ptr_ir_defun> m_defuns;
    std::vector<ptr_ir_struct> m_struct;

    ptr_ir_expr parse_expr();
    ptr_ir_struct parse_defstruct();
    ptr_ir_defun parse_defun();
    ptr_ir_type parse_type();
    ptr_ir_struct parse_struct();
    ptr_ir_type parse_ref();
    ptr_ir_decimal parse_decimal();
    ptr_ir_let parse_let();
    bool check_recursive(ir_struct *p, std::unordered_set<std::string> &used);
    std::string parse_id();
};
} // namespace lunar

#endif // LUNAR_IR_HPP