#ifndef LUNAR_PARSER_HPP
#define LUNAR_PARSER_HPP

#include "lunar_env.hpp"
#include "lunar_parsec.hpp"
#include "lunar_type.hpp"

#include <memory>
#include <string>
#include <unordered_map>
#include <unordered_set>
#include <vector>

namespace lunar {

struct ast_id;

struct ast {
    ast() : m_line(0), m_column(0) {}
    virtual ~ast(){};

    virtual void print() const = 0;

    void set_pos(const parsec &p) {
        m_line = p.get_line();
        m_column = p.get_column();
    }

    virtual const ast_id *get_ast_id() { return nullptr; }

    enum asttype {
        AST_ID,
        AST_DOTID,
        AST_CLASS,
        AST_KFUN,       // kind
        AST_KSTAR,      // kind
        AST_TVARS,      // arguments of type variable
        AST_PRED,       // predicate
        AST_PREDS,      // predicates
        AST_TYPE,       // type
        AST_TYPES,      // types
        AST_INTERFACE,  // interface
        AST_INTERFACES, // interfaces
        AST_INFIX,      // infix
        AST_DEFUN,      // function definition
        AST_ARG,        // argument
        AST_ARGS,       // arguments
        AST_EXPR,       // expression
        AST_DEFVAR,     // variable definition
        AST_DEFVARS,    // variable definitions
        AST_DICTELM,    // element of dictionary
        AST_INSTANCE,   // instance
        AST_MEMBER,     // variable definition in structure or union
        AST_MEMBERS,    // variable definition in structure or union
        AST_IMPORT,     // import
    };

    std::size_t m_line;
    std::size_t m_column;
    asttype m_asttype;
};

typedef std::unique_ptr<ast> ptr_ast;

struct ast_id : public ast {
    ast_id() { m_asttype = AST_ID; }
    virtual ~ast_id() {}

    virtual const ast_id *get_ast_id() { return this; }

    virtual void print() const;

    std::string m_id;
};

typedef std::unique_ptr<ast_id> ptr_ast_id;

struct ast_dotid : public ast {
    ast_dotid() { m_asttype = AST_DOTID; }
    virtual ~ast_dotid() {}

    virtual void print() const;

    std::string get_id() const {
        std::string ret;
        int n = 0;
        for (auto &s : m_ids) {
            if (n > 0)
                ret += ".";

            ret += s->m_id;
            n++;
        }
        return ret;
    }

    std::vector<ptr_ast_id> m_ids;
};

typedef std::unique_ptr<ast_dotid> ptr_ast_dotid;

struct ast_kind : public ast {
    ast_kind() {}
    virtual ~ast_kind() {}
    virtual void print() const = 0;
};

typedef std::unique_ptr<ast_kind> ptr_ast_kind;

struct ast_kfun : public ast_kind {
    ast_kfun() { m_asttype = AST_KFUN; }
    virtual ~ast_kfun() {}

    virtual void print() const;

    ptr_ast_kind m_left;
    ptr_ast_kind m_right;
};

typedef std::unique_ptr<ast_kfun> ptr_ast_kfun;

struct ast_kstar : public ast_kind {
    ast_kstar() { m_asttype = AST_KSTAR; }
    virtual ~ast_kstar() {}

    virtual void print() const;
};

typedef std::unique_ptr<ast_kstar> ptr_ast_kstar;

struct ast_interface;

struct ast_tvars : public ast {
    ast_tvars() {}
    virtual ~ast_tvars() {}

    virtual void print() const;

    struct arg {
        ptr_ast_id m_id;
        ptr_ast_kind m_kind;
    };

    typedef std::unique_ptr<arg> ptr_arg;

    std::vector<ptr_arg> m_args;
};

typedef std::unique_ptr<ast_tvars> ptr_ast_tvars;

struct ast_preds;
typedef std::unique_ptr<ast_preds> ptr_ast_preds;

struct ast_interfaces;
typedef std::unique_ptr<ast_interfaces> ptr_ast_interfaces;

struct ast_class : public ast {
    ast_class() { m_asttype = AST_CLASS; }
    virtual ~ast_class() {}

    virtual void print() const;
    virtual const ast_id *get_ast_id() { return m_id.get(); }

    ptr_ast_id m_id;
    ptr_ast_tvars m_tvars;
    ptr_ast_preds m_preds;
    ptr_ast_interfaces m_interfaces;
};

typedef std::unique_ptr<ast_class> ptr_ast_class;

struct ast_types;
typedef std::unique_ptr<ast_types> ptr_ast_types;

struct ast_type : public ast {
    ast_type() { m_asttype = AST_TYPE; }
    virtual ~ast_type() {}

    virtual void print() const = 0;

    enum type {
        TYPE_NORMAL,
        TYPE_FUN,
        TYPE_TUPLE,
        TYPE_STRUCT,
        TYPE_UNION,
        TYPE_VEC,
    };

    type m_type;
};

typedef std::unique_ptr<ast_type> ptr_ast_type;

struct ast_normaltype : public ast_type {
    ast_normaltype() { m_type = TYPE_NORMAL; }
    virtual ~ast_normaltype() {}

    virtual void print() const;

    ptr_ast_dotid m_id;
    ptr_ast_id m_tvar; // if this class specifies a type variable, m_tvar is not
                       // nullptr otherwise m_tvar is nullptr
    ptr_ast_types m_args;
};

typedef std::unique_ptr<ast_normaltype> ptr_ast_normaltype;

struct ast_funtype : public ast_type {
    ast_funtype() { m_type = TYPE_FUN; }
    virtual ~ast_funtype() {}

    virtual void print() const;

    ptr_ast_types m_args;
    ptr_ast_type m_ret;
};

struct ast_tupletype : public ast_type {
    ast_tupletype() { m_type = TYPE_TUPLE; }
    virtual ~ast_tupletype() {}

    virtual void print() const;

    ptr_ast_types m_types;
};

struct ast_expr;
typedef std::unique_ptr<ast_expr> ptr_ast_expr;

struct ast_num;
typedef std::unique_ptr<ast_num> ptr_ast_num;

struct ast_vectype : public ast_type {
    ast_vectype() { m_type = TYPE_VEC; }
    virtual ~ast_vectype() {}

    virtual void print() const;

    ptr_ast_type m_vectype;
    std::vector<ptr_ast_num> m_nums;
};

struct ast_types : public ast {
    ast_types() { m_asttype = AST_TYPES; }
    virtual ~ast_types() {}

    virtual void print() const;

    std::vector<ptr_ast_type> m_types;
};

struct ast_pred : public ast {
    ast_pred() { m_asttype = AST_PRED; }
    virtual ~ast_pred() {}

    virtual void print() const;

    ptr_ast_dotid m_id;
    ptr_ast_types m_args;
};

typedef std::unique_ptr<ast_pred> ptr_ast_pred;

struct ast_preds : public ast {
    ast_preds() { m_asttype = AST_PREDS; }
    virtual ~ast_preds() {}

    virtual void print() const;

    std::vector<ptr_ast_pred> m_preds;
};

typedef std::unique_ptr<ast_preds> ptr_ast_preds;

struct ast_infix : public ast {
    ast_infix() { m_asttype = AST_INFIX; }
    virtual ~ast_infix() {}

    virtual void print() const;

    std::string m_infix;
};

typedef std::unique_ptr<ast_infix> ptr_ast_infix;

struct ast_interface : public ast {
    ast_interface() { m_asttype = AST_INTERFACE; }
    virtual ~ast_interface() {}

    virtual void print() const;

    ptr_ast_id m_id;
    ptr_ast_infix m_infix;
    ptr_ast_types m_args;
    ptr_ast_type m_ret;
};

typedef std::unique_ptr<ast_interface> ptr_ast_interface;

struct ast_interfaces : public ast {
    ast_interfaces() { m_asttype = AST_INTERFACES; }
    virtual ~ast_interfaces() {}

    virtual void print() const;

    std::vector<ptr_ast_interface> m_interfaces;
};

struct ast_arg : public ast {
    ast_arg() { m_asttype = AST_ARG; }
    virtual ~ast_arg() {}

    virtual void print() const;

    ptr_ast_id m_id;
    ptr_ast_type m_type;
};

typedef std::unique_ptr<ast_arg> ptr_ast_arg;

struct ast_args : public ast {
    ast_args() { m_asttype = AST_ARGS; }
    virtual ~ast_args() {}

    virtual void print() const;

    std::vector<ptr_ast_arg> m_args;
};

typedef std::unique_ptr<ast_args> ptr_ast_args;

struct ast_exprs;
typedef std::unique_ptr<ast_exprs> ptr_ast_exprs;

struct ast_defun : public ast {
    ast_defun() { m_asttype = AST_DEFUN; }
    virtual ~ast_defun() {}

    virtual void print() const;
    virtual const ast_id *get_ast_id() { return m_id.get(); }

    ptr_ast_id m_id;
    ptr_ast_infix m_infix;
    ptr_ast_args m_args;
    ptr_ast_type m_ret;
    ptr_ast_preds m_preds;
    ptr_ast_exprs m_exprs;
};

typedef std::unique_ptr<ast_defun> ptr_ast_defun;

struct ast_prefix : public ast {
    ast_prefix(char c) : m_prefix(c) {}
    virtual ~ast_prefix() {}

    virtual void print() const;

    char m_prefix;
};

typedef std::unique_ptr<ast_prefix> ptr_ast_prefix;

class type;
typedef std::shared_ptr<type> shared_type;

struct ast_expr : public ast {
    ast_expr() { m_asttype = AST_EXPR; }
    virtual ~ast_expr() {}

    virtual void print() const = 0;

    enum ETYPE {
        EXPR_APPLY,
        EXPR_IF,
        EXPR_LET,
        EXPR_TUPLE,
        EXPR_BLOCK,
        EXPR_INDEX,
        EXPR_BINEXPR,
        EXPR_NUM,
        EXPR_STR,
        EXPR_VECTOR,
        EXPR_DICT,
        EXPR_PARENTHESIS,
        EXPRS,
    };

    ETYPE m_exprtype;
    shared_type m_type; // type of this expression
    ptr_ast_prefix m_prefix;
};

struct ast_exprs : public ast_expr {
    ast_exprs() { m_exprtype = EXPRS; }
    virtual ~ast_exprs() {}

    virtual void print() const;

    std::vector<ptr_ast_expr> m_exprs;
};

struct ast_expr_id : public ast_expr {
    ast_expr_id() {}
    virtual ~ast_expr_id() {}

    virtual void print() const;

    ptr_ast_id m_id;
};

struct ast_apply : public ast_expr {
    ast_apply() { m_exprtype = EXPR_APPLY; }
    virtual ~ast_apply() {}

    virtual void print() const;

    ptr_ast_expr m_func;
    std::vector<ptr_ast_expr> m_args;
};

typedef std::unique_ptr<ast_apply> ptr_ast_apply;

struct ast_if;

typedef std::unique_ptr<ast_if> ptr_ast_if;

struct ast_if : public ast_expr {
    ast_if() { m_exprtype = EXPR_IF; }
    virtual ~ast_if() {}

    virtual void print() const;

    ptr_ast_expr m_cond;
    ptr_ast_exprs m_then;
    ptr_ast_if m_elif;
    ptr_ast_exprs m_else;
};

struct ast_defvar : public ast {
    ast_defvar() { m_asttype = AST_DEFVAR; }
    virtual ~ast_defvar() {}

    virtual void print() const;

    ptr_ast_id m_id;
    ptr_ast_expr m_expr;
    ptr_ast_type m_type;
};

typedef std::unique_ptr<ast_defvar> ptr_ast_defvar;

struct ast_defvars : public ast {
    ast_defvars() { m_asttype = AST_DEFVARS; }
    virtual ~ast_defvars() {}

    virtual void print() const;

    std::vector<ptr_ast_defvar> m_defs;
};

typedef std::unique_ptr<ast_defvars> ptr_ast_defvars;

struct ast_let : public ast_expr {
    ast_let() { m_exprtype = EXPR_LET; }
    virtual ~ast_let() {}

    virtual void print() const;

    ptr_ast_defvars m_defvars;
    ptr_ast_expr m_in;
};

typedef std::unique_ptr<ast_let> ptr_ast_let;

struct ast_tuple : public ast_expr {
    ast_tuple() { m_exprtype = EXPR_TUPLE; }
    virtual ~ast_tuple() {}

    virtual void print() const;

    std::vector<ptr_ast_expr> m_exprs;
};

typedef std::unique_ptr<ast_tuple> ptr_ast_tuple;

struct ast_vector : public ast_expr {
    ast_vector() { m_exprtype = EXPR_VECTOR; }
    virtual ~ast_vector() {}

    virtual void print() const;

    std::vector<ptr_ast_expr> m_exprs;
};

typedef std::unique_ptr<ast_vector> ptr_ast_vector;

struct ast_dictelm : public ast {
    ast_dictelm() { m_asttype = AST_DICTELM; }
    virtual ~ast_dictelm() {}

    virtual void print() const;

    ptr_ast_expr m_key;
    ptr_ast_expr m_val;
};

typedef std::unique_ptr<ast_dictelm> ptr_ast_dictelm;

struct ast_dict : public ast_expr {
    ast_dict() { m_exprtype = EXPR_DICT; }
    virtual ~ast_dict() {}

    virtual void print() const;

    std::vector<ptr_ast_dictelm> m_elms;
};

typedef std::unique_ptr<ast_dict> ptr_ast_dict;

struct ast_block : public ast_expr {
    ast_block() { m_exprtype = EXPR_BLOCK; }
    virtual ~ast_block() {}

    virtual void print() const;

    std::vector<ptr_ast_expr> m_exprs;
};

typedef std::unique_ptr<ast_block> ptr_ast_block;

struct ast_index : public ast_expr {
    ast_index() { m_exprtype = EXPR_INDEX; }
    virtual ~ast_index() {}

    virtual void print() const;

    ptr_ast_expr m_array;
    ptr_ast_expr m_index;
};

typedef std::unique_ptr<ast_index> ptr_ast_index;

struct ast_binexpr : public ast_expr {
    ast_binexpr() { m_exprtype = EXPR_BINEXPR; }
    virtual ~ast_binexpr() {}

    virtual void print() const;

    ptr_ast_infix m_op;
    ptr_ast_expr m_left;
    ptr_ast_expr m_right;
};

typedef std::unique_ptr<ast_binexpr> ptr_ast_binexpr;

struct ast_num : public ast_expr {
    ast_num() { m_exprtype = EXPR_NUM; }
    virtual ~ast_num() {}

    virtual void print() const;

    parsec::numtype m_numtype;
    std::string m_num;
};

struct ast_str : public ast_expr {
    ast_str() { m_exprtype = EXPR_STR; }
    virtual ~ast_str() {}

    virtual void print() const;

    std::string m_str;
};

typedef std::unique_ptr<ast_str> ptr_ast_str;

struct ast_parenthesis : public ast_expr {
    ast_parenthesis() { m_exprtype = EXPR_PARENTHESIS; }
    virtual ~ast_parenthesis() {}

    virtual void print() const;

    ptr_ast_expr m_expr;
};

typedef std::unique_ptr<ast_parenthesis> ptr_ast_parenthesis;

struct ast_instance : public ast {
    ast_instance() { m_asttype = AST_INSTANCE; }
    virtual ~ast_instance() {}

    virtual void print() const;

    // e.g.
    // instance Ord<Maybe<`t>> implies Ord<`t>
    //   m_arg: Maybe<`t>
    //   m_req: Ord<`t>
    ptr_ast_pred m_arg;  // argument
    ptr_ast_preds m_req; // require
    std::unordered_map<std::string, ptr_ast_defun> m_id2defun;
};

typedef std::unique_ptr<ast_instance> ptr_ast_instance;

struct ast_member : public ast {
    ast_member() { m_asttype = AST_MEMBER; }
    virtual ~ast_member() {}

    virtual void print() const;
    virtual const ast_id *get_ast_id() { return m_id.get(); }

    ptr_ast_id m_id;
    ptr_ast_type m_type;
};

typedef std::unique_ptr<ast_member> ptr_ast_member;

struct ast_members : public ast {
    ast_members() { m_asttype = AST_MEMBERS; }
    virtual ~ast_members() {}

    virtual void print() const;

    std::vector<ptr_ast_member> m_vars;
};

typedef std::unique_ptr<ast_members> ptr_ast_members;

struct ast_struct : public ast_type {
    ast_struct() { m_type = TYPE_STRUCT; }
    virtual ~ast_struct() {}

    virtual void print() const;
    virtual const ast_id *get_ast_id() { return m_id.get(); }

    ptr_ast_id m_id;
    ptr_ast_tvars m_tvars; // type arguments
    ptr_ast_preds m_preds; // requirements
    ptr_ast_members m_members;
};

typedef std::unique_ptr<ast_struct> ptr_ast_struct;

struct ast_union : public ast_type {
    ast_union() { m_type = TYPE_UNION; }
    virtual ~ast_union() {}

    virtual void print() const;
    virtual const ast_id *get_ast_id() { return m_id.get(); }

    ptr_ast_id m_id;
    ptr_ast_tvars m_tvars; // type arguments
    ptr_ast_preds m_preds; // requirements
    ptr_ast_members m_members;
};

typedef std::unique_ptr<ast_union> ptr_ast_union;

struct ast_import : public ast {
    ast_import() : m_is_here(false) { m_asttype = AST_IMPORT; }
    virtual ~ast_import() {}

    virtual void print() const;

    virtual const ast_id *get_ast_id() {
        if (m_as)
            return m_as.get();

        return nullptr;
    }

    std::string get_id() const { return m_id->get_id(); }

    ptr_ast_dotid m_id;
    ptr_ast_id m_as;
    bool m_is_here;
    std::string m_full_path;
};

typedef std::unique_ptr<ast_import> ptr_ast_import;

class parser;

class typeclass;
typedef std::shared_ptr<typeclass> shared_typeclass;

class inst;
typedef std::shared_ptr<inst> shared_inst;

class module;

class module_tree {
  public:
    module_tree() {}
    virtual ~module_tree() {}

    void add(ptr_ast_import ptr, size_t n = 0);
    // find corresponding import AST
    // if no AST is found, then nullptr is returned
    const ast_import *find(const std::vector<ptr_ast_id> &id,
                           unsigned int &pos) const;

    void print(size_t &n) const;

    std::unordered_map<std::string, std::unique_ptr<module_tree>> m_children;
    ptr_ast_import m_import;
};

class module {
  public:
    module(const std::string &filename, const std::string &str, parser &p);
    virtual ~module() {}

    bool parse();
    void print() const;

    bool is_defined(const std::string &str, ast *ptr);

    // find type class recursively
    bool find_typeclass(const ast_dotid *dotid, std::string &path,
                        std::string &id, unsigned int pos = 0) const;

    // find user defined type recursively
    bool find_type(const ast_dotid *dotid, std::string &path, std::string &id,
                   unsigned int pos = 0) const;

    module *find_module(const ast_dotid *dotid, unsigned int &pos) const;

    const std::string &get_filename() const { return m_filename; }
    const parsec &get_parsec() const { return m_parsec; }

    const std::unordered_map<std::string, ptr_ast_class> &get_classes() const {
        return m_id2class;
    }

    const std::unordered_multimap<std::string, ptr_ast_instance> &
    get_instances() const {
        return m_id2inst;
    }

  private:
    parsec m_parsec;
    const std::string m_filename;
    parser &m_parser;
    lunar_env m_env;
    std::unordered_map<std::string, ptr_ast_class> m_id2class;
    std::unordered_map<std::string, ptr_ast_defun> m_id2defun;
    std::unordered_map<std::string, ptr_ast_struct> m_id2struct;
    std::unordered_map<std::string, ptr_ast_union> m_id2union;
    std::unordered_map<std::string, ast_member *> m_id2union_mem;
    std::unordered_multimap<std::string, ptr_ast_instance> m_id2inst;

    // import module as id
    std::unordered_map<std::string, ptr_ast_import> m_id2import;
    // import module
    // (without as id)
    module_tree m_modules;
    // import module here
    std::vector<ptr_ast_import> m_vec_modules;

    bool m_is_parsed;
    bool m_is_loaded_module;

    ptr_ast_class parse_class();
    ptr_ast_id parse_id();
    ptr_ast_id parse_tvar();
    ptr_ast_dotid parse_dotid();
    ptr_ast_tvars parse_tvars();
    ptr_ast_kind parse_kind();
    ptr_ast_pred parse_pred();
    ptr_ast_preds parse_preds();
    ptr_ast_type parse_type(bool is_funret);
    ptr_ast_types parse_types();
    bool parse_arg_types(ptr_ast_types &types);
    ptr_ast_interface parse_interface();
    ptr_ast_interfaces parse_interfaces();
    ptr_ast_infix parse_infix();
    ptr_ast_defun parse_defun(bool is_infix = false);
    ptr_ast_args parse_args();
    ptr_ast_arg parse_arg();
    ptr_ast_prefix parse_prefix();
    ptr_ast_expr parse_expr0();
    ptr_ast_expr parse_expr();
    ptr_ast_expr parse_exprp(ptr_ast_expr lhs);
    ptr_ast_apply parse_apply(ptr_ast_expr fun);
    ptr_ast_if parse_if();
    ptr_ast_let parse_let();
    ptr_ast_defvar parse_defvar();
    ptr_ast_defvars parse_defvars();
    ptr_ast_exprs parse_exprs();
    ptr_ast_expr parse_parentheses(); // ()
    ptr_ast_vector parse_brackets();  // []
    ptr_ast_expr parse_braces();      // {}
    ptr_ast_str parse_str();
    ptr_ast_num parse_num();
    ptr_ast_infix parse_binop();
    ptr_ast_instance parse_instance();
    ptr_ast_member parse_prod();
    ptr_ast_member parse_sum();
    ptr_ast_members parse_prods();
    ptr_ast_members parse_sums();
    ptr_ast_struct parse_struct();
    ptr_ast_union parse_union();
    ptr_ast_import parse_import();
    bool parse_st_un(const char *str);
    void parse_spaces();
    void parse_spaces_sep();
    bool parse_spaces_plus();
    bool parse_sep();

    friend class parser;
};

typedef std::unique_ptr<module> ptr_module;

class parser {
  public:
    parser();
    virtual ~parser() {}

    bool parse_module(const std::string &str);
    void add_load_path(const char *p);
    bool add_module(const std::string &filename);
    bool parse();
    void print() const;
    int get_pri(const std::string &infix) const {
        auto it = m_op2pri.find(infix);
        if (it != m_op2pri.end())
            return it->second;

        return 0;
    }
    const std::unordered_map<std::string, ptr_module> &get_modules() const {
        return m_modules;
    }

  private:
    std::unordered_map<std::string, ptr_module> m_modules;
    std::unordered_set<char> m_no_id_char;
    std::unordered_set<char> m_wsp2;
    std::unordered_set<char> m_wsp3;
    std::unordered_set<char> m_newline;
    std::unordered_set<char> m_newline_sc;
    std::unordered_set<char> m_prefix;
    std::unordered_set<char> m_infix;

    std::unordered_map<std::string, int> m_op2pri;
    lunar_env m_env;

    bool load_all_module(module *m);
    bool load_module(module *m, ast_import *im);
    bool load_module_tree(module *m, module_tree *tree);

    friend class module;
};

} // namespace lunar

#endif // LUNAR_PARSER_HPP
