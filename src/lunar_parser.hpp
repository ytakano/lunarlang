#ifndef LUNAR_PARSER_HPP
#define LUNAR_PARSER_HPP

#include "lunar_parsec.hpp"

#include <string>
#include <unordered_map>
#include <unordered_set>
#include <vector>

namespace lunar {

struct ast {
    ast() : m_line(0), m_column(0) {}
    virtual ~ast(){};

    void set_pos(const parsec &p) {
        m_line = p.get_line();
        m_column = p.get_column();
    }

    enum asttype {
        AST_ID,
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
    };

    std::size_t m_line;
    std::size_t m_column;
    asttype m_asttype;
};

struct ast_id : public ast {
    ast_id() { m_asttype = AST_ID; }
    virtual ~ast_id() {}
    std::string m_id;
};

typedef std::unique_ptr<ast_id> ptr_ast_id;

struct ast_kind : public ast {
    ast_kind() {}
    virtual ~ast_kind() {}
};

typedef std::unique_ptr<ast_kind> ptr_ast_kind;

struct ast_kfun : public ast_kind {
    ast_kfun() { m_asttype = AST_KFUN; }
    virtual ~ast_kfun() {}

    ptr_ast_kind m_left;
    ptr_ast_kind m_right;
};

typedef std::unique_ptr<ast_kfun> ptr_ast_kfun;

struct ast_kstar : public ast_kind {
    ast_kstar() { m_asttype = AST_KSTAR; }
    virtual ~ast_kstar() {}
};

typedef std::unique_ptr<ast_kstar> ptr_ast_kstar;

struct ast_interface;

struct ast_tvars : public ast {
    ast_tvars() {}
    virtual ~ast_tvars() {}

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
    enum type {
        TYPE_NORMAL,
        TYPE_FUN,
        TYPE_TUPLE,
    };

    type m_type;
};

typedef std::unique_ptr<ast_type> ptr_ast_type;

struct ast_normaltype : public ast_type {
    ast_normaltype() { m_type = TYPE_NORMAL; }
    virtual ~ast_normaltype() {}

    ptr_ast_id m_id;
    ptr_ast_types m_args;
};

typedef std::unique_ptr<ast_normaltype> ptr_ast_normaltype;

struct ast_funtype : public ast_type {
    ast_funtype() { m_type = TYPE_FUN; }
    virtual ~ast_funtype() {}

    ptr_ast_types m_args;
    ptr_ast_type m_ret;
};

struct ast_tupletype : public ast_type {
    ast_tupletype() { m_type = TYPE_TUPLE; }
    virtual ~ast_tupletype() {}

    ptr_ast_types m_types;
};

struct ast_types : public ast {
    ast_types() { m_asttype = AST_TYPES; }
    virtual ~ast_types() {}

    std::vector<ptr_ast_type> m_types;
};

struct ast_pred : public ast {
    ast_pred() { m_asttype = AST_PRED; }
    virtual ~ast_pred() {}

    ptr_ast_id m_id;
    ptr_ast_types m_args;
};

typedef std::unique_ptr<ast_pred> ptr_ast_pred;

struct ast_preds : public ast {
    ast_preds() { m_asttype = AST_PREDS; }
    virtual ~ast_preds() {}

    std::vector<ptr_ast_pred> m_preds;
};

typedef std::unique_ptr<ast_preds> ptr_ast_preds;

struct ast_infix : public ast {
    ast_infix() { m_asttype = AST_INFIX; }
    virtual ~ast_infix() {}

    std::string m_infix;
};

typedef std::unique_ptr<ast_infix> ptr_ast_infix;

struct ast_interface : public ast {
    ast_interface() { m_asttype = AST_INTERFACE; }
    virtual ~ast_interface() {}

    ptr_ast_id m_id;
    ptr_ast_types m_args;
    ptr_ast_type m_ret;
    ptr_ast_infix m_infix;
};

typedef std::unique_ptr<ast_interface> ptr_ast_interface;

struct ast_interfaces : public ast {
    ast_interfaces() { m_asttype = AST_INTERFACES; }
    virtual ~ast_interfaces() {}

    std::vector<ptr_ast_interface> m_interfaces;
};

class parser;

class module {
  public:
    module(const std::string &filename, const std::string &str, parser &p)
        : m_parsec(str), m_filename(filename), m_parser(p) {}
    virtual ~module() {}

    bool parse();

  private:
    parsec m_parsec;
    const std::string m_filename;
    parser &m_parser;
    std::unordered_map<std::string, ptr_ast_class> m_id2class;

    ptr_ast_class parse_class();
    ptr_ast_id parse_id();
    ptr_ast_id parse_tvar();
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
    void parse_spaces();
    bool parse_spaces_plus();
    bool parse_sep();
};

typedef std::unique_ptr<module> ptr_module;

class parser {
  public:
    parser();
    virtual ~parser() {}

    bool add_module(const char *filename);
    bool compile();

  private:
    std::unordered_map<std::string, ptr_module> m_modules;
    std::unordered_set<char> m_no_id_char;
    std::unordered_set<char> m_wsp2;
    std::unordered_set<char> m_wsp3;
    std::unordered_set<char> m_newline;
    std::unordered_set<char> m_newline_sc;
    std::unordered_set<char> m_infix;

    friend class module;
};

} // namespace lunar

#endif // LUNAR_PARSER_HPP