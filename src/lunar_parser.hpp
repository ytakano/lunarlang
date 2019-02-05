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
        AST_KFUN,  // kind
        AST_KSTAR, // kind
        AST_TVARS, // arguments of type variable
        AST_PRED,  // predicate
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

struct ast_interface : public ast {};

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

struct ast_class : public ast {
    ast_class() { m_asttype = AST_CLASS; }
    virtual ~ast_class() {}

    ptr_ast_id m_id;
    ptr_ast_tvars m_tvars;
};

typedef std::unique_ptr<ast_class> ptr_ast_class;

struct ast_pred : public ast {
    ast_pred() { m_asttype = AST_PRED; }
    virtual ~ast_pred() {}

    ptr_ast_id m_id;
};

class parser;

class module {
  public:
    module(const std::string &filename, const std::string &str, parser &p)
        : m_parsec(str), m_filename(filename), m_parser(p) {}
    virtual ~module() {}

  private:
    parsec m_parsec;
    const std::string m_filename;
    parser &m_parser;

    bool parse();
    ptr_ast_class parse_class();
    ptr_ast_id parse_id();
    ptr_ast_id parse_tvar();
    ptr_ast_tvars parse_tvars();
    ptr_ast_kind parse_kind();
};

typedef std::unique_ptr<module> ptr_module;

class parser {
  public:
    parser();
    virtual ~parser() {}

  private:
    std::unordered_map<std::string, ptr_module> m_modules;
    std::unordered_set<char> m_no_id_char;

    friend class module;
};

} // namespace lunar

#endif // LUNAR_PARSER_HPP