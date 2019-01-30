#ifndef LUNAR_PARSER_HPP
#define LUNAR_PARSER_HPP

#include "lunar_parsec.hpp"

#include <string>
#include <unordered_map>
#include <unordered_set>

namespace lunar {

struct ast {
    ast() : m_line(0), m_column(0) {}
    virtual ~ast(){};

    void set_pos(const parsec &p) {
        m_line = p.get_line();
        m_column = p.get_column();
    }

    std::size_t m_line;
    std::size_t m_column;
};

struct ast_id : public ast {
    std::string m_id;
};

typedef std::unique_ptr<ast_id> ptr_ast_id;

struct ast_interface : public ast {};

struct ast_class : public ast {
  public:
    ast_class() {}
    virtual ~ast_class() {}
};

struct ast_inst : public ast {};

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
    ptr_ast_id parse_id();
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