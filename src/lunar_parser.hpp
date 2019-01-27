#ifndef LUNAR_PARSER_HPP
#define LUNAR_PARSER_HPP

#include "lunar_parsec.hpp"

#include <string>

namespace lunar {

struct ast {
    ast() : m_line(0), m_column(0) {}
    virtual ~ast(){};

    std::size_t m_line;
    std::size_t m_column;
};

struct ast_id : public ast {
    std::string m_id;
};

struct ast_interface : public ast {};

struct ast_class : public ast {};

struct ast_inst : public ast {};

class lang {
  public:
    lang(const std::string &filename, const std::string &str);
    virtual ~lang() {}

  private:
    parsec m_parsec;
    const std::string m_filename;
};

} // namespace lunar

#endif // LUNAR_PARSER_HPP