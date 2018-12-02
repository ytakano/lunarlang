#include "lunar_ir.hpp"

namespace lunar {

ir::ir(const std::string &str) : m_parsec(str) {

    m_no_id_char.insert(' ');
    m_no_id_char.insert('\r');
    m_no_id_char.insert('\n');
    m_no_id_char.insert('\t');
    m_no_id_char.insert('!');
    m_no_id_char.insert('@');
    m_no_id_char.insert('#');
    m_no_id_char.insert('$');
    m_no_id_char.insert('%');
    m_no_id_char.insert('^');
    m_no_id_char.insert('&');
    m_no_id_char.insert('*');
    m_no_id_char.insert('(');
    m_no_id_char.insert(')');
    m_no_id_char.insert('-');
    m_no_id_char.insert('+');
    m_no_id_char.insert('{');
    m_no_id_char.insert('}');
    m_no_id_char.insert('[');
    m_no_id_char.insert(']');
    m_no_id_char.insert('|');
    m_no_id_char.insert('\\');
    m_no_id_char.insert(':');
    m_no_id_char.insert(';');
    m_no_id_char.insert('\'');
    m_no_id_char.insert('"');
    m_no_id_char.insert('<');
    m_no_id_char.insert('>');
    m_no_id_char.insert(',');
    m_no_id_char.insert('.');
    m_no_id_char.insert('?');
    m_no_id_char.insert('/');
    m_no_id_char.insert('`');
    m_no_id_char.insert('~');

    m_no_id_char_head = m_no_id_char;

    m_no_id_char_head.insert('0');
    m_no_id_char_head.insert('1');
    m_no_id_char_head.insert('2');
    m_no_id_char_head.insert('3');
    m_no_id_char_head.insert('4');
    m_no_id_char_head.insert('5');
    m_no_id_char_head.insert('6');
    m_no_id_char_head.insert('7');
    m_no_id_char_head.insert('8');
    m_no_id_char_head.insert('9');
}

std::unique_ptr<ir_expr> ir::parse() {
    std::unique_ptr<ir_expr> expr(new ir_expr);

    m_parsec.spaces();
    m_parsec.character('(');
    if (m_parsec.is_fail()) {
        return nullptr;
    }

    m_parsec.spaces();
    expr->m_name = parse_id();
    if (m_parsec.is_fail())
        return nullptr;

    m_parsec.spaces();

    return expr;
}

std::unique_ptr<ir_defun> ir::parse_defun() {
    std::unique_ptr<ir_defun> defun(new ir_defun);
    auto id = parse_id();

    return defun;
}

std::string ir::parse_id() {
    std::string ret;

    char c = m_parsec.oneof_not(m_no_id_char_head);
    if (m_parsec.is_fail())
        return ret;

    ret.push_back(c);

    PMANY(m_parsec, ret, m_parsec.oneof_not(m_no_id_char));

    return ret;
}

} // namespace lunar