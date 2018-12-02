#include "lunar_ir.hpp"

#include <iostream>

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

ptr_ir_expr ir::parse() { return parse_expr(); }

ptr_ir_expr ir::parse_expr() {
    ptr_ir_expr expr;

    m_parsec.spaces();
    m_parsec.character('(');
    if (m_parsec.is_fail()) {
        // print
        return nullptr;
    }

    m_parsec.spaces();
    auto id = parse_id();
    if (m_parsec.is_fail()) {
        // print
        return nullptr;
    }

    if (id == "defun") {
        expr = parse_defun();
    } else {
    }

    return expr;
}

ptr_ir_type ir::parse_type() {
    std::string s = parse_id();
    if (m_parsec.is_fail())
        return nullptr;

    if (s == "u64") {
        auto t = new ir_scalar;
        t->m_type = TYPE_U64;
        return ptr_ir_type((ir_type *)t);
    } else if (s == "u32") {
        auto t = new ir_scalar;
        t->m_type = TYPE_U32;
        return ptr_ir_type((ir_type *)t);
    } else if (s == "bool") {
        auto t = new ir_scalar;
        t->m_type = TYPE_BOOL;
        return ptr_ir_type((ir_type *)t);
    }

    return nullptr;
}

ptr_ir_defun ir::parse_defun() {
    m_parsec.space();
    if (m_parsec.is_fail()) {
        // print
        return nullptr;
    }

    // parse function name
    ptr_ir_defun defun(new ir_defun);
    auto id = parse_id();
    if (m_parsec.is_fail())
        return nullptr;

    defun->m_name = id;

    m_parsec.spaces();

    // parse return types
    m_parsec.character('(');
    if (m_parsec.is_fail()) {
        // print
        return nullptr;
    }

    m_parsec.spaces();

    auto t = parse_type();
    if (m_parsec.is_fail()) {
        return nullptr;
    }
    defun->m_ret.push_back(std::move(t));

    for (;;) {
        char tmp;
        m_parsec.spaces();

        PTRY(m_parsec, tmp, m_parsec.character(','));
        if (m_parsec.is_fail())
            break;

        m_parsec.spaces();
        auto t2 = parse_type();
        if (m_parsec.is_fail()) {
            return nullptr;
        }

        defun->m_ret.push_back(std::move(t2));
    }

    m_parsec.spaces();
    m_parsec.character(')');
    if (m_parsec.is_fail()) {
        // print
        return nullptr;
    }

    return defun;
}

std::string ir::parse_id() {
    std::string ret;

    char c = m_parsec.oneof_not(m_no_id_char_head);
    if (m_parsec.is_fail()) {
        // print
        return ret;
    }

    ret.push_back(c);
    PMANY(m_parsec, ret, m_parsec.oneof_not(m_no_id_char));

    return ret;
}

} // namespace lunar