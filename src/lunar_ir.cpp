#include "lunar_ir.hpp"

#include <iostream>

#define SYNTAXERR(M, ...)                                                      \
    fprintf(stderr, "syntax error (%s:%lu:%lu): " M "\n", m_filename.c_str(),  \
            m_parsec.get_line(), m_parsec.get_column(), ##__VA_ARGS__)

namespace lunar {

ir::ir(const std::string &filename, const std::string &str)
    : m_filename(filename), m_parsec(str) {

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

bool ir::parse(std::list<ptr_ir_defun> &defuns) {
    for (;;) {
        m_parsec.spaces();
        char c;
        PTRY(m_parsec, c, m_parsec.character('('));
        if (m_parsec.is_fail()) {
            if (m_parsec.is_eof()) {
                return true;
            } else {
                SYNTAXERR("expected ( or EOF");
            }

            return false;
        }

        m_parsec.spaces();
        auto id = parse_id();
        if (m_parsec.is_fail()) {
            SYNTAXERR("expected identifier");
            return false;
        }

        if (id == "defun") {
            auto defun = parse_defun();
            if (!defun)
                return false;

            defuns.push_back(std::move(defun));
        } else {
            SYNTAXERR("expected defun");
            return false;
        }

        m_parsec.spaces();
        m_parsec.character(')');
        if (m_parsec.is_fail()) {
            SYNTAXERR("expected )");
            return false;
        }
    }

    return false; // never reach here
}

ptr_ir_expr ir::parse_expr() {
    std::string id;
    PTRY(m_parsec, id, parse_id());
    if (m_parsec.is_fail()) {
        // APPLY
    } else {
        if (id == "let") {
            // LET
        } else {
            // IDENTIFIER
            auto e = std::make_unique<ir_id>();
            e->m_id = id;
            return e;
        }
    }

    return nullptr;
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
        SYNTAXERR("expected (");
        return nullptr;
    }

    // parse function name
    ptr_ir_defun defun(new ir_defun);
    auto id = parse_id();
    if (m_parsec.is_fail()) {
        SYNTAXERR("expected identifier");
        return nullptr;
    }

    defun->m_name = id;

    m_parsec.space();
    if (m_parsec.is_fail()) {
        SYNTAXERR("expected space");
        return nullptr;
    }

    m_parsec.spaces();

    // parse return types
    m_parsec.character('(');
    if (m_parsec.is_fail()) {
        // print
        SYNTAXERR("expected (");
        return nullptr;
    }

    m_parsec.spaces();

    auto t = parse_type();
    if (m_parsec.is_fail()) {
        SYNTAXERR("expected type or identifier");
        return nullptr;
    }
    defun->m_ret.push_back(std::move(t));

    for (;;) {
        char tmp;
        m_parsec.spaces();
        PTRY(m_parsec, tmp, m_parsec.character(')'));
        if (!m_parsec.is_fail())
            break;

        auto t2 = parse_type();
        if (m_parsec.is_fail()) {
            SYNTAXERR("expected type or identifier");
            return nullptr;
        }

        defun->m_ret.push_back(std::move(t2));
    }

    m_parsec.space();
    if (m_parsec.is_fail()) {
        SYNTAXERR("expected whitespace");
        return nullptr;
    }
    m_parsec.spaces();

    // parse arguments
    m_parsec.character('(');
    if (m_parsec.is_fail()) {
        SYNTAXERR("expected (");
        return nullptr;
    }

    for (;;) {
        m_parsec.spaces();
        char tmp;
        PTRY(m_parsec, tmp, m_parsec.character(')'));
        if (m_parsec.is_fail()) {
            m_parsec.character('(');
            if (m_parsec.is_fail()) {
                SYNTAXERR("expected (");
                return nullptr;
            }

            m_parsec.spaces();

            auto t = parse_type();
            if (!t) {
                SYNTAXERR("expected type");
                return nullptr;
            }

            m_parsec.space();
            if (m_parsec.is_fail()) {
                SYNTAXERR("expected whitespace");
                return nullptr;
            }

            auto id = parse_id();
            if (m_parsec.is_fail()) {
                SYNTAXERR("expected identifier");
                return nullptr;
            }

            auto p = std::make_unique<std::pair<ptr_ir_type, std::string>>(
                std::move(t), id);
            defun->m_args.push_back(std::move(p));

            m_parsec.spaces();
            m_parsec.character(')');
            if (m_parsec.is_fail()) {
                SYNTAXERR("expected )");
                return nullptr;
            }
        } else {
            break;
        }
    }

    m_parsec.space();
    if (m_parsec.is_fail()) {
        SYNTAXERR("expected whitespace");
        return nullptr;
    }
    m_parsec.spaces();

    auto expr = parse_expr();
    if (!expr)
        return nullptr;

    defun->m_expr = std::move(expr);

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

void ir_scalar::print() {
    std::cout << "\"";
    switch (m_type) {
    case TYPE_BOOL:
        std::cout << "bool";
        break;
    case TYPE_U32:
        std::cout << "u32";
        break;
    case TYPE_U64:
        std::cout << "u64";
        break;
    }
    std::cout << "\"";
}

void ir_defun::print() {
    std::cout << "{\"defun\":{\"id\":\"" << m_name << "\","
              << "\"ret\":[";
    auto n = m_ret.size();
    for (auto &p : m_ret) {
        p->print();
        n--;
        if (n > 0) {
            std::cout << ",";
        }
    }
    std::cout << "],\"args\":[";
    n = m_args.size();
    for (auto &q : m_args) {
        std::cout << "{\"type\":";
        q->first->print();
        std::cout << ",\"id\":\"" << q->second << "\"}";
        n--;
        if (n > 0) {
            std::cout << ",";
        }
    }
    std::cout << "],\"expr\":";
    m_expr->print();
    std::cout << "}}";
}

} // namespace lunar