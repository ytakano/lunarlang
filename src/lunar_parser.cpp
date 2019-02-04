#include "lunar_parser.hpp"
#include "lunar_print.hpp"

#define SYNTAXERR(M)                                                           \
    do {                                                                       \
        fprintf(stderr, "%s:%lu:%lu:(%d) syntax error: " M "\n",               \
                m_filename.c_str(), m_parsec.get_line(),                       \
                m_parsec.get_column(), __LINE__);                              \
        print_err(m_parsec.get_line(), m_parsec.get_column(),                  \
                  m_parsec.get_str());                                         \
    } while (0)

#define SPACEPLUS(PARSEC)                                                      \
    do {                                                                       \
        (PARSEC).space();                                                      \
        if ((PARSEC).is_fail()) {                                              \
            SYNTAXERR("expected whitespaces");                                 \
            return nullptr;                                                    \
        }                                                                      \
        (PARSEC).spaces();                                                     \
    } while (0)

#define PARSEID(ID, PARSEC)                                                    \
    do {                                                                       \
        (ID) = parse_id();                                                     \
        if ((PARSEC).is_fail()) {                                              \
            SYNTAXERR("expected an identifier");                               \
            return nullptr;                                                    \
        }                                                                      \
    } while (0)

#define PARSETVAR(ID, PARSEC)                                                  \
    do {                                                                       \
        (ID) = parse_tvar();                                                   \
        if ((PARSEC).is_fail()) {                                              \
            SYNTAXERR("expected a type variable");                             \
            return nullptr;                                                    \
        }                                                                      \
    } while (0)

#define PEEK(C, PARSEC)                                                        \
    do {                                                                       \
        C = (PARSEC).peek();                                                   \
        if (m_parsec.is_fail()) {                                              \
            SYNTAXERR("unexpected EOF");                                       \
            return nullptr;                                                    \
        }                                                                      \
    } while (0);

namespace lunar {

parser::parser() {
    m_no_id_char.insert(' ');
    m_no_id_char.insert('\r');
    m_no_id_char.insert('\n');
    m_no_id_char.insert('\t');
    m_no_id_char.insert('@');
    m_no_id_char.insert('#');
    m_no_id_char.insert('$');
    m_no_id_char.insert('(');
    m_no_id_char.insert(')');
    m_no_id_char.insert('{');
    m_no_id_char.insert('}');
    m_no_id_char.insert('[');
    m_no_id_char.insert(']');
    m_no_id_char.insert(':');
    m_no_id_char.insert(';');
    m_no_id_char.insert('\'');
    m_no_id_char.insert('"');
    m_no_id_char.insert(',');
    m_no_id_char.insert('.');
    m_no_id_char.insert('?');
    m_no_id_char.insert('`');
    m_no_id_char.insert('~');
    m_no_id_char.insert('<');
    m_no_id_char.insert('>');
    m_no_id_char.insert('+');
    m_no_id_char.insert('-');
    m_no_id_char.insert('*');
    m_no_id_char.insert('|');
    m_no_id_char.insert('\\');
    m_no_id_char.insert('/');
    m_no_id_char.insert('%');
    m_no_id_char.insert('^');
    m_no_id_char.insert('&');
    m_no_id_char.insert('=');
}

bool module::parse() {
    for (;;) {
        m_parsec.spaces();
        if (m_parsec.is_eof())
            break;

        auto id = parse_id();
        if (!id) {
            SYNTAXERR("unexpected character");
            return false;
        }

        if (id->m_id == "fn") {

        } else if (id->m_id == "inst") {

        } else if (id->m_id == "class") {
        }
    }

    return true;
}

ptr_ast_id module::parse_id() {
    auto ret = std::make_unique<ast_id>();

    ret->set_pos(m_parsec);

    PMANYONE(m_parsec, ret->m_id, m_parsec.oneof_not(m_parser.m_no_id_char));

    if (m_parsec.is_fail())
        return nullptr;

    return ret;
}

ptr_ast_id module::parse_tvar() {
    auto ret = std::make_unique<ast_id>();

    ret->set_pos(m_parsec);

    m_parsec.character('`');
    ret->m_id.push_back('`');

    PMANYONE(m_parsec, ret->m_id, m_parsec.oneof_not(m_parser.m_no_id_char));

    if (m_parsec.is_fail())
        return nullptr;

    return ret;
}

ptr_ast_kind module::parse_kind() {
    std::deque<ptr_ast_kind> ks;

    auto k = std::make_unique<ast_kstar>();
    k->set_pos(m_parsec);

    char c;
    m_parsec.character('*');
    if (m_parsec.is_fail()) {
        SYNTAXERR("expected *");
        return nullptr;
    }

    ks.push_back(std::move(k));

    for (;;) {
        m_parsec.spaces();

        std::string ar;
        PTRY(m_parsec, ar, m_parsec.str("->"));
        if (m_parsec.is_fail())
            break;

        m_parsec.spaces();

        auto k = std::make_unique<ast_kstar>();
        k->set_pos(m_parsec);

        m_parsec.character('*');
        if (m_parsec.is_fail()) {
            SYNTAXERR("expected *");
            return nullptr;
        }

        ks.push_back(std::move(k));
    }

    for (;;) {
        auto rhs = std::move(ks.back());
        ks.pop_back();
        if (ks.empty())
            return rhs;

        auto lhs = std::move(ks.back());

        auto kfun = std::make_unique<ast_kfun>();
        kfun->m_left = std::move(lhs);
        kfun->m_right = std::move(rhs);

        ks.push_back(std::move(kfun));
    }

    return nullptr; // not reach here
}

ptr_ast_tvars module::parse_tvars() {
    auto tvars = std::make_unique<ast_tvars>();

    tvars->set_pos(m_parsec);

    m_parsec.character('<');
    if (m_parsec.is_fail()) {
        SYNTAXERR("expected <");
        return nullptr;
    }

    m_parsec.spaces();

    for (;;) {
        ast_tvars::ptr_arg arg;
        PARSETVAR(arg->m_id, m_parsec);

        m_parsec.spaces();

        char c;
        PEEK(c, m_parsec);

        if (c == ':') {
            m_parsec.str("::");
            if (m_parsec.is_fail()) {
                SYNTAXERR("expected ::");
                return nullptr;
            }
            m_parsec.spaces();

            arg->m_kind = parse_kind();
            if (!arg->m_kind)
                return nullptr;

            m_parsec.spaces();
        }

        tvars->m_args.push_back(std::move(arg));

        PEEK(c, m_parsec);

        if (c == '>')
            break;

        m_parsec.character(',');
        if (m_parsec.is_fail()) {
            SYNTAXERR("expected ,");
            return nullptr;
        }
        m_parsec.spaces();
    }

    m_parsec.character('>');
    return tvars;
}

ptr_ast_class module::parse_class() {
    SPACEPLUS(m_parsec);

    auto cls = std::make_unique<ast_class>();

    PARSEID(cls->m_id, m_parsec);

    m_parsec.spaces();

    cls->m_tvars = parse_tvars();
    if (!cls->m_tvars)
        return nullptr;

    return cls;
}

} // namespace lunar