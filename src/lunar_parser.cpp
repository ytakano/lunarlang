#include "lunar_parser.hpp"
#include "lunar_print.hpp"

#define SYNTAXERR(M, ...)                                                      \
    do {                                                                       \
        fprintf(stderr, "%s:%lu:%lu:(%d) syntax error: " M "\n",               \
                m_filename.c_str(), m_parsec.get_line(),                       \
                m_parsec.get_column(), __LINE__, ##__VA_ARGS__);               \
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

#define PARSECHAR(C, PARSEC)                                                   \
    do {                                                                       \
        (PARSEC).character(C);                                                 \
        if ((PARSEC).is_fail()) {                                              \
            SYNTAXERR("expected a '%c'", C);                                   \
            return nullptr;                                                    \
        }                                                                      \
    } while (0)

#define PARSESTR(STR, PARSEC)                                                  \
    do {                                                                       \
        (PARSEC).str(STR);                                                     \
        if ((PARSEC).is_fail()) {                                              \
            SYNTAXERR("expected \"%s\"", STR);                                 \
            return nullptr;                                                    \
        }                                                                      \
    } while (0)

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

    PARSECHAR('`', m_parsec);
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

    PARSECHAR('*', m_parsec);

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

        PARSECHAR('*', m_parsec);

        ks.push_back(std::move(k));
    }

    // make syntax tree from bottom
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

    PARSECHAR('<', m_parsec);

    m_parsec.spaces();

    for (;;) {
        ast_tvars::ptr_arg arg;
        PARSETVAR(arg->m_id, m_parsec);

        m_parsec.spaces();

        char c;
        PEEK(c, m_parsec);

        if (c == ':') {
            PARSESTR("::", m_parsec);
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

        PARSECHAR(',', m_parsec);
        m_parsec.spaces();
    }

    m_parsec.character('>');
    return tvars;
}

ptr_ast_type module::parse_type() {
    char c;
    PEEK(c, m_parsec);

    auto ret = std::make_unique<ast_type>();
    ret->set_pos(m_parsec);

    if (c == '`') {
        // type variable
        PARSETVAR(ret->m_id, m_parsec);
    } else {
        // identifier
        PARSEID(ret->m_id, m_parsec);
        m_parsec.spaces();

        // arguments
        PTRY(m_parsec, c, m_parsec.character('<'));
        if (!m_parsec.is_fail()) {
            m_parsec.spaces();
            ret->m_args = parse_types();
            if (!ret->m_args)
                return nullptr;

            PARSECHAR('>', m_parsec);
        }
    }

    return ret;
}

ptr_ast_types module::parse_types() {
    auto ret = std::make_unique<ast_types>();
    ret->set_pos(m_parsec);

    for (;;) {
        auto t = parse_type();
        if (!t)
            return nullptr;

        ret->m_types.push_back(std::move(t));

        m_parsec.spaces();
        char c;
        PTRY(m_parsec, c, m_parsec.character(','));
        if (m_parsec.is_fail())
            break;

        m_parsec.spaces();
    }

    return ret;
}

ptr_ast_pred module::parse_pred() {
    auto ret = std::make_unique<ast_pred>();

    ret->set_pos(m_parsec);
    PARSEID(ret->m_id, m_parsec);

    m_parsec.spaces();
    PARSECHAR('<', m_parsec);
    m_parsec.spaces();

    // arguments
    ret->m_args = parse_types();
    if (!ret->m_args)
        return nullptr;

    PARSECHAR('>', m_parsec);

    return ret;
}

ptr_ast_preds module::parse_preds() {
    auto ret = std::make_unique<ast_preds>();

    ret->set_pos(m_parsec);
    PARSESTR("where", m_parsec);
    SPACEPLUS(m_parsec);

    for (;;) {
        auto pred = parse_pred();
        if (!pred)
            return nullptr;

        ret->m_preds.push_back(std::move(pred));
        m_parsec.spaces();

        char c;
        PTRY(m_parsec, c, m_parsec.character(','));
        if (m_parsec.is_fail())
            break;

        m_parsec.spaces();
    }

    return ret;
}

ptr_ast_class module::parse_class() {
    SPACEPLUS(m_parsec);

    auto ret = std::make_unique<ast_class>();

    // parse class name
    PARSEID(ret->m_id, m_parsec);

    m_parsec.spaces();

    // parse type variable arguments
    ret->m_tvars = parse_tvars();
    if (!ret->m_tvars)
        return nullptr;

    m_parsec.spaces();

    char c;
    PTRY(m_parsec, c, m_parsec.character('{'));

    if (m_parsec.is_fail()) {
        // parse predicates
        ret->m_preds = parse_preds();

        PARSECHAR('{', m_parsec);
    }

    m_parsec.spaces();

    // interfaces

    PARSECHAR('}', m_parsec);

    return ret;
}

} // namespace lunar