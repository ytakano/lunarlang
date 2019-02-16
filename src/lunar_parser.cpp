#include "lunar_parser.hpp"
#include "lunar_print.hpp"

#include <fstream>

#define SYNTAXERR(M, ...)                                                      \
    do {                                                                       \
        fprintf(stderr, "%s:%lu:%lu:(%d) syntax error: " M "\n",               \
                m_filename.c_str(), m_parsec.get_line(),                       \
                m_parsec.get_column(), __LINE__, ##__VA_ARGS__);               \
        print_err(m_parsec.get_line(), m_parsec.get_column(),                  \
                  m_parsec.get_str());                                         \
    } while (0)

#define SPACEPLUS()                                                            \
    do {                                                                       \
        if (!parse_spaces_plus())                                              \
            return nullptr;                                                    \
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

    m_wsp2.insert(' ');
    m_wsp2.insert('\t');

    m_wsp3.insert(' ');
    m_wsp3.insert('\t');
    m_wsp3.insert('\r');
    m_wsp3.insert('\n');
    m_wsp3.insert(';');

    m_infix.insert('+');
    m_infix.insert('-');
    m_infix.insert('<');
    m_infix.insert('>');
    m_infix.insert('/');
    m_infix.insert('%');
    m_infix.insert(':');
    m_infix.insert('|');
    m_infix.insert('&');
    m_infix.insert('*');
    m_infix.insert('^');
    m_infix.insert('@');
    m_infix.insert('=');
    m_infix.insert('.');
    m_infix.insert('!');
    m_infix.insert('?');

    m_newline.insert('\r');
    m_newline.insert('\n');

    m_newline_sc.insert('\r');
    m_newline_sc.insert('\n');
    m_newline_sc.insert(';');
}

bool module::parse() {
    for (;;) {
        parse_spaces();
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
            auto cls = parse_class();
            if (!cls)
                return false;

            cls->m_line = id->m_line;
            cls->m_column = id->m_column;

            // TODO: check multiply defined
            m_id2class[cls->m_id->m_id] = std::move(cls);
        }
    }

    return true;
}

// $ID := [^0-9$WHITESPACE][^$WHITESPACE]+
ptr_ast_id module::parse_id() {
    auto ret = std::make_unique<ast_id>();

    ret->set_pos(m_parsec);

    PMANYONE(m_parsec, ret->m_id, m_parsec.oneof_not(m_parser.m_no_id_char));

    if (m_parsec.is_fail())
        return nullptr;

    return ret;
}

// $TVAR := `$ID
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

// $KIND := $STAR | $STAR -> $KIND
// $STAR := *
ptr_ast_kind module::parse_kind() {
    std::deque<ptr_ast_kind> ks;

    auto k = std::make_unique<ast_kstar>();
    k->set_pos(m_parsec);

    PARSECHAR('*', m_parsec);

    ks.push_back(std::move(k));

    for (;;) {
        parse_spaces();

        std::string ar;
        PTRY(m_parsec, ar, m_parsec.str("->"));
        if (m_parsec.is_fail())
            break;

        parse_spaces();

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

// $TVARKIND := `$ID | `$ID :: $KIND
// $TVARKINDS := $TVARKIND | $TVARKIND , $TVARKINDS
// $TVARS := <$TVARKINDS>
ptr_ast_tvars module::parse_tvars() {
    auto tvars = std::make_unique<ast_tvars>();

    tvars->set_pos(m_parsec);

    PARSECHAR('<', m_parsec);

    parse_spaces();

    for (;;) {
        ast_tvars::ptr_arg arg;
        PARSETVAR(arg->m_id, m_parsec);

        parse_spaces();

        char c;
        PEEK(c, m_parsec);

        if (c == ':') {
            PARSESTR("::", m_parsec);
            parse_spaces();

            arg->m_kind = parse_kind();
            if (!arg->m_kind)
                return nullptr;

            parse_spaces();
        }

        tvars->m_args.push_back(std::move(arg));

        PEEK(c, m_parsec);

        if (c == '>')
            break;

        PARSECHAR(',', m_parsec);
        parse_spaces();
    }

    m_parsec.character('>');
    return tvars;
}

// $TYPE := $IDTVAR <$TYPES>? | fn ( $TYPES? ) : $TYPE | ( $TYPES? )
// $IDTVAR := $ID | $TVAR
ptr_ast_type module::parse_type(bool is_funret = false) {
    char c;
    PEEK(c, m_parsec);

    switch (c) {
    case '`': {
        // $TVAR <$TYPES>?
        auto ret = std::make_unique<ast_normaltype>();
        PARSETVAR(ret->m_id, m_parsec);

        parse_spaces();
        if (!parse_arg_types(ret->m_args))
            return nullptr;

        return ret;
    }
    case '(': {
        // ( $TYPES? )
        parse_spaces();
        auto ret = std::make_unique<ast_tupletype>();

        PTRY(m_parsec, c, m_parsec.character(')'));
        if (m_parsec.is_fail()) {
            ret->m_types = parse_types();
            if (!ret->m_types)
                return nullptr;
            parse_spaces();
            PARSECHAR(')', m_parsec);
        }

        return ret;
    }
    default:
        auto id = parse_id();
        if (!id) {
            SYNTAXERR("expected a type specifier");
            return nullptr;
        }

        if (id->m_id == "fn") {
            // fn ( $TYPES? ) : $TYPE
            parse_spaces();
            PARSECHAR('(', m_parsec);
            parse_spaces();

            auto ret = std::make_unique<ast_funtype>();

            // $TYPES?
            PTRY(m_parsec, c, m_parsec.character(')'));
            if (m_parsec.is_fail()) {
                ret->m_args = parse_types();
                if (!ret->m_args)
                    return nullptr;

                parse_spaces();
                PARSECHAR(')', m_parsec);
            }

            parse_spaces();
            PARSECHAR(':', m_parsec);
            parse_spaces();

            ret->m_ret = parse_type();
            if (!ret->m_ret)
                return nullptr;

            return ret;
        } else {
            // $TYPE <$TYPES>?
            auto ret = std::make_unique<ast_normaltype>();
            ret->m_id = std::move(id);

            parse_spaces();
            if (!parse_arg_types(ret->m_args))
                return nullptr;

            return ret;
        }
    }

    return nullptr; // never reach here
}

// <$TYPES>?
bool module::parse_arg_types(ptr_ast_types &types) {
    char c;
    PTRY(m_parsec, c, m_parsec.character('<'));
    if (m_parsec.is_fail())
        return true;

    parse_spaces();

    types = parse_types();
    if (!types)
        return false;

    parse_spaces();
    m_parsec.character('>');
    if (m_parsec.is_fail()) {
        SYNTAXERR("expected '>'");
        return false;
    }

    return true;
}

// $TYPES := $TYPE | $TYPE , $TYPES
ptr_ast_types module::parse_types() {
    auto ret = std::make_unique<ast_types>();
    ret->set_pos(m_parsec);

    for (;;) {
        auto t = parse_type();
        if (!t)
            return nullptr;

        ret->m_types.push_back(std::move(t));

        parse_spaces();
        char c;
        PTRY(m_parsec, c, m_parsec.character(','));
        if (m_parsec.is_fail())
            break;

        parse_spaces();
    }

    return ret;
}

// $PRED := $ID <$TYPES>
ptr_ast_pred module::parse_pred() {
    auto ret = std::make_unique<ast_pred>();

    ret->set_pos(m_parsec);
    PARSEID(ret->m_id, m_parsec);

    parse_spaces();
    PARSECHAR('<', m_parsec);
    parse_spaces();

    // arguments
    ret->m_args = parse_types();
    if (!ret->m_args)
        return nullptr;

    PARSECHAR('>', m_parsec);

    return ret;
}

// $PREDS := where $PREDS_
// $PREDS_ := $PRED | $PRED, $PRED
ptr_ast_preds module::parse_preds() {
    auto ret = std::make_unique<ast_preds>();

    ret->set_pos(m_parsec);
    PARSESTR("where", m_parsec);
    SPACEPLUS();

    for (;;) {
        auto pred = parse_pred();
        if (!pred)
            return nullptr;

        ret->m_preds.push_back(std::move(pred));
        parse_spaces();

        char c;
        PTRY(m_parsec, c, m_parsec.character(','));
        if (m_parsec.is_fail())
            break;

        parse_spaces();
    }

    return ret;
}

// $CLASSDECL := class $ID $TVARKINDSP $PREDS? { $INTERFACES $WHITESPACE3* }
ptr_ast_class module::parse_class() {
    SPACEPLUS();

    auto ret = std::make_unique<ast_class>();

    // parse class name
    PARSEID(ret->m_id, m_parsec);

    parse_spaces();

    // parse type variable arguments
    ret->m_tvars = parse_tvars();
    if (!ret->m_tvars)
        return nullptr;

    parse_spaces();

    char c;
    PTRY(m_parsec, c, m_parsec.character('{'));

    if (m_parsec.is_fail()) {
        // parse predicates
        ret->m_preds = parse_preds();

        PARSECHAR('{', m_parsec);
    }

    parse_spaces();

    // interfaces
    ret->m_interfaces = parse_interfaces();
    if (!ret->m_interfaces)
        return nullptr;

    return ret;
}

// $INTERFAE := fn $INTNAME ( $TYPES ) : $TYPE
// $INTNAME := $ID | infix $INFIX
ptr_ast_interface module::parse_interface() {
    auto ret = std::make_unique<ast_interface>();

    ret->set_pos(m_parsec);

    // fn $INTNAME
    PARSESTR("fn", m_parsec);
    SPACEPLUS();

    PARSEID(ret->m_id, m_parsec);
    if (ret->m_id->m_id == "infix") {
        parse_spaces();
        ret->m_infix = parse_infix();
        if (!ret->m_infix)
            return nullptr;
    }

    parse_spaces();
    PARSECHAR('(', m_parsec);
    parse_spaces();

    char c;
    PTRY(m_parsec, c, m_parsec.character(')'));
    if (m_parsec.is_fail()) {
        ret->m_args = parse_types();
        if (!ret->m_args)
            return nullptr;

        parse_spaces();
        PARSECHAR(')', m_parsec);
    }

    parse_spaces();
    PARSECHAR(':', m_parsec);
    parse_spaces();

    ret->m_ret = parse_type();
    if (!ret->m_ret)
        return nullptr;

    return ret;
}

// $INFIX := $INFIXCHAR+
// $INFIXCHAR := + | - | < | > | / | % | : | & |
//               * | ^ | @ | = | . | ! | ? | ~
ptr_ast_infix module::parse_infix() {
    auto ret = std::make_unique<ast_infix>();

    PMANYONE(m_parsec, ret->m_infix,
             [](parsec &p, std::unordered_set<char> &s) {
                 return p.oneof(s);
             }(m_parsec, m_parser.m_infix));

    if (m_parsec.is_fail())
        return nullptr;

    return ret;
}

// $INTERFACES := $INTERFACE | $INTERFACE $SEP $INTERFACES
ptr_ast_interfaces module::parse_interfaces() {
    auto ret = std::make_unique<ast_interfaces>();

    ret->set_pos(m_parsec);

    for (;;) {
        auto intf = parse_interface();
        if (!intf)
            return nullptr;

        ret->m_interfaces.push_back(std::move(intf));

        char c;
        PTRY(m_parsec, c, [](parsec &p, std::unordered_set<char> &cs) {
            std::string tmp;
            PMANY(p, tmp, p.oneof(cs));
            return p.character('}');
        }(m_parsec, m_parser.m_wsp3));
        if (!m_parsec.is_fail())
            return ret;

        if (!parse_sep())
            return nullptr;
    }
}

// $NEWLINE := \r | \n | ;
// $WHITESPACE2 := space | tab
// $WHITESPACE3 := space | tab | \r | \n | \r\n | ;
// $SEP := $WHITESPACE2* $NEWLINE+ $WHITESPACE3*
bool module::parse_sep() {
    // skip comment
    std::string s;
    PTRY(m_parsec, s, m_parsec.str("//"));
    if (!m_parsec.is_fail())
        PMANY(m_parsec, s, m_parsec.oneof_not(m_parser.m_newline));

    // $WHITESPACE2*
    std::string tmp;
    PMANY(m_parsec, tmp, m_parsec.oneof(m_parser.m_wsp2));

    // $NEWLINE+
    tmp.clear();
    PMANYONE(m_parsec, tmp, m_parsec.oneof(m_parser.m_newline_sc));
    if (!m_parsec.is_fail()) {
        SYNTAXERR("expected a newline or ';'");
        return false;
    }

    // $WHITESPACE3*
    char c;
    for (;;) {
        PMANY(m_parsec, tmp, m_parsec.oneof(m_parser.m_wsp3));

        PTRY(m_parsec, s, m_parsec.str("//"));
        if (m_parsec.is_fail())
            return true;

        PMANY(m_parsec, s, m_parsec.oneof_not(m_parser.m_newline));
    }

    return true;
}

void module::parse_spaces() {
    char c;

    for (;;) {
        m_parsec.spaces();

        std::string s;
        PTRY(m_parsec, s, m_parsec.str("//"));
        if (m_parsec.is_fail())
            return;

        PMANY(m_parsec, s, m_parsec.oneof_not(m_parser.m_newline));
    }
}

bool module::parse_spaces_plus() {
    char c;
    std::string s;

    PTRY(m_parsec, s, m_parsec.str("//"));
    if (!m_parsec.is_fail())
        PMANY(m_parsec, s, m_parsec.oneof_not(m_parser.m_newline));

    m_parsec.space();
    if (m_parsec.is_fail()) {
        SYNTAXERR("expected whitespaces");
        return false;
    }

    parse_spaces();

    return true;
}

bool parser::add_module(const char *filename) {
    std::ifstream ifs(filename);
    if (ifs.fail())
        return false;

    std::string content((std::istreambuf_iterator<char>(ifs)),
                        (std::istreambuf_iterator<char>()));

    auto m = std::make_unique<module>(filename, content, *this);

    m_modules[filename] = std::move(m);

    return true;
}

bool parser::compile() { return true; }

} // namespace lunar