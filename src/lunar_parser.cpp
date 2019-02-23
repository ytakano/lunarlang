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
        ks.pop_back();

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
        auto arg = std::make_unique<ast_tvars::arg>();
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

// $TYPE := $IDTVAR <$TYPES>? | fn ( $TYPES? ) $TYPESPEC | ( $TYPES? )
// $IDTVAR := $ID | $TVAR
// $TYPESPEC := : $TYPE
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

// $INTERFACE := fn $INTNAME ( $TYPES ) $TYPESPEC
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
        PTRY(m_parsec, c, [](parsec &p, module &m) {
            m.parse_spaces_sep();
            return p.character('}');
        }(m_parsec, *this));
        if (!m_parsec.is_fail())
            return ret;

        if (!parse_sep())
            return nullptr;
    }
}

// $DEFUN := fn $ID ( $ARGS? ) $RETTYPE? $PREDS? { $EXPRS }
ptr_ast_defun module::parse_defun() {
    SPACEPLUS();

    auto ret = std::make_unique<ast_defun>();
    ret->set_pos(m_parsec);

    // $ID
    PARSEID(ret->m_id, m_parsec);
    parse_spaces();

    // ( $ARGS? )
    PARSECHAR('(', m_parsec);
    parse_spaces();

    char c;
    PTRY(m_parsec, c, m_parsec.character(')'));
    if (m_parsec.is_fail()) {
        ret->m_args = parse_args();
        if (!ret->m_args)
            return nullptr;

        PARSECHAR(')', m_parsec);
    }

    // $RETTYPE?
    parse_spaces();
    PTRY(m_parsec, c, m_parsec.character(':'));
    if (!m_parsec.is_fail()) {
        ret->m_ret = parse_type();
        if (!ret->m_ret)
            return nullptr;

        parse_spaces();
    }

    // $PREDS?
    PTRY(m_parsec, c, m_parsec.character('{'));
    if (m_parsec.is_fail()) {
        // parse predicates
        ret->m_preds = parse_preds();

        PARSECHAR('{', m_parsec);
    }

    // TODO: expr

    PARSECHAR('}', m_parsec);

    return ret;
}

// $ARGS := $ARG | $ARG , $ARGS
ptr_ast_args module::parse_args() {
    auto ret = std::make_unique<ast_args>();
    ret->set_pos(m_parsec);

    char c;
    for (;;) {
        auto arg = parse_arg();
        if (!arg)
            return nullptr;

        ret->m_args.push_back(std::move(arg));

        parse_spaces();
        PTRY(m_parsec, c, m_parsec.character(','));
        if (m_parsec.is_fail()) {
            return ret;
        }
        parse_spaces();
    }

    return nullptr; // never reach here
}

// $ARG := $ID $TYPESPEC?
ptr_ast_arg module::parse_arg() {
    auto ret = std::make_unique<ast_arg>();
    ret->set_pos(m_parsec);

    PARSEID(ret->m_id, m_parsec);
    parse_spaces();

    char c;
    PEEK(c, m_parsec);
    if (c != ':')
        return ret;

    parse_spaces();
    ret->m_type = parse_type();
    if (!ret->m_type)
        return nullptr;

    return ret;
}

// $EXPR0 := $ID | $IF | $LET | ( $EXPR , ) | ( $EXPR ) | ( $EXPRS_? ) |
//          { $EXPRS } | $LITERALS
ptr_ast_expr module::parse_expr0() {
    char c;
    PEEK(c, m_parsec);

    switch (c) {
    case '(':
        // ( $EXPR , ) | ( $EXPR ) | ( $EXPRS_? )
        ;
    case '{':
        // { $DICT } | { $EXPRS }
        ;
    case '[':
        // [ $EXPRS_? ]
        ;
    case '"':;
    case '0':
    case '1':
    case '2':
    case '3':
    case '4':
    case '5':
    case '6':
    case '7':
    case '8':
    case '9':;
    }

    auto id = parse_id();
    if (!id) {
        SYNTAXERR("unexpected character");
        return nullptr;
    }

    if (id->m_id == "if") {
        // $IF
        auto ret = parse_if();
        if (!ret)
            return nullptr;

        ret->m_line = id->m_line;
        ret->m_column = id->m_column;
        return ret;
    } else if (id->m_id == "let") {
        // $LET
        auto ret = parse_let();
        if (!ret)
            return nullptr;

        ret->m_line = id->m_line;
        ret->m_column = id->m_column;
    } else {
        // $ID
        auto ret = std::make_unique<ast_expr_id>();
        ret->m_line = id->m_line;
        ret->m_column = id->m_column;
        ret->m_id = std::move(id);
        return ret;
    }

    return nullptr;
}

// $EXPR := $EXPR0 $EXPR'
ptr_ast_expr module::parse_expr() { return nullptr; }

// $EXPR' := ∅ | . $ID $EXPR' | $INFIX $EXPR $EXPR' | [ $EXPR ] $EXPR' |
//          $APPLY $EXPR'
ptr_ast_expr module::parse_exprp() { return nullptr; }

// $IF := if $EXPR { $EXPRS } $ELSE?
// $ELSE := elif $EXPR { $EXPRS } $ELSE | else { $EXPRS }
ptr_ast_if module::parse_if() {
    SPACEPLUS();

    auto ret = std::make_unique<ast_if>();

    ret->m_cond = parse_expr();
    if (!ret->m_cond)
        return nullptr;

    parse_spaces();
    PARSECHAR('{', m_parsec);
    parse_spaces();

    // then
    // $EXPRS
    ret->m_then = parse_exprs();
    if (!ret->m_then)
        return nullptr;

    parse_spaces();

    struct pos {
        int line, column;
    };

    auto func = [](parsec &p, std::unordered_set<char> &ch, const char *str) {
        std::string ret;
        p.spaces();
        int line = p.get_line();
        int column = p.get_column();
        ret = p.oneof_not(ch);
        if (ret != str)
            p.set_fail(true);

        return pos{line, column};
    };

    pos p;
    PTRY(m_parsec, p, func(m_parsec, m_parser.m_no_id_char, "elif"));
    if (!m_parsec.is_fail()) {
        // elif
        ret->m_elif = parse_if();
        if (!ret->m_elif)
            return nullptr;

        ret->m_elif->m_line = p.line;
        ret->m_elif->m_column = p.column;

        return ret;
    }

    PTRY(m_parsec, p, func(m_parsec, m_parser.m_no_id_char, "else"));
    if (!m_parsec.is_fail()) {
        // else
        parse_spaces();
        PARSECHAR('{', m_parsec);
        parse_spaces();

        // $EXPRS
        ret->m_else = parse_exprs();
        if (!ret->m_else)
            return nullptr;

        parse_spaces();

        return ret;
    }

    return ret;
}

// $LET := let $DEFVARS $IN?
// $IN := in $EXPR
ptr_ast_let module::parse_let() {
    SPACEPLUS();
    auto ret = std::make_unique<ast_let>();

    // $DEFVARS
    ret->m_defvars = parse_defvars();
    if (!ret->m_defvars)
        return nullptr;

    // $IN?
    bool flag;
    PTRY(m_parsec, flag, [](parsec &p, module &m) {
        p.str("in");
        if (p.is_fail())
            return false;

        m.parse_spaces();
        p.character('{');

        return p.is_fail();
    }(m_parsec, *this));

    if (!flag)
        return ret;

    parse_spaces();

    ret->m_in = parse_exprs();
    if (!ret->m_in)
        return nullptr;

    return ret;
}

// $DEFVAR := $ID = $EXPR $TYPESPEC?
ptr_ast_defvar module::parse_defvar() {
    auto ret = std::make_unique<ast_defvar>();
    ret->set_pos(m_parsec);

    // $ID
    PARSEID(ret->m_id, m_parsec);

    // =
    parse_spaces();
    PARSECHAR('=', m_parsec);
    parse_spaces();

    // $EXPR
    ret->m_expr = parse_expr();
    if (!ret->m_expr)
        return nullptr;

    // $TYPESPEC?
    parse_spaces();
    PARSECHAR(':', m_parsec);
    parse_spaces();

    ret->m_type = parse_type();
    if (!ret->m_type)
        return nullptr;

    return ret;
}

// $DEFVARS := $DEFVAR | $DEFVAR , $DEFVARS
ptr_ast_defvars module::parse_defvars() {
    auto ret = std::make_unique<ast_defvars>();

    ret->set_pos(m_parsec);
    for (;;) {
        // $DEFVAR
        auto def = parse_defvar();
        if (!def)
            return nullptr;

        ret->m_defs.push_back(std::move(def));
        parse_spaces();
        char c = m_parsec.peek();
        if (c != ',')
            return ret;

        parse_spaces();
    }

    return nullptr;
}

// $EXPRS := $EXPR | $EXPR $SEP $EXPR
ptr_ast_exprs module::parse_exprs() {
    auto ret = std::make_unique<ast_exprs>();

    ret->set_pos(m_parsec);
    for (;;) {
        // $EXPR
        auto e = parse_expr();
        if (!e)
            return nullptr;

        ret->m_exprs.push_back(std::move(e));

        char c;
        PTRY(m_parsec, c, [](parsec &p, module &m) {
            m.parse_spaces_sep();
            return p.character('}');
        }(m_parsec, *this));
        if (!m_parsec.is_fail())
            return ret;

        // $SEP
        if (!parse_sep())
            return nullptr;
    }

    return nullptr; // never reach here
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
    if (m_parsec.is_fail()) {
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

void module::parse_spaces_sep() {
    std::string tmp;

    for (;;) {
        PMANY(m_parsec, tmp, m_parsec.oneof(m_parser.m_wsp3));

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

bool parser::add_module(const std::string &filename) {
    std::ifstream ifs(filename);
    if (ifs.fail())
        return false;

    // TODO: check file size

    std::string content((std::istreambuf_iterator<char>(ifs)),
                        (std::istreambuf_iterator<char>()));

    auto m = std::make_unique<module>(filename, content, *this);

    m_modules[filename] = std::move(m);

    return true;
}

bool parser::parse() {
    for (auto &p : m_modules) {
        if (!p.second->parse())
            return false;
    }
    return true;
}

void parser::print() {
    for (auto &p : m_modules) {
        p.second->print();
    }

    std::cout << std::endl;
}

void module::print() {
    std::cout << "{\"classes\":[";
    int n = 1;
    for (auto &p : m_id2class) {
        p.second->print();
        if (n < m_id2class.size())
            std::cout << ",";

        n++;
    }
    std::cout << "]";

    std::cout << "}";
}

void ast_id::print() { std::cout << "\"" << m_id << "\""; }

void ast_kfun::print() {
    std::cout << "{\"left\":";
    m_left->print();
    std::cout << ",\"right\":";
    m_right->print();
    std::cout << "}";
}

void ast_kstar::print() { std::cout << "\"*\""; }

void ast_tvars::print() {
    std::cout << "[";
    int n = 1;
    for (auto &v : m_args) {
        std::cout << "{\"id\":";
        v->m_id->print();
        if (v->m_kind) {
            std::cout << ",\"kind\":";
            v->m_kind->print();
        }

        std::cout << "}";
        if (n < m_args.size())
            std::cout << ",";

        n++;
    }
    std::cout << "]";
}

void ast_class::print() {
    std::cout << "{\"id\":";
    m_id->print();

    std::cout << ",\"type variables\":";
    m_tvars->print();

    if (m_preds) {
        std::cout << ",\"predicates\":";
        m_preds->print();
    }

    std::cout << ",\"interfaces\":";
    m_interfaces->print();

    std::cout << "}";
}

void ast_normaltype::print() {
    std::cout << "{\"type\": \"normal\",\"id\":";
    m_id->print();

    if (m_args) {
        std::cout << ",\"type arguments\":";
        m_args->print();
    }

    std::cout << "}";
}

void ast_funtype::print() {
    std::cout << "{\"type\": \"function\",\"arguments\":";
    m_args->print();

    std::cout << ",\"return\":";
    m_ret->print();

    std::cout << "}";
}

void ast_tupletype::print() {
    std::cout << "{\"type\": \"tuple\",\"types\":";
    m_types->print();
    std::cout << "}";
}

#define PRINTLIST(LIST)                                                        \
    do {                                                                       \
        std::cout << "[";                                                      \
        int n = 1;                                                             \
        for (auto &p : LIST) {                                                 \
            p->print();                                                        \
            if (n < (LIST).size())                                             \
                std::cout << ",";                                              \
            n++;                                                               \
        }                                                                      \
        std::cout << "]";                                                      \
    } while (0)

void ast_types::print() { PRINTLIST(m_types); }

void ast_pred::print() {
    std::cout << "{\"id\":";
    m_id->print();

    std::cout << ",\"arguments\":";
    m_args->print();

    std::cout << "}";
}

void ast_preds::print() { PRINTLIST(m_preds); }

void ast_infix::print() { std::cout << "\"" << m_infix << "\""; }

void ast_interface::print() {
    std::cout << "{\"id\":";
    m_id->print();

    if (m_infix) {
        std::cout << ",\"infix\":";
        m_infix->print();
    }

    std::cout << ",\"arguments\":";
    m_args->print();

    std::cout << ",\"return\":";
    m_ret->print();
    std::cout << "}";
}

void ast_interfaces::print() { PRINTLIST(m_interfaces); }

void ast_arg::print() {
    std::cout << "{\"id\":";
    m_id->print();
    if (m_type) {
        std::cout << ",\"type\":";
        m_type->print();
    }
    std::cout << "}";
}

void ast_args::print() { PRINTLIST(m_args); }

void ast_defun::print() {
    std::cout << "{\"id\":";
    m_id->print();

    std::cout << ",\"arguments\":";
    m_args->print();

    if (m_ret) {
        std::cout << ",\"return\":";
        m_ret->print();
    }

    if (m_preds) {
        std::cout << ",\"predicates\":";
        m_preds->print();
    }

    std::cout << "}";
}

} // namespace lunar