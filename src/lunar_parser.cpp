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

    m_op2pri["."] = 19;
    m_op2pri["*"] = 14;
    m_op2pri["/"] = 14;
    m_op2pri["%"] = 14;
    m_op2pri["+"] = 13;
    m_op2pri["-"] = 13;
    m_op2pri["<<"] = 12;
    m_op2pri[">>"] = 12;
    m_op2pri["<"] = 11;
    m_op2pri[">"] = 11;
    m_op2pri["<="] = 11;
    m_op2pri[">="] = 11;
    m_op2pri["=="] = 10;
    m_op2pri["!="] = 10;
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

        if (id->m_id == "func") {
            auto fn = parse_defun();
            if (!fn)
                return false;

            fn->m_line = id->m_line;
            fn->m_column = id->m_column;

            // TODO: check multiply defined
            m_id2defun[fn->m_id->m_id] = std::move(fn);
        } else if (id->m_id == "instance") {
            auto inst = parse_instance();
            if (!inst)
                return false;

            inst->m_line = id->m_line;
            inst->m_column = id->m_column;

            // TODO: check multiply defined
            m_id2inst.insert(std::pair<std::string, ptr_ast_instance>(
                inst->m_pred->m_id->m_id, std::move(inst)));
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

// $TYPE := $IDTVAR <$TYPES>? | func ( $TYPES? ) $TYPESPEC | ( $TYPES? )
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

        if (id->m_id == "func") {
            // func ( $TYPES? ) : $TYPE
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

// $PREDS := require $PREDS_
// $PREDS_ := $PRED | $PRED, $PRED
ptr_ast_preds module::parse_preds() {
    auto ret = std::make_unique<ast_preds>();

    ret->set_pos(m_parsec);
    PARSESTR("require", m_parsec);
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

// $CLASSDECL := class $ID $TVARS $PREDS? { $INTERFACES $WHITESPACE3* }
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

// $INTERFACE := func $INTNAME ( $TYPES ) $TYPESPEC
// $INTNAME := $ID | infix $INFIX
ptr_ast_interface module::parse_interface() {
    auto ret = std::make_unique<ast_interface>();

    ret->set_pos(m_parsec);

    // func $INTNAME
    PARSESTR("func", m_parsec);
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

// $DEFUN := func $ID ( $ARGS? ) $RETTYPE? $PREDS? { $EXPRS }
ptr_ast_defun module::parse_defun(bool is_infix) {
    SPACEPLUS();

    auto ret = std::make_unique<ast_defun>();
    ret->set_pos(m_parsec);

    // $ID
    PARSEID(ret->m_id, m_parsec);
    parse_spaces();

    if (is_infix && ret->m_id->m_id == "infix") {
        ret->m_infix = parse_infix();
        if (!ret->m_infix)
            return nullptr;

        parse_spaces();
    }

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
        parse_spaces();
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

    // $EXPRS
    parse_spaces();
    ret->m_exprs = parse_exprs();
    if (!ret->m_exprs)
        return nullptr;

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

    PARSECHAR(':', m_parsec);
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
        return parse_parentheses();
    case '{':
        // { $DICT } | { $EXPRS }
        return parse_braces();
    case '[':
        // [ $EXPRS_? ]
        return parse_brackets();
    case '"':
        return parse_str();
    }

    if ('0' <= c && c <= '9') {
        // number
        return parse_num();
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
        return ret;
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

ptr_ast_num module::parse_num() {
    auto ret = std::make_unique<ast_num>();
    ret->set_pos(m_parsec);

    ret->m_numtype = m_parsec.num_literal(ret->m_num);
    if (ret->m_numtype == parsec::NUM_FAIL) {
        SYNTAXERR("unexpected character");
        return nullptr;
    }

    return ret;
}

ptr_ast_str module::parse_str() {
    auto ret = std::make_unique<ast_str>();
    ret->set_pos(m_parsec);

    if (m_parsec.str_literal(ret->m_str)) {
        SYNTAXERR("unexpected character");
        return nullptr;
    }

    return ret;
}

// { $DICT } | { $EXPRS }
ptr_ast_expr module::parse_braces() {
    int line = m_parsec.get_line();
    int column = m_parsec.get_column();
    PARSECHAR('{', m_parsec);
    parse_spaces();

    auto e = parse_expr();
    if (!e)
        return nullptr;

    parse_spaces();

    char c;
    PTRY(m_parsec, c, [](module &m) {
        m.parse_spaces();
        return m.m_parsec.character(':');
    }(*this));

    if (m_parsec.is_fail()) {
        // $EXPRS
        auto ret = std::make_unique<ast_exprs>();

        ret->m_line = e->m_line;
        ret->m_column = e->m_column;
        ret->m_exprs.push_back(std::move(e));

        for (;;) {
            // $EXPR
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

            auto e = parse_expr();
            if (!e)
                return nullptr;

            ret->m_exprs.push_back(std::move(e));
        }
    } else {
        // $DICT
        parse_spaces();
        auto v = parse_expr();
        if (!v)
            return nullptr;

        auto elm = std::make_unique<ast_dictelm>();
        elm->m_line = e->m_line;
        elm->m_column = e->m_column;
        elm->m_key = std::move(e);
        elm->m_val = std::move(v);

        auto ret = std::make_unique<ast_dict>();
        ret->m_elms.push_back(std::move(elm));
        ret->m_line = line;
        ret->m_column = column;

        for (;;) {
            // $DICTELM
            char c;
            PTRY(m_parsec, c, [](parsec &p, module &m) {
                m.parse_spaces_sep();
                return p.character('}');
            }(m_parsec, *this));
            if (!m_parsec.is_fail())
                return ret;

            parse_spaces();
            PARSECHAR(',', m_parsec);
            parse_spaces();

            auto e = std::make_unique<ast_dictelm>();
            e->set_pos(m_parsec);

            e->m_key = parse_expr();
            if (!e->m_key)
                return nullptr;

            parse_spaces();
            PARSECHAR(':', m_parsec);
            parse_spaces();

            e->m_val = parse_expr();
            if (!e->m_val)
                return nullptr;
        }
    }

    return nullptr; // never reach here
}

// $EXPR := $EXPR0 $EXPR' | $EXPR0 $EXPR' $INFIX+ $EXPR
ptr_ast_expr module::parse_expr() {
    std::deque<ptr_ast_infix> infix;
    std::deque<ptr_ast> exprs; // Reverse Polish Notation

    for (;;) {
        // $EXPR0 $EXPR'
        auto lhs = parse_expr0();
        if (!lhs)
            return nullptr;

        lhs = parse_exprp(std::move(lhs));
        if (!lhs)
            return nullptr;

        exprs.push_back(std::move(lhs));

        // $INFIX+ $EXPR
        bool is_infix;
        PTRY(m_parsec, is_infix, [](module &m) {
            m.parse_spaces();
            char tmp = m.m_parsec.peek();
            if (HASKEY(m.m_parser.m_infix, tmp)) {
                return true;
            }

            m.m_parsec.set_fail(true);
            return false;
        }(*this));
        if (m_parsec.is_fail() || !is_infix)
            break;

        // $INFIX+
        auto op = parse_binop();
        if (!op)
            return nullptr;

        parse_spaces();

        // make expression written in reverse polish notation
        while (!infix.empty()) {
            auto &b = infix.back();
            if (m_parser.get_pri(b->m_infix) >= m_parser.get_pri(op->m_infix)) {
                exprs.push_back(std::move(b));
                infix.pop_back();
            } else {
                break;
            }
        }

        infix.push_back(std::move(op));
    }

    while (!infix.empty()) {
        auto &p = infix.back();
        exprs.push_back(std::move(p));
        infix.pop_back();
    }

    // make AST from the expression written in reverse polish notation
    std::deque<ptr_ast> st;
    while (!exprs.empty()) {
        auto &p = exprs.front();

        assert(p->m_asttype == ast::AST_EXPR || p->m_asttype == ast::AST_INFIX);

        if (p->m_asttype == ast::AST_EXPR) {
            st.push_back(std::move(p));
        } else if (p->m_asttype == ast::AST_INFIX) {
            auto op = std::make_unique<ast_binexpr>();

            op->m_op = std::unique_ptr<ast_infix>((ast_infix *)p.release());

            op->m_right =
                std::unique_ptr<ast_expr>((ast_expr *)st.back().release());
            st.pop_back();

            op->m_left =
                std::unique_ptr<ast_expr>((ast_expr *)st.back().release());
            st.pop_back();

            st.push_back(std::move(op));
        }

        exprs.pop_front();
    }

    assert(st.size() == 1);
    assert(st.back()->m_asttype == ast::AST_EXPR);

    return std::unique_ptr<ast_expr>((ast_expr *)st.back().release());
}

ptr_ast_infix module::parse_binop() {
    auto ret = std::make_unique<ast_infix>();
    ret->set_pos(m_parsec);

    PMANY(m_parsec, ret->m_infix, m_parsec.oneof(m_parser.m_infix));

    return ret;
}

// $EXPR' := ∅ [ $EXPR ] $EXPR' | $APPLY $EXPR'
ptr_ast_expr module::parse_exprp(ptr_ast_expr lhs) {
    char c = m_parsec.peek();
    if (m_parsec.is_fail())
        return lhs;

    switch (c) {
    case '[': {
        m_parsec.character('[');
        parse_spaces();

        auto e = parse_expr();
        if (!e)
            return nullptr;

        parse_spaces();
        PARSECHAR(']', m_parsec);

        auto index = std::make_unique<ast_index>();
        index->m_array = std::move(lhs);
        index->m_index = std::move(e);

        return parse_exprp(std::move(index));
    }
    case '(': {
        auto apply = parse_apply(std::move(lhs));
        return parse_exprp(std::move(apply));
    }
    }

    return lhs;
}

// $APPLY := ( $EXPRS_? )
ptr_ast_apply module::parse_apply(ptr_ast_expr fun) {
    auto ret = std::make_unique<ast_apply>();
    ret->m_line = fun->m_line;
    ret->m_column = fun->m_column;
    ret->m_func = std::move(fun);

    PARSECHAR('(', m_parsec);
    parse_spaces();

    char c;
    PTRY(m_parsec, c, m_parsec.character(')'));
    if (!m_parsec.is_fail())
        return ret;

    for (;;) {
        auto e = parse_expr();
        if (!e)
            return nullptr;

        ret->m_args.push_back(std::move(e));

        parse_spaces();
        PTRY(m_parsec, c, m_parsec.character(')'));
        if (!m_parsec.is_fail())
            return ret;

        PARSECHAR(',', m_parsec);
        parse_spaces();
    }

    return nullptr; // never reach here
}

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

    struct pos {
        int line, column;
    };

    auto func = [](module &m, const char *str) {
        std::string ret;
        m.parse_spaces();
        int line = m.m_parsec.get_line();
        int column = m.m_parsec.get_column();
        PMANY(m.m_parsec, ret, m.m_parsec.oneof_not(m.m_parser.m_no_id_char));
        if (ret != str)
            m.m_parsec.set_fail(true);

        return pos{line, column};
    };

    pos p;
    PTRY(m_parsec, p, func(*this, "elif"));
    if (!m_parsec.is_fail()) {
        // elif
        ret->m_elif = parse_if();
        if (!ret->m_elif)
            return nullptr;

        ret->m_elif->m_line = p.line;
        ret->m_elif->m_column = p.column;

        return ret;
    }

    PTRY(m_parsec, p, func(*this, "else"));
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
    bool is_in;
    PTRY(m_parsec, is_in, [](parsec &p, module &m) {
        m.parse_spaces_plus();
        p.str("in");
        if (p.is_fail())
            return false;

        m.parse_spaces_plus();
        return !p.is_fail();
    }(m_parsec, *this));

    if (!is_in)
        return ret;

    ret->m_in = parse_expr();
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

    // $TYPESPEC?
    char c;
    PTRY(m_parsec, c, [](module &m) {
        m.parse_spaces();
        return m.m_parsec.character(':');
    }(*this))

    if (!m_parsec.is_fail()) {
        parse_spaces();
        ret->m_type = parse_type();
        if (!ret->m_type)
            return nullptr;
    }

    // =
    parse_spaces();
    PARSECHAR('=', m_parsec);
    parse_spaces();

    // $EXPR
    ret->m_expr = parse_expr();
    if (!ret->m_expr)
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

        char is_comma;
        PTRY(m_parsec, is_comma, [](module &m) {
            m.parse_spaces();
            m.m_parsec.character(',');
            if (m.m_parsec.is_fail())
                return false;
            return true;
        }(*this));
        if (!is_comma)
            return ret;

        parse_spaces();
    }

    return nullptr;
}

// $PROD
ptr_ast_member module::parse_prod() { return nullptr; }

// $SUM
ptr_ast_member module::parse_sum() { return nullptr; }

// { $PROD }
ptr_ast_members module::parse_prods() { return nullptr; }

// { $SUM }
ptr_ast_members module::parse_sums() { return nullptr; }

// $STRUCT := struct $ID $TVARS? $PREDS? { $PROD }
ptr_ast_struct module::parse_struct() { return nullptr; }

// $UNION := union $ID $TVARS? $PREDS? { $SUM }
ptr_ast_union module::parse_union() { return nullptr; }

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

// ( $EXPR , ) | ( $EXPR ) | ( $EXPRS_? )
ptr_ast_expr module::parse_parentheses() {
    int line = m_parsec.get_line();
    int column = m_parsec.get_column();

    PARSECHAR('(', m_parsec);
    parse_spaces();
    auto e = parse_expr();
    if (!e)
        return nullptr;

    // expression
    parse_spaces();
    char c;
    PTRY(m_parsec, c, m_parsec.character(')'));
    if (!m_parsec.is_fail()) {
        auto p = std::make_unique<ast_parenthesis>();
        p->m_expr = std::move(e);
        p->m_line = line;
        p->m_column = column;
        return p;
    }

    // tuple
    PARSECHAR(',', m_parsec);
    parse_spaces();

    auto tuple = std::make_unique<ast_tuple>();
    tuple->m_exprs.push_back(std::move(e));
    tuple->m_line = line;
    tuple->m_column = column;

    PTRY(m_parsec, c, m_parsec.character(')'));
    if (!m_parsec.is_fail())
        return tuple;

    for (;;) {
        e = parse_expr();
        if (!e)
            return nullptr;

        tuple->m_exprs.push_back(std::move(e));

        parse_spaces();
        PTRY(m_parsec, c, m_parsec.character(')'));
        if (!m_parsec.is_fail())
            return tuple;

        PARSECHAR(',', m_parsec);
        parse_spaces();
    }

    return nullptr; // never reach here
}

// [ $EXPRS_? ]
ptr_ast_vector module::parse_brackets() {
    auto ret = std::make_unique<ast_vector>();
    ret->set_pos(m_parsec);

    PARSECHAR('[', m_parsec);
    parse_spaces();

    char c;
    PTRY(m_parsec, c, m_parsec.character(']'));
    if (!m_parsec.is_fail())
        return ret;

    for (;;) {
        auto e = parse_expr();
        if (!e)
            return nullptr;

        ret->m_exprs.push_back(std::move(e));

        parse_spaces();
        PTRY(m_parsec, c, m_parsec.character(']'));
        if (!m_parsec.is_fail())
            return ret;

        PARSECHAR(',', m_parsec);
        parse_spaces();
    }

    return nullptr; // never reach here
}

// $INST := instance $PRED $PREDS? { $DEFUNS }
ptr_ast_instance module::parse_instance() {
    SPACEPLUS();

    auto ret = std::make_unique<ast_instance>();
    ret->set_pos(m_parsec);

    ret->m_pred = parse_pred();
    if (!ret->m_pred)
        return nullptr;

    parse_spaces();

    char c;
    PTRY(m_parsec, c, m_parsec.character('{'));

    if (m_parsec.is_fail()) {
        // parse predicates
        ret->m_preds = parse_preds();

        PARSECHAR('{', m_parsec);
    }

    for (;;) {
        parse_spaces();
        PTRY(m_parsec, c, m_parsec.character('}'));
        if (!m_parsec.is_fail())
            return ret;

        int line = m_parsec.get_line();
        int column = m_parsec.get_column();
        m_parsec.str("func");
        if (m_parsec.is_fail()) {
            SYNTAXERR("expected a function definition");
            return nullptr;
        }

        auto fn = parse_defun(true);
        if (!fn)
            return nullptr;

        fn->m_line = line;
        fn->m_column = column;

        ret->m_id2defun[fn->m_id->m_id] = std::move(fn);
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
    if (m_parsec.is_fail()) {
        SYNTAXERR("expected a newline or ';'");
        return false;
    }

    // $WHITESPACE3*
    for (;;) {
        PMANY(m_parsec, tmp, m_parsec.oneof(m_parser.m_wsp3));

        PTRY(m_parsec, s, m_parsec.str("//"));
        if (m_parsec.is_fail()) {
            m_parsec.set_fail(false);
            return true;
        }

        PMANY(m_parsec, s, m_parsec.oneof_not(m_parser.m_newline));
    }

    return true;
}

void module::parse_spaces() {
    for (;;) {
        m_parsec.spaces();

        std::string s;
        PTRY(m_parsec, s, m_parsec.str("//"));
        if (m_parsec.is_fail()) {
            m_parsec.set_fail(false);
            return;
        }

        PMANY(m_parsec, s, m_parsec.oneof_not(m_parser.m_newline));
    }
}

void module::parse_spaces_sep() {
    std::string tmp;

    for (;;) {
        PMANY(m_parsec, tmp, m_parsec.oneof(m_parser.m_wsp3));

        std::string s;
        PTRY(m_parsec, s, m_parsec.str("//"));
        if (m_parsec.is_fail()) {
            m_parsec.set_fail(false);
            return;
        }

        PMANY(m_parsec, s, m_parsec.oneof_not(m_parser.m_newline));
    }
}

bool module::parse_spaces_plus() {
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
    size_t n = 1;
    for (auto &p : m_id2class) {
        p.second->print();
        if (n < m_id2class.size())
            std::cout << ",";

        n++;
    }
    std::cout << "],\"instances\":[";

    n = 0;
    for (auto &p : m_id2inst) {
        if (n > 0)
            std::cout << ",";
        p.second->print();
        n++;
    }

    std::cout << "],\"functions\":[";

    n = 1;
    for (auto &p : m_id2defun) {
        p.second->print();
        if (n < m_id2defun.size())
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
    size_t n = 1;
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
        size_t n = 1;                                                          \
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

    if (m_infix) {
        std::cout << ",\"infix\":";
        m_infix->print();
    }

    if (m_args) {
        std::cout << ",\"arguments\":";
        m_args->print();
    }

    if (m_ret) {
        std::cout << ",\"return\":";
        m_ret->print();
    }

    if (m_preds) {
        std::cout << ",\"predicates\":";
        m_preds->print();
    }

    if (m_exprs) {
        std::cout << ",\"expressions\":";
        m_exprs->print();
    }

    std::cout << "}";
}

void ast_exprs::print() {
    std::cout << "{\"exprs\":";
    PRINTLIST(m_exprs);
    std::cout << "}";
}

void ast_expr_id::print() { std::cout << "{\"id\":\"" << m_id->m_id << "\"}"; }

void ast_apply::print() {
    std::cout << "{\"apply\":{\"func\":";
    m_func->print();
    std::cout << ",\"arguemnts\":";
    PRINTLIST(m_args);
    std::cout << "}}";
}

void ast_if::print() {
    std::cout << "{\"if\":{\"condition\":";
    m_cond->print();
    std::cout << ",\"then\":";
    m_then->print();

    ast_exprs *els = nullptr;
    if (m_elif) {
        std::cout << ",\"elif\":[";
        int n = 0;
        ast_if *ptr;
        for (ptr = m_elif.get();; ptr = ptr->m_elif.get()) {
            if (n > 0)
                std::cout << ",";

            std::cout << "{\"condition\":";
            ptr->m_cond->print();
            std::cout << ",\"then\":";
            ptr->m_then->print();
            std::cout << "}";
            n++;

            if (!ptr->m_elif)
                break;
        }
        std::cout << "]";
        if (ptr->m_else)
            els = ptr->m_else.get();
    } else if (m_else) {
        els = m_else.get();
    }

    if (els != nullptr) {
        std::cout << ",\"else\":";
        els->print();
    }

    std::cout << "}}";
}

void ast_defvar::print() {
    std::cout << "{\"defvar\":{\"id\":";
    m_id->print();
    std::cout << ",\"expression\":";
    m_expr->print();

    if (m_type) {
        std::cout << ",\"type\":";
        m_type->print();
    }

    std::cout << "}}";
}

void ast_defvars::print() {
    std::cout << "{\"defvars\":";
    PRINTLIST(m_defs);
    std::cout << "}";
}

void ast_let::print() {
    std::cout << "{\"let\":{\"defvars\":";
    m_defvars->print();

    if (m_in) {
        std::cout << ",\"in\":";
        m_in->print();
    }

    std::cout << "}}";
}

void ast_tuple::print() {
    std::cout << "{\"tuple\":";
    PRINTLIST(m_exprs);
    std::cout << "}";
}

void ast_vector::print() {
    std::cout << "{\"vector\":";
    PRINTLIST(m_exprs);
    std::cout << "}";
}

void ast_dictelm::print() {
    std::cout << "{\"key\":";
    m_key->print();
    std::cout << ",\"value\":";
    m_val->print();
    std::cout << "}";
}

void ast_dict::print() {
    std::cout << "{\"key\":";
    PRINTLIST(m_elms);
    std::cout << "}";
}

void ast_block::print() {
    std::cout << "{\"block\":";
    PRINTLIST(m_exprs);
    std::cout << "}";
}

void ast_index::print() {
    std::cout << "{\"indexing\":{\"array\":";
    m_array->print();
    std::cout << ",\"index\":";
    m_index->print();
    std::cout << "}}";
}

void ast_binexpr::print() {
    std::cout << "{\"binary expression\":{\"operator\":";
    m_op->print();
    std::cout << ",\"left\":";
    m_left->print();
    std::cout << ",\"right\":";
    m_right->print();
    std::cout << "}}";
}

void ast_num::print() {
    std::cout << "{\"number\":{\"type\":";

    switch (m_numtype) {
    case parsec::NUM_DOUBLE:
        std::cout << "\"double\"";
    case parsec::NUM_FLOAT:
        std::cout << "\"float\"";
    case parsec::NUM_INT:
        std::cout << "\"int\"";
    default:;
    }

    std::cout << ",\"num\":\"" << m_num << "\"}}";
}

void ast_str::print() { std::cout << "{\"string\":\"" << m_str << "\"}"; }

void ast_parenthesis::print() {
    std::cout << "{\"parenthesis\":";
    m_expr->print();
    std::cout << "}";
}

void ast_instance::print() {
    std::cout << "{\"instance\":{\"pred\":";
    m_pred->print();

    if (m_preds) {
        std::cout << ",\"require\":";
        m_preds->print();
    }

    std::cout << ",\"functions\":[";
    int n = 0;
    for (auto &p : m_id2defun) {
        if (n > 0)
            std::cout << ",";
        p.second->print();
        n++;
    }

    std::cout << "]}}";
}

} // namespace lunar
