#include "lunar_ir.hpp"

#include "lunar_string.hpp"

#include <iostream>

#include <boost/algorithm/string.hpp>
#include <boost/lexical_cast.hpp>

#define SYNTAXERR(M)                                                           \
    do {                                                                       \
        fprintf(stderr, "%s:%lu:%lu:(%d) syntax error: " M "\n",               \
                m_filename.c_str(), m_parsec.get_line(),                       \
                m_parsec.get_column(), __LINE__);                              \
        print_err(m_parsec.get_line(), m_parsec.get_column());                 \
    } while (0)

#define SYNTAXERR2(M, LINE, COLUMN)                                            \
    do {                                                                       \
        fprintf(stderr, "%s:%lu:%lu:(%d) syntax error: " M "\n",               \
                m_filename.c_str(), LINE, COLUMN, __LINE__);                   \
        print_err(LINE, COLUMN);                                               \
    } while (0)

#define SEMANTICERR(IR, AST, M, ...)                                           \
    do {                                                                       \
        fprintf(stderr, "%s:%lu:%lu:(%d) semantic error: " M "\n",             \
                (IR).get_filename().c_str(), (AST)->m_line, (AST)->m_column,   \
                __LINE__, ##__VA_ARGS__);                                      \
        (IR).print_err((AST)->m_line, (AST)->m_column);                        \
    } while (0)

#define TYPEERR(IR, AST, M1, M2, ...)                                          \
    do {                                                                       \
        fprintf(stderr, "%s:%lu:%lu:(%d) type error: " M1 "\n",                \
                (IR).get_filename().c_str(), (AST)->m_line, (AST)->m_column,   \
                __LINE__);                                                     \
        (IR).print_err((AST)->m_line, (AST)->m_column);                        \
        fprintf(stderr, M2 "\n", ##__VA_ARGS__);                               \
    } while (0)

#define SPACEPLUS(PARSEC)                                                      \
    do {                                                                       \
        (PARSEC).space();                                                      \
        if ((PARSEC).is_fail()) {                                              \
            SYNTAXERR("expected whitespace");                                  \
            return nullptr;                                                    \
        }                                                                      \
        (PARSEC).spaces();                                                     \
    } while (0)

#define PARSEID(ID, PARSEC)                                                    \
    do {                                                                       \
        (ID) = parse_id();                                                     \
        if ((PARSEC).is_fail()) {                                              \
            SYNTAXERR("expected identifier");                                  \
            return nullptr;                                                    \
        }                                                                      \
    } while (0)

#define PARSELPAREN(PARSEC)                                                    \
    do {                                                                       \
        (PARSEC).character('(');                                               \
        if ((PARSEC).is_fail()) {                                              \
            SYNTAXERR("expected (");                                           \
            return nullptr;                                                    \
        }                                                                      \
    } while (0)

#define PARSERPAREN(PARSEC)                                                    \
    do {                                                                       \
        (PARSEC).spaces();                                                     \
        (PARSEC).character(')');                                               \
        if ((PARSEC).is_fail()) {                                              \
            SYNTAXERR("expected )");                                           \
            return nullptr;                                                    \
        }                                                                      \
    } while (0)

#define TRYRPAREN(PARSEC)                                                      \
    do {                                                                       \
        char tmp;                                                              \
        PTRY(PARSEC, tmp, [](parsec &p) {                                      \
            p.spaces();                                                        \
            return p.character(')');                                           \
        }(PARSEC));                                                            \
    } while (0)

#define TRUEVAL(ctx) llvm::ConstantInt::get(ctx, llvm::APInt(1, 1, false))
#define FALSEVAL(ctx) llvm::ConstantInt::get(ctx, llvm::APInt(1, 0, false))
#define VOIDVAL(ctx) llvm::ConstantInt::get(ctx, llvm::APInt(1, 0, false))
#define I8PTR(ctx) llvm::PointerType::getUnqual(llvm::IntegerType::get(ctx, 8))

namespace lunar {

static bool eq_type(ir_type *lhs, ir_type *rhs) {
    if (lhs->m_irtype != rhs->m_irtype)
        return false;

    switch (lhs->m_irtype) {
    case ir_type::IRTYPE_SCALAR: {
        auto t1 = (const ir_scalar *)lhs;
        auto t2 = (const ir_scalar *)rhs;
        return t1->m_type == t2->m_type;
    }
    case ir_type::IRTYPE_FUN: {
        auto t1 = (const ir_funtype *)lhs;
        auto t2 = (const ir_funtype *)rhs;
        if (t1->m_args.size() != t2->m_args.size())
            return false;

        if (!eq_type(t1->m_ret.get(), t2->m_ret.get()))
            return false;

        for (size_t i = 0; i < t1->m_args.size(); i++) {
            if (!eq_type(t1->m_args[i].get(), t2->m_args[i].get()))
                return false;
        }

        return true;
    }
    case ir_type::IRTYPE_VEC: {
        auto t1 = (const ir_vec *)lhs;
        auto t2 = (const ir_vec *)rhs;
        return eq_type(t1->m_type.get(), t2->m_type.get());
    }
    case ir_type::IRTYPE_REF: {
        auto t1 = (const ir_ref *)lhs;
        auto t2 = (const ir_ref *)rhs;
        return eq_type(t1->m_type.get(), t2->m_type.get());
    }
    case ir_type::IRTYPE_STRUCT: {
        auto t1 = (const ir_struct *)lhs;
        auto t2 = (const ir_struct *)rhs;

        if (t1->m_member.size() != t2->m_member.size())
            return false;

        for (size_t i = 0; i < t1->m_member.size(); i++) {
            if (!eq_type(t1->m_member[i].get(), t2->m_member[i].get()))
                return false;
        }

        return true;
    }
    case ir_type::IRTYPE_UTF8:
        return true;
    case ir_type::IRTYPE_USER:
        return false;
    }
}

static bool unify_type(ir_expr *lhs, ir_expr *rhs) {
    if (lhs->m_type->m_irtype != rhs->m_type->m_irtype)
        return false;

    // unify int type
    switch (lhs->m_type->m_irtype) {
    case ir_type::IRTYPE_SCALAR: {
        auto s1 = (ir_scalar *)(lhs->m_type.get());
        auto s2 = (ir_scalar *)(rhs->m_type.get());
        if (s1->m_type == TYPE_INT) {
            switch (s2->m_type) {
            case TYPE_BOOL:
            case TYPE_FP32:
            case TYPE_FP64:
                return false;
            default:
                s1->m_type = s2->m_type;
                return true;
            }
        } else if (s2->m_type == TYPE_INT) {
            switch (s1->m_type) {
            case TYPE_BOOL:
            case TYPE_FP32:
            case TYPE_FP64:
                return false;
            default:
                s2->m_type = s1->m_type;
                return true;
            }
        } else {
            if (s1->m_type == s2->m_type) {
                lhs->m_type = rhs->m_type;
                return true;
            } else {
                return false;
            }
        }
    }
    case ir_type::IRTYPE_VEC:
    case ir_type::IRTYPE_FUN:
    case ir_type::IRTYPE_REF:
    case ir_type::IRTYPE_STRUCT:
        if (!eq_type(lhs->m_type.get(), rhs->m_type.get()))
            return false;

        lhs->m_type = rhs->m_type;
        return true;
    case ir_type::IRTYPE_UTF8:
        return true;
    case ir_type::IRTYPE_USER:
        return false;
    }

    assert(false);
    return false; // never reach here
}

ir::ir(const std::string &filename, const std::string &str)
    : m_parsec(str), m_filename(filename), m_llvm_builder(m_llvm_ctx),
      m_llvm_module(filename, m_llvm_ctx), m_llvm_datalayout(&m_llvm_module) {

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

    m_0to9.insert('0');
    m_0to9.insert('1');
    m_0to9.insert('2');
    m_0to9.insert('3');
    m_0to9.insert('4');
    m_0to9.insert('5');
    m_0to9.insert('6');
    m_0to9.insert('7');
    m_0to9.insert('8');
    m_0to9.insert('9');

    m_1to9.insert('1');
    m_1to9.insert('2');
    m_1to9.insert('3');
    m_1to9.insert('4');
    m_1to9.insert('5');
    m_1to9.insert('6');
    m_1to9.insert('7');
    m_1to9.insert('8');
    m_1to9.insert('9');

    m_esc_char['a'] = '\a';
    m_esc_char['b'] = '\b';
    m_esc_char['f'] = '\f';
    m_esc_char['r'] = '\r';
    m_esc_char['n'] = '\n';
    m_esc_char['t'] = '\t';
    m_esc_char['v'] = '\v';
    m_esc_char['\\'] = '\\';
    m_esc_char['?'] = '\?';
    m_esc_char['\''] = '\'';
    m_esc_char['\"'] = '"';
    m_esc_char['\0'] = '\0';
    m_esc_char['U'] = 'U';
    m_esc_char['u'] = 'u';

    m_hex2num['0'] = 0;
    m_hex2num['1'] = 1;
    m_hex2num['2'] = 2;
    m_hex2num['3'] = 3;
    m_hex2num['4'] = 4;
    m_hex2num['5'] = 5;
    m_hex2num['6'] = 6;
    m_hex2num['7'] = 7;
    m_hex2num['8'] = 8;
    m_hex2num['9'] = 9;
    m_hex2num['a'] = 10;
    m_hex2num['A'] = 10;
    m_hex2num['b'] = 11;
    m_hex2num['B'] = 11;
    m_hex2num['c'] = 12;
    m_hex2num['C'] = 12;
    m_hex2num['d'] = 13;
    m_hex2num['D'] = 13;
    m_hex2num['e'] = 14;
    m_hex2num['e'] = 14;
    m_hex2num['f'] = 15;
    m_hex2num['F'] = 15;
}

bool ir::parse() {
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
        auto line = m_parsec.get_line();
        auto column = m_parsec.get_column();
        auto id = parse_id();
        if (m_parsec.is_fail()) {
            SYNTAXERR("expected identifier");
            return false;
        }

        if (id == "defun") {
            auto defun = parse_defun();
            if (!defun)
                return false;

            defun->m_line = line;
            defun->m_column = column;
            m_defuns.push_back(std::move(defun));
        } else if (id == "extern") {
            auto extn = parse_extern();
            if (!extn)
                return false;

            extn->m_line = line;
            extn->m_column = column;
            m_externs.push_back(std::move(extn));
        } else if (id == "struct") {
            auto st = parse_defstruct();
            if (!st)
                return false;

            st->m_line = line;
            st->m_column = column;
            m_struct.push_back(std::move(st));
        } else {
            SYNTAXERR("expected defun or struct");
            return false;
        }
    }

    assert(false);
    return false; // never reach here
}

ptr_ir_struct ir::parse_defstruct() {
    SPACEPLUS(m_parsec);

    std::string name;
    PARSEID(name, m_parsec);
    SPACEPLUS(m_parsec);

    auto st = std::make_unique<ir_struct>();

    st->m_name = name;

    int n = 0;
    for (;;) {
        PARSELPAREN(m_parsec);
        m_parsec.spaces();

        auto line = m_parsec.get_line();
        auto column = m_parsec.get_column();

        auto t = parse_reftype();
        if (!t)
            return nullptr;

        SPACEPLUS(m_parsec);
        std::string id;
        PARSEID(id, m_parsec);
        PARSERPAREN(m_parsec);

        if (HASKEY(st->m_id2idx, id)) {
            ir_id i;
            i.m_line = line;
            i.m_column = column;
            SEMANTICERR(*this, &i, "%s is multiply defined", id.c_str());
            return nullptr;
        }

        st->m_id2idx[id] = n;
        st->m_member.push_back(std::move(t));
        n++;

        TRYRPAREN(m_parsec);
        if (!m_parsec.is_fail())
            return st;

        SPACEPLUS(m_parsec);
    }

    assert(false);
    return nullptr; // never reach hare
}

ptr_ir_expr ir::parse_expr() {
    std::string id;
    char tmp;

    auto line = m_parsec.get_line();
    auto column = m_parsec.get_column();

    // DECIMAL
    ptr_ir_decimal dec;
    PTRY(m_parsec, tmp, m_parsec.character('0'));
    if (m_parsec.is_fail()) {
        PTRY(m_parsec, dec, parse_decimal());
    } else {
        dec = std::make_unique<ir_decimal>();
        dec->m_num = "0";
    }

    // FLOATING POINT NUMBER
    if (dec) {
        char dot;
        PTRY(m_parsec, dot, m_parsec.character('.'));
        if (!m_parsec.is_fail()) {
            dec->m_num.push_back(dot);
            auto fptype = parse_float(dec->m_num);
            if (!fptype)
                return nullptr;

            fptype->m_line = line;
            fptype->m_column = column;
            return fptype;
        }

        if (dec) {
            dec->m_line = line;
            dec->m_column = column;
            return dec;
        }
    }

    // string
    PTRY(m_parsec, tmp, m_parsec.character('"'));
    if (!m_parsec.is_fail()) {
        ptr_ir_str str = parse_str();
        if (str) {
            str->m_line = line;
            str->m_column = column;
            return str;
        } else {
            return nullptr;
        }
    }

    PTRY(m_parsec, tmp, m_parsec.character('('));
    if (!m_parsec.is_fail()) {
        line = m_parsec.get_line();
        column = m_parsec.get_column();
        PTRY(m_parsec, id, m_parsec.str("let"));

        // LET
        if (!m_parsec.is_fail()) {
            SPACEPLUS(m_parsec);

            auto let = parse_let();
            if (let) {
                let->m_line = line;
                let->m_column = column;
            }

            return let;
        }

        // VECTOR
        PTRY(m_parsec, id, m_parsec.str("vec"));
        if (!m_parsec.is_fail()) {
            SPACEPLUS(m_parsec);

            auto vec = parse_mkvec();
            if (vec) {
                vec->m_line = line;
                vec->m_column = column;
            }

            return vec;
        }

        // APPLY
        auto apply = std::make_unique<ir_apply>();
        for (;;) {
            TRYRPAREN(m_parsec);
            if (!m_parsec.is_fail()) {
                if (apply->m_expr.size() == 0)
                    apply->m_expr_type = ir_expr::EXPRVOID;
                return apply;
            }

            if (apply->m_expr.size() > 0)
                SPACEPLUS(m_parsec);
            else
                m_parsec.spaces();

            auto e = parse_expr();
            if (!e)
                return nullptr;

            apply->m_line = line;
            apply->m_column = column;
            apply->m_expr.push_back(std::move(e));
        }
    }

    PTRY(m_parsec, id, parse_id());
    if (!m_parsec.is_fail()) {
        // BOOL
        if (id == "true") {
            auto b = std::make_unique<ir_bool>();
            b->m_bool = true;
            b->m_line = line;
            b->m_column = column;
            return b;
        } else if (id == "false") {
            auto b = std::make_unique<ir_bool>();
            b->m_bool = false;
            b->m_line = line;
            b->m_column = column;
            return b;
        }

        // IDENTIFIER
        auto e = std::make_unique<ir_id>();
        e->m_id = id;
        e->m_line = line;
        e->m_column = column;
        return e;
    }

    SYNTAXERR("could not parse expression");
    return nullptr;
}

#define PARSETYPE(ID, STR, FUN)                                                \
    do {                                                                       \
        if (ID == STR) {                                                       \
            auto ret = FUN;                                                    \
            if (!ret)                                                          \
                return nullptr;                                                \
                                                                               \
            ret->m_line = line;                                                \
            ret->m_column = column;                                            \
            return ret;                                                        \
        }                                                                      \
    } while (0)

ptr_ir_type ir::parse_type() {
    auto line = m_parsec.get_line();
    auto column = m_parsec.get_column();

    char tmp;
    PTRY(m_parsec, tmp, m_parsec.character('('));
    if (!m_parsec.is_fail()) {
        m_parsec.spaces();
        std::string id;
        PARSEID(id, m_parsec);

        PARSETYPE(id, "ref", parse_ref());

        if (id == "vec") {
            SYNTAXERR2("vector type must be referred by reference type", line,
                       column);
            return nullptr;
        }

        if (id == "struct") {
            SYNTAXERR2("structure type must be referred by reference type",
                       line, column);
            return nullptr;
        }

        if (id == "fun") {
            SYNTAXERR2("function type must be referred by reference type", line,
                       column);
            return nullptr;
        }

        SYNTAXERR2("expected type specifiler", line, column);
        return nullptr;
    }

    auto s = parse_scalartype();
    if (s->m_irtype == ir_type::IRTYPE_USER) {
        SYNTAXERR2("unexpected type", line, column);
        return nullptr;
    } else if (s->m_irtype == ir_type::IRTYPE_UTF8) {
        SYNTAXERR2("utf8 type must be referred by refernce type", line, column);
        return nullptr;
    }

    if (s) {
        s->m_line = line;
        s->m_column = column;
    }

    return s;
}

ptr_ir_type ir::parse_scalartype() {
    std::string s;
    PARSEID(s, m_parsec);

    // TODO: fix to use a look up table
    if (s == "u64") {
        auto t = new ir_scalar;
        t->m_type = TYPE_U64;
        return ptr_ir_type((ir_type *)t);
    } else if (s == "s64") {
        auto t = new ir_scalar;
        t->m_type = TYPE_S64;
        return ptr_ir_type((ir_type *)t);
    } else if (s == "u32") {
        auto t = new ir_scalar;
        t->m_type = TYPE_U32;
        return ptr_ir_type((ir_type *)t);
    } else if (s == "s32") {
        auto t = new ir_scalar;
        t->m_type = TYPE_S32;
        return ptr_ir_type((ir_type *)t);
    } else if (s == "u16") {
        auto t = new ir_scalar;
        t->m_type = TYPE_U16;
        return ptr_ir_type((ir_type *)t);
    } else if (s == "s16") {
        auto t = new ir_scalar;
        t->m_type = TYPE_S16;
        return ptr_ir_type((ir_type *)t);
    } else if (s == "u8") {
        auto t = new ir_scalar;
        t->m_type = TYPE_U8;
        return ptr_ir_type((ir_type *)t);
    } else if (s == "s8") {
        auto t = new ir_scalar;
        t->m_type = TYPE_S8;
        return ptr_ir_type((ir_type *)t);
    } else if (s == "bool") {
        auto t = new ir_scalar;
        t->m_type = TYPE_BOOL;
        return ptr_ir_type((ir_type *)t);
    } else if (s == "fp64") {
        auto t = new ir_scalar;
        t->m_type = TYPE_FP64;
        return ptr_ir_type((ir_type *)t);
    } else if (s == "fp32") {
        auto t = new ir_scalar;
        t->m_type = TYPE_FP32;
        return ptr_ir_type((ir_type *)t);
    } else if (s == "void") {
        auto t = new ir_scalar;
        t->m_type = TYPE_VOID;
        return ptr_ir_type((ir_type *)t);
    } else if (s == "utf8") {
        auto t = new ir_utf8;
        return ptr_ir_type((ir_type *)t);
    } else {
        auto user = std::make_unique<ir_usertype>();
        user->m_name = s;
        return user;
    }

    assert(false);
    return nullptr; // never reach here
}

ptr_ir_struct ir::parse_struct() {
    SPACEPLUS(m_parsec);

    auto st = std::make_unique<ir_struct>();

    for (;;) {
        auto t = parse_reftype();
        if (!t)
            return nullptr;

        st->m_member.push_back(std::move(t));

        TRYRPAREN(m_parsec);
        if (!m_parsec.is_fail())
            return st;

        SPACEPLUS(m_parsec);
    }

    assert(false);
    return nullptr; // never reach hare
}

ptr_ir_type ir::parse_ref() {
    SPACEPLUS(m_parsec);

    auto t = parse_reftype();
    if (!t)
        return nullptr;

    PARSERPAREN(m_parsec);

    auto ref = std::make_unique<ir_ref>();
    ref->m_type = std::move(t);

    return ref;
}

ptr_ir_type ir::parse_reftype() {
    auto line = m_parsec.get_line();
    auto column = m_parsec.get_column();

    char tmp;
    PTRY(m_parsec, tmp, m_parsec.character('('));
    if (!m_parsec.is_fail()) {
        m_parsec.spaces();

        line = m_parsec.get_line();
        column = m_parsec.get_column();
        std::string id;
        PARSEID(id, m_parsec);

        PARSETYPE(id, "ref", parse_ref());
        PARSETYPE(id, "struct", parse_struct());
        PARSETYPE(id, "vec", parse_vec());
        PARSETYPE(id, "fun", parse_fun());

        SYNTAXERR2("expected type specifier", line, column);
        return nullptr;
    }

    auto s = parse_scalartype();
    if (s) {
        s->m_line = line;
        s->m_column = column;
    }

    return s;
}

ptr_ir_type ir::parse_fun() {
    SPACEPLUS(m_parsec);

    auto ret = parse_type();
    if (!ret)
        return nullptr;

    SPACEPLUS(m_parsec);
    PARSELPAREN(m_parsec);

    auto fun = std::make_unique<ir_funtype>();
    fun->m_ret = std::move(ret);

    for (int n = 0;; n++) {
        TRYRPAREN(m_parsec);
        if (!m_parsec.is_fail())
            break;

        if (n > 0)
            SPACEPLUS(m_parsec);
        else
            m_parsec.spaces();

        auto arg = parse_type();
        if (!arg)
            return nullptr;

        fun->m_args.push_back(std::move(arg));
    }

    PARSERPAREN(m_parsec);

    return fun;
}

ptr_ir_type ir::parse_vec() {
    SPACEPLUS(m_parsec);

    auto t = parse_vectype();
    if (!t)
        return nullptr;

    auto ret = std::make_unique<ir_vec>();
    ret->m_type = std::move(t);

    TRYRPAREN(m_parsec);
    if (!m_parsec.is_fail()) {
        return ret;
    }

    SPACEPLUS(m_parsec);

    auto num = parse_decimal();
    if (!num) {
        SYNTAXERR("expected a decimal number");
        return nullptr;
    }

    ret->m_num = std::move(num);

    PARSERPAREN(m_parsec);

    return ret;
}

ptr_ir_type ir::parse_vectype() {
    auto line = m_parsec.get_line();
    auto column = m_parsec.get_column();

    char tmp;
    PTRY(m_parsec, tmp, m_parsec.character('('));
    if (!m_parsec.is_fail()) {
        m_parsec.spaces();

        line = m_parsec.get_line();
        column = m_parsec.get_column();
        std::string id;
        PARSEID(id, m_parsec);

        PARSETYPE(id, "ref", parse_ref());
        PARSETYPE(id, "struct", parse_struct());

        if (id == "vec") {
            SYNTAXERR2("multi dimentional vector is not supported", line,
                       column);
            return nullptr;
        }

        if (id == "fun") {
            SYNTAXERR2("function vector is not supported. use (ref func * *) "
                       "type for vector",
                       line, column);
            return nullptr;
        }

        SYNTAXERR2("expected type specifier", line, column);
        return nullptr;
    }

    auto s = parse_scalartype();
    if (s) {
        s->m_line = line;
        s->m_column = column;
    }

    return s;
}

ptr_ir_defun ir::parse_defun() {
    SPACEPLUS(m_parsec);

    // parse function name
    ptr_ir_defun defun(new ir_defun);
    std::string id;
    PARSEID(id, m_parsec);
    defun->m_name = id;

    SPACEPLUS(m_parsec);

    // parse return types
    auto t = parse_type();
    if (!t)
        return nullptr;

    defun->m_ret = std::move(t);

    SPACEPLUS(m_parsec);

    // parse arguments
    PARSELPAREN(m_parsec);

    int n = 0;
    for (;;) {
        TRYRPAREN(m_parsec);
        if (m_parsec.is_fail()) {
            if (n > 0)
                SPACEPLUS(m_parsec);
            else
                m_parsec.spaces();

            PARSELPAREN(m_parsec);
            m_parsec.spaces();

            auto t = parse_type();
            if (!t)
                return nullptr;

            SPACEPLUS(m_parsec);

            std::string id;
            PARSEID(id, m_parsec);

            auto p = std::make_unique<ir_id>();
            p->m_id = id;
            p->m_type = std::move(t);
            defun->m_args.push_back(std::move(p));

            PARSERPAREN(m_parsec);
        } else {
            break;
        }
    }

    SPACEPLUS(m_parsec);

    // parse expression
    auto expr = parse_expr();
    if (!expr)
        return nullptr;

    PARSERPAREN(m_parsec);

    defun->m_expr = std::move(expr);

    return defun;
}

ptr_ir_extern ir::parse_extern() {
    SPACEPLUS(m_parsec);

    // parse function name
    ptr_ir_extern extn(new ir_extern);

    std::string id;
    PARSEID(id, m_parsec);
    extn->m_name = id;

    SPACEPLUS(m_parsec);

    // parse return types
    auto t = parse_type();
    if (!t)
        return nullptr;

    extn->m_ret = std::move(t);

    SPACEPLUS(m_parsec);

    // parse arguments
    PARSELPAREN(m_parsec);

    int n = 0;
    for (;;) {
        TRYRPAREN(m_parsec);
        if (m_parsec.is_fail()) {
            if (n > 0)
                SPACEPLUS(m_parsec);
            else
                m_parsec.spaces();

            auto t = parse_type();
            if (!t)
                return nullptr;

            extn->m_args.push_back(std::move(t));
            n++;
        } else {
            break;
        }
    }

    PARSERPAREN(m_parsec);

    return extn;
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

ptr_ir_str ir::parse_str() {
    std::string ret;

    for (;;) {
        char c = m_parsec.any();
        if (m_parsec.is_fail()) {
            return nullptr;
        }

        if (c == '\"')
            break;

        if (c == '\\') {
            // parse escape character
            char esc = m_parsec.satisfy(
                [&](char c) { return m_esc_char.find(c) != m_esc_char.end(); });

            if (m_parsec.is_fail()) {
                SYNTAXERR("unexpected character");
                return nullptr;
            }

            if (esc == 'u' || esc == 'U') {
                for (int i = 0; i < (esc == 'u') ? 2 : 4; i++) {
                    char h0 = m_parsec.hex();
                    if (m_parsec.is_fail()) {
                        SYNTAXERR("unexpected character");
                        return nullptr;
                    }

                    char h1 = m_parsec.hex();
                    if (m_parsec.is_fail()) {
                        SYNTAXERR("unexpected character");
                        return nullptr;
                    }

                    ret.push_back(m_hex2num[h0] << 4 | m_hex2num[h1]);
                }
            } else {
                ret.push_back(m_esc_char[esc]);
            }
        } else {
            ret.push_back(c);
        }
    }

    auto ptr = std::make_unique<ir_str>();
    ptr->m_str = ret;

    return ptr;
}

ptr_ir_decimal ir::parse_decimal() {
    std::string num;
    auto line = m_parsec.get_line();
    auto column = m_parsec.get_column();

    char minus;
    PTRY(m_parsec, minus, m_parsec.character('-'));
    if (!m_parsec.is_fail())
        num.push_back(minus);

    char c = m_parsec.oneof(m_1to9);
    if (m_parsec.is_fail())
        return nullptr;

    num.push_back(c);
    PMANY(m_parsec, num, m_parsec.oneof(m_0to9));

    auto d = std::make_unique<ir_decimal>();
    d->m_num = num;
    d->m_line = line;
    d->m_column = column;

    return d;
}

ptr_ir_float ir::parse_float(std::string num) {
    char c;

    PMANYONE(m_parsec, num, m_parsec.oneof(m_0to9));
    if (m_parsec.is_fail()) {
        SYNTAXERR("must be a decimal number");
        return nullptr;
    }

    PTRY(m_parsec, c, m_parsec.character('e'));
    if (!m_parsec.is_fail()) {
        num.push_back(c);
        c = m_parsec.satisfy([](char a) { return a == '+' || a == '-'; });
        if (m_parsec.is_fail()) {
            SYNTAXERR("must be + or -");
            return nullptr;
        }

        num.push_back(c);

        PMANYONE(m_parsec, num, m_parsec.oneof(m_0to9));
        if (m_parsec.is_fail()) {
            SYNTAXERR("must be a decimal number");
            return nullptr;
        }
    }

    ptr_ir_float fptype = std::make_unique<ir_float>();

    PTRY(m_parsec, c, m_parsec.character('f'));
    if (!m_parsec.is_fail())
        fptype->m_is_double = false;

    fptype->m_num = num;

    return fptype;
}

ptr_ir_let ir::parse_let() {
    m_parsec.spaces();

    // parse definitions
    PARSELPAREN(m_parsec);

    auto let = std::make_unique<ir_let>();

    for (;;) {
        if (let->m_def.size() > 0)
            SPACEPLUS(m_parsec);
        else
            m_parsec.spaces();

        PARSELPAREN(m_parsec);
        m_parsec.spaces();

        auto type = parse_type();
        if (!type)
            return nullptr;

        SPACEPLUS(m_parsec);

        std::string id;
        PARSEID(id, m_parsec);

        SPACEPLUS(m_parsec);

        auto e = parse_expr();
        if (!e)
            return nullptr;

        PARSERPAREN(m_parsec);

        auto p = std::make_unique<ir_let::var>();
        p->m_id = std::make_unique<ir_id>();
        p->m_id->m_id = id;
        p->m_id->m_type = std::shared_ptr<ir_type>(type->clone());
        p->m_expr = std::move(e);
        let->m_def.push_back(std::move(p));

        TRYRPAREN(m_parsec);
        if (!m_parsec.is_fail())
            break;
    }

    SPACEPLUS(m_parsec);

    auto e = parse_expr();
    if (!e)
        return nullptr;

    let->m_expr = std::move(e);

    PARSERPAREN(m_parsec);

    return let;
}

ptr_ir_mkvec ir::parse_mkvec() {
    auto t = parse_vectype();
    if (!t)
        return nullptr;

    SPACEPLUS(m_parsec);

    auto e = parse_expr();
    if (!e)
        return nullptr;

    PARSERPAREN(m_parsec);

    auto mkvec = std::make_unique<ir_mkvec>();
    mkvec->m_vectype = std::move(t);
    mkvec->m_num = std::move(e);

    return mkvec;
}

std::string ir_struct::str() const {
    std::string s = "(struct";
    for (auto &p : m_member) {
        s += " ";
        s += p->str();
    }
    s += ")";
    return s;
}

std::string ir_scalar::str() const {
    switch (m_type) {
    case TYPE_BOOL:
        return "bool";
    case TYPE_FP64:
        return "fp64";
    case TYPE_FP32:
        return "fp32";
    case TYPE_U64:
        return "u64";
    case TYPE_S64:
        return "s64";
    case TYPE_U32:
        return "u32";
    case TYPE_S32:
        return "s32";
    case TYPE_U16:
        return "u16";
    case TYPE_S16:
        return "s16";
    case TYPE_U8:
        return "u8";
    case TYPE_S8:
        return "s8";
    case TYPE_INT:
        return "int";
    case TYPE_VOID:
        return "void";
    case TYPE_VEC:
    case TYPE_REF:
    case TYPE_STRUCT:
    case TYPE_FUN:
    case TYPE_UTF8:
        assert(m_type != TYPE_REF && m_type != TYPE_STRUCT &&
               m_type != TYPE_FUN && m_type != TYPE_UTF8);
        return "";
    }
}

std::string ir_funtype::str() const {
    std::string ret;
    int n = 0;
    for (auto &p : m_args) {
        if (n > 0)
            ret += ",";
        ret += p->str();
    }

    ret += " -> ";
    ret += m_ret->str();

    return ret;
}

std::string ir_ref::str() const { return "(ref " + m_type->str() + ")"; }

std::string ir_utf8::str() const { return "utf8"; }

std::string ir_vec::str() const { return "(vec " + m_type->str() + ")"; }

bool ir::check_type() {
    add_builtin();

    for (auto &s : m_struct) {
        if (HASKEY(m_id2struct, s->m_name)) {
            SEMANTICERR(*this, s.get(), "%s is multiply defined",
                        s->m_name.c_str());
            return false;
        }
        m_id2struct[s->m_name] =
            std::shared_ptr<ir_struct>((ir_struct *)s->clone());
    }

    for (auto &s : m_struct) {
        std::unordered_set<std::string> used;
        used.insert(s->m_name);
        if (!check_recursive(s.get(), used))
            return false;
    }

    for (auto &s : m_id2struct)
        s.second = std::static_pointer_cast<ir_struct>(resolve_type(s.second));

    for (auto &p : m_defuns) {
        if (HASKEY(m_id2fun, p->m_name) || HASKEY(m_id2struct, p->m_name)) {
            SEMANTICERR(*this, p.get(), "%s is multiply defined",
                        p->m_name.c_str());
            return false;
        }

        p->resolve_funtype();
        m_id2fun[p->m_name] = p->m_funtype;
    }

    for (auto &p : m_externs) {
        if (HASKEY(m_id2fun, p->m_name) || HASKEY(m_id2struct, p->m_name)) {
            SEMANTICERR(*this, p.get(), "%s is multiply defined",
                        p->m_name.c_str());
            return false;
        }

        p->resolve_funtype();
        m_id2fun[p->m_name] = p->m_funtype;
    }

    for (auto &p : m_defuns) {
        if (p->m_ret->m_irtype == ir_type::IRTYPE_STRUCT) {
            std::unordered_set<std::string> used;
            check_recursive((ir_struct *)p->m_ret.get(), used);
        }

        if (!p->check_type(*this)) {
            return false;
        }

        if (p->m_name == "main") {
            if (p->m_ret->m_irtype != ir_type::IRTYPE_SCALAR) {
                SEMANTICERR(*this, p, "main function must be void type");
                return false;
            }

            ir_scalar *s = (ir_scalar *)p->m_ret.get();
            if (s->m_type != TYPE_VOID) {
                SEMANTICERR(*this, p, "main function must be void type");
                return false;
            }

            if (p->m_args.size() > 0) {
                SEMANTICERR(*this, p,
                            "main function must not take any arguments");
                return false;
            }
        }
    }

    for (auto &p : m_externs) {
        if (p->m_ret->m_irtype == ir_type::IRTYPE_STRUCT) {
            std::unordered_set<std::string> used;
            check_recursive((ir_struct *)p->m_ret.get(), used);
        }

        if (!p->check_type(*this)) {
            return false;
        }
    }

    return true;
}

static std::unique_ptr<ir_extern> mk_extern(const std::string &name,
                                            type_spec type) {
    auto extn = std::make_unique<ir_extern>(false);
    extn->m_name = name;

    auto t = std::make_unique<ir_scalar>();
    t->m_type = type;
    extn->m_ret = std::move(t);

    return extn;
}

static void append_arg(ir_extern *extn, type_spec type) {
    auto arg = std::make_unique<ir_scalar>();
    arg->m_type = type;
    extn->m_args.push_back(std::move(arg));
}

static ptr_ir_scalar mk_voidty() {
    auto voidty = std::make_unique<ir_scalar>();
    voidty->m_type = TYPE_VOID;
    return voidty;
}

static ptr_ir_ref mk_refvoid() {
    auto ref = std::make_unique<ir_ref>();
    auto u8ptr = std::make_unique<ir_scalar>();
    u8ptr->m_type = TYPE_U8;
    ref->m_type = std::move(u8ptr);
    return ref;
}

void ir::add_builtin() {
    // TODO: add built-in functions
    auto print_unum = mk_extern("print_unum", TYPE_VOID);
    append_arg(print_unum.get(), TYPE_U64);
    m_externs.push_back(std::move(print_unum));

    auto print_snum = mk_extern("print_snum", TYPE_VOID);
    append_arg(print_snum.get(), TYPE_S64);
    m_externs.push_back(std::move(print_snum));

    auto print_boolean = mk_extern("print_boolean", TYPE_VOID);
    append_arg(print_boolean.get(), TYPE_BOOL);
    m_externs.push_back(std::move(print_boolean));

    auto print_fp32 = mk_extern("print_fp32", TYPE_VOID);
    append_arg(print_fp32.get(), TYPE_FP32);
    m_externs.push_back(std::move(print_fp32));

    auto print_fp64 = mk_extern("print_fp64", TYPE_VOID);
    append_arg(print_fp64.get(), TYPE_FP64);
    m_externs.push_back(std::move(print_fp64));

    auto print_utf8 = mk_extern("print_utf8", TYPE_VOID);
    print_utf8->m_args.push_back(mk_refvoid());
    m_externs.push_back(std::move(print_utf8));

    auto print_ptr = mk_extern("print_ptr", TYPE_VOID);
    print_ptr->m_args.push_back(mk_refvoid());
    m_externs.push_back(std::move(print_ptr));

    auto print_flush = mk_extern("print_flush", TYPE_VOID);
    m_externs.push_back(std::move(print_flush));

    auto print_endl = mk_extern("print_endl", TYPE_VOID);
    m_externs.push_back(std::move(print_endl));

    auto init_thread = mk_extern("init_thread", TYPE_VOID);
    m_externs.push_back(std::move(init_thread));

    auto run_green_thread = mk_extern("run_green_thread", TYPE_VOID);
    m_externs.push_back(std::move(run_green_thread));

    auto yield_green_thread = mk_extern("yield_green_thread", TYPE_VOID);
    m_externs.push_back(std::move(yield_green_thread));

    // void spawn_green_thread(void (*func)(void *), void *arg,
    //                         uint32_t stack_size = 4096 * 32);
    auto spawn_green_thread = mk_extern("spawn_green_thread", TYPE_VOID);
    auto fun = std::make_unique<ir_funtype>();
    fun->m_ret = mk_voidty();
    fun->m_args.push_back(mk_refvoid());
    spawn_green_thread->m_args.push_back(std::move(fun));
    spawn_green_thread->m_args.push_back(mk_refvoid());
    append_arg(spawn_green_thread.get(), TYPE_U32);
    m_externs.push_back(std::move(spawn_green_thread));
}

shared_ir_type ir::resolve_type(shared_ir_type type) const {
    switch (type->m_irtype) {
    case ir_type::IRTYPE_SCALAR:
    case ir_type::IRTYPE_UTF8:
        return type;
    case ir_type::IRTYPE_STRUCT: {
        auto p = (ir_struct *)type.get();
        for (auto &q : p->m_member) {
            auto t = resolve_type(q);
            if (t)
                q = t;
            else
                return nullptr;
        }

        return type;
    }
    case ir_type::IRTYPE_REF: {
        auto p = (ir_ref *)type.get();
        p->m_type = resolve_type(p->m_type);
        return type;
    }
    case ir_type::IRTYPE_VEC: {
        auto p = (ir_vec *)type.get();
        p->m_type = resolve_type(p->m_type);
        return type;
    }
    case ir_type::IRTYPE_USER: {
        auto p = (ir_usertype *)type.get();
        auto it = m_id2struct.find(p->m_name);
        if (it == m_id2struct.end()) {
            SEMANTICERR(*this, type, "%s is undefined", p->m_name.c_str());
            return nullptr;
        }
        return shared_ir_type(std::static_pointer_cast<ir_type>(it->second));
    }
    case ir_type::IRTYPE_FUN: {
        auto p = (ir_funtype *)type.get();
        auto t = resolve_type(p->m_ret);
        if (t)
            p->m_ret = t;
        else
            return nullptr;

        for (auto &q : p->m_args) {
            auto u = resolve_type(q);
            if (u)
                q = u;
            else
                return nullptr;
        }

        return type;
    }
    }

    return nullptr;
}

void ir_defun::resolve_funtype() {
    m_funtype = std::make_shared<ir_funtype>();
    m_funtype->m_ret = shared_ir_type(m_ret->clone());

    for (auto &q : m_args) {
        m_funtype->m_args.push_back(shared_ir_type(q->m_type->clone()));
    }
}

void ir_extern::resolve_funtype() {
    m_funtype = std::make_shared<ir_funtype>();
    m_funtype->m_ret = shared_ir_type(m_ret->clone());

    for (auto &q : m_args) {
        m_funtype->m_args.push_back(shared_ir_type(q->clone()));
    }
}

bool ir::check_recursive(ir_struct *p, std::unordered_set<std::string> &used) {
    for (auto &q : p->m_member) {
        if (q->m_irtype == ir_type::IRTYPE_USER) {
            auto u = (ir_usertype *)q.get();
            if (HASKEY(used, u->m_name)) {
                SEMANTICERR(*this, q.get(), "%s is recusively defined",
                            u->m_name.c_str());
                return false;
            }
            used.insert(u->m_name);
            auto it = m_id2struct.find(u->m_name);
            if (it == m_id2struct.end()) {
                SEMANTICERR(*this, q.get(), "%s is undefined",
                            u->m_name.c_str());
                return false;
            }

            if (!check_recursive(it->second.get(), used))
                return false;

            q = std::shared_ptr<ir_type>(
                std::static_pointer_cast<ir_type>(it->second));
        }
    }

    return true;
}

static bool check_refvec(const ir &ref, ir_type *type) {
    if (type->m_irtype == ir_type::IRTYPE_REF) {
        auto r = (ir_ref *)type;
        if (r->m_type->m_irtype == ir_type::IRTYPE_VEC) {
            auto v = (ir_vec *)r->m_type.get();
            if (v->m_num) {
                SEMANTICERR(
                    ref, v->m_num,
                    "the number of elements could not be specified here");
                return false;
            }
        }
    }

    return true;
}

bool ir_defun::check_type(const ir &ref) {
    ir_expr::id2type vars;

    m_funtype =
        std::static_pointer_cast<ir_funtype>(ref.resolve_type(m_funtype));
    if (!m_funtype)
        return false;

    int n = 0;
    for (auto &p : m_args) {
        if (!check_refvec(ref, p->m_type.get()))
            return false;
        vars[p->m_id].push_back(m_funtype->m_args[n]);
        n++;
    }

    if (!m_expr->check_type(ref, vars))
        return false;

    if (!check_refvec(ref, m_funtype->m_ret.get()))
        return false;

    auto ret = std::make_unique<ir_id>();
    ret->m_type = m_funtype->m_ret;

    if (!unify_type(ret.get(), m_expr.get())) {
        TYPEERR(ref, m_expr, "unexpected type",
                "    expected: %s\n"
                "    actual: %s",
                ret->m_type->str().c_str(), m_expr->m_type->str().c_str());
        return false;
    }

    return true;
}

bool ir_extern::check_type(const ir &ref) {
    ir_expr::id2type vars;

    m_funtype =
        std::static_pointer_cast<ir_funtype>(ref.resolve_type(m_funtype));
    if (!m_funtype)
        return false;

    return true;
}

shared_ir_type ir_id::check_type(const ir &ref, id2type &vars) {
    auto it = vars.find(m_id);
    if (it == vars.end()) {
        SEMANTICERR(ref, this, "%s is undefined", m_id.c_str());
        return nullptr;
    }

    assert(it->second.size() > 0);

    m_type = it->second.back();
    return m_type;
}

shared_ir_type ir_decimal::check_type(const ir &ref, id2type &vars) {
    auto p = new ir_scalar;
    p->m_type = TYPE_INT;

    m_type = shared_ir_type(p);

    return m_type;
}

shared_ir_type ir_float::check_type(const ir &ref, id2type &vars) {
    auto p = new ir_scalar;

    if (m_is_double)
        p->m_type = TYPE_FP64;
    else
        p->m_type = TYPE_FP32;

    m_type = shared_ir_type(p);

    return m_type;
}

shared_ir_type ir_str::check_type(const ir &ref, id2type &vars) {
    auto ret = std::make_shared<ir_ref>();
    ret->m_type = std::make_shared<ir_utf8>();
    m_type = ret;
    return m_type;
}

shared_ir_type ir_bool::check_type(const ir &ref, id2type &vars) {
    auto p = new ir_scalar;
    p->m_type = TYPE_BOOL;

    m_type = shared_ir_type(p);

    return m_type;
}

shared_ir_type ir_let::check_type(const ir &ref, id2type &vars) {
    // check the type of the variable definitions
    for (auto &p : m_def) {
        p->m_id->m_type = ref.resolve_type(p->m_id->m_type);
        if (!p->m_id->m_type)
            return nullptr;

        if (!p->m_expr->check_type(ref, vars))
            return nullptr;

        if (p->m_expr->m_type->m_irtype == ir_type::IRTYPE_STRUCT) {
            auto r = std::make_shared<ir_ref>();
            r->m_type = p->m_expr->m_type;
            p->m_expr->m_type = r;
        }

        if (!check_refvec(ref, p->m_id->m_type.get()))
            return nullptr;

        // this may not be necessary
        // p->m_expr->m_type = ref.resolve_type(p->m_expr->m_type);

        if (!unify_type(p->m_id.get(), p->m_expr.get())) {
            TYPEERR(ref, p->m_expr, "unexpected type",
                    "    expected: %s\n"
                    "    actual: %s",
                    p->m_id->m_type->str().c_str(),
                    p->m_expr->m_type->str().c_str());
            return nullptr;
        }

        vars[p->m_id->m_id].push_back(p->m_id->m_type);
    }

    // check the type of the expression
    auto t = m_expr->check_type(ref, vars);

    if (!t)
        return nullptr;

    for (auto &p : m_def) {
        auto it = vars.find(p->m_id->m_id);
        it->second.pop_back();
    }

    m_type = t;

    return t;
}

shared_ir_type ir_mkvec::check_type(const ir &ref, id2type &vars) {
    // check the type of vector
    auto t = std::shared_ptr<ir_type>(m_vectype->clone());
    t = ref.resolve_type(t);

    auto v = std::make_shared<ir_vec>();
    v->m_type = t;

    auto reftype = std::make_shared<ir_ref>();
    reftype->m_type = v;

    m_type = reftype;

    // check whether the type of the expression indicating the number of
    // elements is u64
    auto e = m_num->check_type(ref, vars);
    if (!e)
        return nullptr;

    ir_id u64expr;
    auto u64type = std::make_shared<ir_scalar>();
    u64type->m_type = TYPE_U64;
    u64expr.m_type = u64type;

    if (!unify_type(&u64expr, m_num.get())) {
        TYPEERR(ref, m_num, "unexpected type",
                "    expected: u64\n"
                "    actual: %s",
                e->str().c_str());
    }

    return reftype;
}

shared_ir_type ir_apply::check_type(const ir &ref, id2type &vars) {
    if (m_expr_type == EXPRVOID) {
        auto v = std::make_shared<ir_scalar>();
        v->m_type = TYPE_VOID;
        m_type = v;
        return v;
    }

    auto first = m_expr.begin();
    auto id = (ir_id *)(first->get());
    if ((*first)->m_expr_type == ir_expr::EXPRID) {
        if (id->m_id.size() == 1) {
            switch (id->m_id[0]) {
            case '+':
            case '-':
            case '*':
            case '/':
                if (m_expr.size() < 3) {
                    SEMANTICERR(ref, this,
                                "%s requires more than or equal to 2 arguments",
                                id->m_id.c_str());
                    return nullptr;
                }

                if (!m_expr[1]->check_type(ref, vars))
                    return nullptr;

                for (size_t i = 2; i < m_expr.size(); i++) {
                    if (!m_expr[i]->check_type(ref, vars))
                        return nullptr;

                    if (!unify_type(m_expr[i - 1].get(), m_expr[i].get())) {
                        TYPEERR(ref, m_expr[i].get(), "unexpected type",
                                "    expected: %s\n"
                                "    actual: %s",
                                m_expr[i - 1]->m_type->str().c_str(),
                                m_expr[i]->m_type->str().c_str());
                        return nullptr;
                    }
                }

                m_type = m_expr[1]->m_type;
                return m_type;
            case '<':
            case '>':
                return check_magnitude(ref, vars, id->m_id);
            case '=':
                return check_eq(ref, vars);
            default:
                return check_call(ref, vars, id->m_id);
            }
        } else if (id->m_id == "if") {
            return check_ifexpr(ref, vars);
        } else if (id->m_id == ">=" || id->m_id == "<=") {
            return check_magnitude(ref, vars, id->m_id);
        } else if (id->m_id == "!=") {
            return check_eq(ref, vars);
        } else if (id->m_id == "print") {
            return check_print(ref, vars);
        } else if (id->m_id == "elm") {
            return check_elm(ref, vars);
        } else if (id->m_id == "load") {
            // TODO
            return check_load(ref, vars);
        } else if (id->m_id == "store") {
            // TODO
        } else {
            // function call
            return check_call(ref, vars, id->m_id);
        }
    }

    // lambda or higher order function

    return nullptr;
}

shared_ir_type ir_apply::check_print(const ir &ref, id2type &vars) {
    for (auto it = m_expr.begin() + 1; it != m_expr.end(); ++it) {
        if (!(*it)->check_type(ref, vars))
            return nullptr;
    }

    auto v = std::make_shared<ir_scalar>();
    v->m_type = TYPE_VOID;
    m_type = v;

    return v;
}

shared_ir_type ir_apply::check_elm(const ir &ref, id2type &vars) {
    if (m_expr.size() != 3) {
        SEMANTICERR(ref, this, "elm requires exactly 2 arguments");
        return nullptr;
    }

    auto type = m_expr[1]->check_type(ref, vars);
    if (!type)
        return nullptr;

    if (type->m_irtype != ir_type::IRTYPE_REF) {
        SEMANTICERR(ref, m_expr[1],
                    "elm requires a reference type at the first argument");
        return nullptr;
    }

    auto reftype = (ir_ref *)type.get();
    switch (reftype->m_type->m_irtype) {
    case ir_type::IRTYPE_STRUCT: {
        if (m_expr[2]->m_expr_type != EXPRID) {
            SEMANTICERR(ref, m_expr[2],
                        "elm requires an identifier at the second argument for "
                        "structure type");
            return nullptr;
        }
        auto id = (ir_id *)m_expr[2].get();
        auto st = (ir_struct *)reftype->m_type.get();

        auto it = st->m_id2idx.find(id->m_id);
        if (it == st->m_id2idx.end()) {
            SEMANTICERR(ref, m_expr[2],
                        "%s is a not member of the first argument",
                        id->m_id.c_str());
            return nullptr;
        }

        auto idx = it->second;
        auto ret = std::make_shared<ir_ref>();
        ret->m_type = st->m_member[idx];

        m_type = ret;

        return ret;
    }
    case ir_type::IRTYPE_VEC: {
        auto d = m_expr[2]->check_type(ref, vars);
        if (d->m_irtype == ir_type::IRTYPE_SCALAR) {
            auto num = (ir_scalar *)d.get();
            if (num->m_type == TYPE_S64 || num->m_type == TYPE_INT) {
                auto ret = std::make_shared<ir_ref>();
                auto vec = (ir_vec *)reftype->m_type.get();

                ret->m_type = vec->m_type;
                m_type = ret;

                return ret;
            }
        }
        SEMANTICERR(
            ref, m_expr[2],
            "elm requires a value of s64 type at the second argument for "
            "vector type");
        return nullptr;
    }
    default:
        SEMANTICERR(ref, m_expr[1],
                    "elm requires a reference type of structure or vector at "
                    "the first argument");
        return nullptr;
    }

    return nullptr;
}

shared_ir_type ir_apply::check_load(const ir &ref, id2type &vars) {
    if (m_expr.size() != 2) {
        SEMANTICERR(ref, this, "elm requires exactly an argument");
        return nullptr;
    }
    auto type = m_expr[1]->check_type(ref, vars);
    if (!type)
        return nullptr;

    if (type->m_irtype != ir_type::IRTYPE_REF) {
        TYPEERR(ref, m_expr[1], "unexpected type",
                "    expected: (ref *)\n"
                "    actual: %s",
                type->str().c_str());
        return nullptr;
    }

    auto r = (ir_ref *)type.get();
    switch (r->m_type->m_irtype) {
    case ir_type::IRTYPE_UTF8:
    case ir_type::IRTYPE_VEC:
    case ir_type::IRTYPE_STRUCT:
    case ir_type::IRTYPE_USER:
        TYPEERR(ref, m_expr[1], "unexpected type",
                "    expected: (ref [scalar|(ref *)|(fun * *)])\n"
                "    actual: %s",
                type->str().c_str());
        return nullptr;
    default:
        m_type = r->m_type;
        return m_type;
    }
}

shared_ir_type ir_apply::check_call(const ir &ref, id2type &vars,
                                    const std::string &id) {
    auto fun = ref.get_funs().find(id);
    auto s = ref.get_id2struct().find(id);

    if (fun == ref.get_funs().end() && s == ref.get_id2struct().end()) {
        SEMANTICERR(ref, this, "%s is undefined", id.c_str());
        return nullptr;
    }

    if (fun != ref.get_funs().end()) {
        // function call
        if (m_expr.size() - 1 != fun->second->m_args.size()) {
            SEMANTICERR(ref, this,
                        "%s requres %lu arguments, but %lu arguments are "
                        "actually passed",
                        id.c_str(), fun->second->m_args.size(),
                        m_expr.size() - 1);
            return nullptr;
        }

        int n = 0;
        for (auto it = m_expr.begin() + 1; it != m_expr.end(); ++it, n++) {
            if (!(*it)->check_type(ref, vars))
                return nullptr;

            if ((*it)->m_type->m_irtype == ir_type::IRTYPE_STRUCT) {
                auto r = std::make_shared<ir_ref>();
                r->m_type = (*it)->m_type;
                (*it)->m_type = r;
            }

            // check types of the definition and actually passed
            ir_id tmp;
            tmp.m_type = shared_ir_type(fun->second->m_args[n]->clone());
            if (!unify_type(&tmp, it->get())) {
                return nullptr;
            }
        }

        m_type = shared_ir_type(fun->second->m_ret->clone());

        return m_type;
    } else {
        // structure construction
        if (m_expr.size() - 1 != s->second->m_member.size()) {
            SEMANTICERR(ref, this,
                        "%s requres %lu arguments, but %lu arguments are "
                        "actually passed",
                        id.c_str(), s->second->m_member.size(),
                        m_expr.size() - 1);
            return nullptr;
        }

        int n = 0;
        for (auto it = m_expr.begin() + 1; it != m_expr.end(); ++it, n++) {
            if (!(*it)->check_type(ref, vars))
                return nullptr;

            auto &member = s->second->m_member[n];

            if (member->m_irtype == ir_type::IRTYPE_USER) {
                auto user = (ir_usertype *)member.get();
                auto it = ref.get_id2struct().find(user->m_name);
                assert(it != ref.get_id2struct().end());
                s->second->m_member[n] =
                    std::static_pointer_cast<ir_type>(it->second);
            }

            ir_id tmp;
            if (member->m_irtype == ir_type::IRTYPE_VEC) {
                auto r = std::make_shared<ir_ref>();
                r->m_type = shared_ir_type(s->second->m_member[n]);
                tmp.m_type = r;
            } else {
                tmp.m_type = shared_ir_type(s->second->m_member[n]);
            }

            if (!unify_type(&tmp, it->get())) {
                TYPEERR(ref, it->get(), "unexpected type",
                        "    expected: %s\n"
                        "    actual: %s",
                        tmp.m_type->str().c_str(),
                        (*it)->m_type->str().c_str());
                return nullptr;
            }
        }

        m_type = shared_ir_type(s->second->clone());
        return m_type;
    }
}

shared_ir_type ir_apply::check_eq(const ir &ref, id2type &vars) {
    if (m_expr.size() != 3) {
        SEMANTICERR(ref, this, "= requires exactly 2 arguments");
        return nullptr;
    }

    if (!m_expr[1]->check_type(ref, vars))
        return nullptr;

    if (m_expr[1]->m_type->m_irtype != ir_type::IRTYPE_SCALAR) {
        TYPEERR(ref, m_expr[1].get(), "unexpected type",
                "    expected: boolean, interger or floating point type\n"
                "    actual: %s",
                m_expr[1]->m_type->str().c_str());
        return nullptr;
    }

    if (!m_expr[2]->check_type(ref, vars))
        return nullptr;

    if (m_expr[2]->m_type->m_irtype != ir_type::IRTYPE_SCALAR) {
        TYPEERR(ref, m_expr[2].get(), "unexpected type",
                "    expected: boolean, interger or floating point type\n"
                "    actual: %s",
                m_expr[1]->m_type->str().c_str());
        return nullptr;
    }

    if (!unify_type(m_expr[1].get(), m_expr[2].get())) {
        TYPEERR(ref, m_expr[2].get(), "unexpected type",
                "    expected: %s\n"
                "    actual: %s",
                m_expr[1]->m_type->str().c_str(),
                m_expr[2]->m_type->str().c_str());
        return nullptr;
    }

    auto t = std::make_shared<ir_scalar>();
    t->m_type = TYPE_BOOL;
    m_type = t;

    return m_type;
}

shared_ir_type ir_apply::check_magnitude(const ir &ref, id2type &vars,
                                         const std::string &id) {
    if (m_expr.size() != 3) {
        SEMANTICERR(ref, this, "%s requires exactly 2 arguments", id.c_str());
        return nullptr;
    }
    if (!m_expr[1]->check_type(ref, vars))
        return nullptr;

    if (m_expr[1]->m_type->m_irtype != ir_type::IRTYPE_SCALAR ||
        ((ir_scalar *)m_expr[1]->m_type.get())->m_type == TYPE_BOOL) {
        TYPEERR(ref, m_expr[1].get(), "unexpected type",
                "    expected: interger or floating point type\n"
                "    actual: %s",
                m_expr[1]->m_type->str().c_str());
        return nullptr;
    }

    if (!m_expr[2]->check_type(ref, vars))
        return nullptr;

    if (m_expr[2]->m_type->m_irtype != ir_type::IRTYPE_SCALAR ||
        ((ir_scalar *)m_expr[2]->m_type.get())->m_type == TYPE_BOOL) {
        TYPEERR(ref, m_expr[2].get(), "unexpected type",
                "    expected: interger or floating point type\n"
                "    actual: %s",
                m_expr[2]->m_type->str().c_str());
        return nullptr;
    }

    if (!unify_type(m_expr[1].get(), m_expr[2].get())) {
        TYPEERR(ref, m_expr[2].get(), "unexpected type",
                "    expected: %s\n"
                "    actual: %s",
                m_expr[1]->m_type->str().c_str(),
                m_expr[2]->m_type->str().c_str());
        return nullptr;
    }

    auto t = std::make_shared<ir_scalar>();
    t->m_type = TYPE_BOOL;
    m_type = t;

    return m_type;
}

shared_ir_type ir_apply::check_ifexpr(const ir &ref, id2type &vars) {
    if (m_expr.size() != 4) {
        SEMANTICERR(ref, this, "\"if\" requires exactly 3 arguments");
        return nullptr;
    }

    if (!m_expr[1]->check_type(ref, vars))
        return nullptr;

    if (m_expr[1]->m_type->m_irtype != ir_type::IRTYPE_SCALAR ||
        ((ir_scalar *)(m_expr[1]->m_type.get()))->m_type != TYPE_BOOL) {
        TYPEERR(ref, m_expr[1].get(), "unexpected type",
                "    expected: bool\n"
                "    actual: %s",
                m_expr[1]->m_type->str().c_str());
        return nullptr;
    }

    if (!m_expr[2]->check_type(ref, vars))
        return nullptr;

    if (!m_expr[3]->check_type(ref, vars))
        return nullptr;

    if (!unify_type(m_expr[2].get(), m_expr[3].get())) {
        TYPEERR(ref, m_expr[3].get(), "unexpected type",
                "    expected: %s\n"
                "    actual: %s",
                m_expr[2]->m_type->str().c_str(),
                m_expr[3]->m_type->str().c_str());
        return nullptr;
    }

    m_type = m_expr[2]->m_type;
    return m_type;
}

bool ir::is_structgen(ir_expr *expr) const {
    if (expr->m_expr_type == ir_expr::EXPRAPPLY) {
        auto apply = (ir_apply *)expr;
        if (apply->m_expr.size() > 0 &&
            apply->m_expr[0]->m_expr_type == ir_expr::EXPRID) {
            auto id = (ir_id *)apply->m_expr[0].get();
            if (HASKEY(m_id2struct, id->m_id)) {
                return true;
            }
        }
    }

    return false;
}

void ir::llvm_memcpy(llvm::Value *dst, llvm::Value *src, size_t size) {
    std::vector<llvm::Value *> args;
    args.push_back(m_llvm_builder.CreateBitCast(dst, I8PTR(m_llvm_ctx)));
    args.push_back(m_llvm_builder.CreateBitCast(src, I8PTR(m_llvm_ctx)));
    args.push_back(
        llvm::ConstantInt::get(m_llvm_ctx, llvm::APInt(64, size, false)));
    args.push_back(FALSEVAL(m_llvm_ctx));
    m_llvm_builder.CreateCall(m_memcpy, args, "memcpytmp");
}

std::string ir::codegen() {
    for (auto &p : m_id2struct) {
        auto struc = p.second->codegen(*this);
        if (!struc)
            return "";
        m_struct_prot[p.second->m_name] = (llvm::StructType *)struc;
    }

    for (auto &p : m_defuns) {
        auto fun = p->mkproto(*this);
        if (!fun)
            return "";
        m_funs_prot[p->m_name] = fun;
    }

    for (auto &p : m_externs) {
        auto fun = p->mkproto(*this);
        if (!fun)
            return "";
        m_funs_prot[p->m_name] = fun;
    }

    // Declare the memcpy
    std::vector<llvm::Type *> vec;
    vec.push_back(I8PTR(m_llvm_ctx)); /* i8 */
    vec.push_back(I8PTR(m_llvm_ctx)); /* i8 */
    vec.push_back(llvm::IntegerType::get(m_llvm_ctx, 64));
    vec.push_back(llvm::IntegerType::get(m_llvm_ctx, 1));
    m_memcpy = llvm::Intrinsic::getDeclaration(&m_llvm_module,
                                               llvm::Intrinsic::memcpy, vec);

    assert(m_memcpy != nullptr);

    for (auto &p : m_defuns) {
        if (!p->codegen(*this))
            return "";
    }

    std::string s;
    llvm::raw_string_ostream os(s);
    m_llvm_module.print(os, nullptr);

    return s;
}

llvm::Type *ir_scalar::codegen(ir &ref) {
    switch (m_type) {
    case TYPE_BOOL:
        return llvm::Type::getInt1Ty(ref.get_llvm_ctx());
    case TYPE_FP64:
        return llvm::Type::getDoubleTy(ref.get_llvm_ctx());
    case TYPE_FP32:
        return llvm::Type::getFloatTy(ref.get_llvm_ctx());
    case TYPE_U64:
    case TYPE_S64:
        return llvm::Type::getInt64Ty(ref.get_llvm_ctx());
    case TYPE_U32:
    case TYPE_S32:
        return llvm::Type::getInt32Ty(ref.get_llvm_ctx());
    case TYPE_U16:
    case TYPE_S16:
        return llvm::Type::getInt16Ty(ref.get_llvm_ctx());
    case TYPE_U8:
    case TYPE_S8:
        return llvm::Type::getInt8Ty(ref.get_llvm_ctx());
    case TYPE_VOID:
        return llvm::Type::getVoidTy(ref.get_llvm_ctx());
    case TYPE_VEC:
    case TYPE_INT:
    case TYPE_REF:
    case TYPE_STRUCT:
    case TYPE_FUN:
    case TYPE_UTF8:
        assert(m_type != TYPE_INT && m_type != TYPE_REF &&
               m_type != TYPE_STRUCT && m_type != TYPE_FUN &&
               m_type != TYPE_UTF8);
        return nullptr;
    }
}

llvm::Type *ir_ref::codegen(ir &ref) {
    switch (m_type->m_irtype) {
    case ir_type::IRTYPE_VEC:
    case ir_type::IRTYPE_STRUCT:
    case ir_type::IRTYPE_UTF8:
    case ir_type::IRTYPE_FUN:
        return m_type->codegen(ref);
    case ir_type::IRTYPE_SCALAR: {
        auto s = (ir_scalar *)m_type.get();
        if (s->m_type == TYPE_VOID) {
            auto t = llvm::Type::getInt1Ty(ref.get_llvm_ctx());
            return llvm::PointerType::getUnqual(t);
        }
    }
    default: {
        auto t = m_type->codegen(ref);
        return llvm::PointerType::getUnqual(t);
    }
    }
}

llvm::Type *ir_utf8::codegen(ir &ref) {
    auto i8 = llvm::Type::getInt8Ty(ref.get_llvm_ctx());
    return llvm::PointerType::getUnqual(i8);
}

llvm::Type *ir_vec::codegen(ir &ref) {
    if (m_num) {
        auto t = m_type->codegen(ref);
        return llvm::ArrayType::get(
            t, boost::lexical_cast<uint64_t>(m_num->m_num));
    } else {
        auto t = m_type->codegen(ref);
        return llvm::PointerType::getUnqual(t);
    }
}

llvm::Type *ir_funtype::codegen(ir &ref) {
    auto ret = m_ret->codegen(ref);
    if (!ret)
        return nullptr;

    std::vector<llvm::Type *> args;
    for (auto &arg : m_args) {
        auto a = arg->codegen(ref);
        if (!a)
            return nullptr;

        args.push_back(a);
    }

    auto ftype = llvm::FunctionType::get(ret, args, false);
    return llvm::PointerType::getUnqual(ftype);
}

llvm::Function *ir_defun::codegen(ir &ref) {
    // bind variables
    ir_expr::id2val vars;
    unsigned i = 0;
    for (auto &arg : m_fun->args()) {
        auto s = m_args[i]->m_id;
        arg.setName(s);
        vars[s].push_back(&arg);
        i++;
    }

    auto bb = llvm::BasicBlock::Create(ref.get_llvm_ctx(), "entry", m_fun);
    auto &builder = ref.get_llvm_builder();
    builder.SetInsertPoint(bb);

    llvm::Value *retval = m_expr->codegen(ref, vars);
    if (retval) {
        if (m_ret->m_irtype == ir_type::IRTYPE_SCALAR &&
            ((ir_scalar *)m_ret.get())->m_type == TYPE_VOID)
            builder.CreateRetVoid();
        else
            builder.CreateRet(retval);

        // Validate the generated code, checking for consistency.
        // llvm::verifyFunction(fun);

        if (m_name == "main")
            codegen_main(ref);

        return m_fun;
    }

    return nullptr;
}

llvm::Function *ir_defun::codegen_main(ir &ref) {
    auto &ctx = ref.get_llvm_ctx();

    auto ret = llvm::IntegerType::get(ctx, 64);
    auto argc = llvm::IntegerType::get(ctx, 64);
    auto charty = llvm::IntegerType::get(ctx, 8);
    auto charptr = llvm::PointerType::getUnqual(charty);
    auto argv = llvm::PointerType::getUnqual(charptr);

    std::vector<llvm::Type *> args;
    args.push_back(std::move(argc));
    args.push_back(std::move(argv));

    auto ftype = llvm::FunctionType::get(ret, args, false);
    auto mainfun = llvm::Function::Create(
        ftype, llvm::Function::ExternalLinkage, "main", &ref.get_llvm_module());

    int n = 0;
    for (auto &arg : mainfun->args()) {
        if (n == 0)
            arg.setName("argc");
        else
            arg.setName("argv");
        n++;
    }

    auto entry = codegen_entry(ref);

    std::vector<llvm::Value *> vals;
    auto bb = llvm::BasicBlock::Create(ref.get_llvm_ctx(), "entry", mainfun);
    auto &builder = ref.get_llvm_builder();
    builder.SetInsertPoint(bb);
    builder.CreateCall(ref.get_function("init_thread"), vals, "");

    auto voidty = llvm::PointerType::getUnqual(llvm::IntegerType::get(ctx, 8));
    vals.push_back(entry);
    vals.push_back(llvm::ConstantPointerNull::get(voidty));
    vals.push_back(
        llvm::ConstantInt::get(ctx, llvm::APInt(32, 4096 * 32, false)));
    builder.CreateCall(ref.get_function("spawn_green_thread"), vals, "");

    vals.clear();
    builder.CreateCall(ref.get_function("run_green_thread"), vals, "");

    builder.CreateRet(llvm::ConstantInt::get(ctx, llvm::APInt(64, 0, false)));

    return mainfun;
}

llvm::Function *ir_defun::codegen_entry(ir &ref) {
    // TODO: handle spawn

    auto &ctx = ref.get_llvm_ctx();

    auto voidty = llvm::Type::getVoidTy(ctx);
    auto voidptr = llvm::PointerType::getUnqual(llvm::IntegerType::get(ctx, 8));

    std::vector<llvm::Type *> args;
    args.push_back(std::move(voidptr));

    auto ftype = llvm::FunctionType::get(voidty, args, false);
    auto entry =
        llvm::Function::Create(ftype, llvm::Function::ExternalLinkage,
                               m_name + ":entry", &ref.get_llvm_module());

    entry->args().begin()->setName("arg");

    auto bb = llvm::BasicBlock::Create(ref.get_llvm_ctx(), "entry", entry);
    auto &builder = ref.get_llvm_builder();
    builder.SetInsertPoint(bb);

    std::vector<llvm::Value *> vals;
    auto ret = builder.CreateCall(m_fun, vals, "");

    if (m_fun->getCallingConv() == llvm::CallingConv::Fast) {
        ret->setCallingConv(llvm::CallingConv::Fast);
        ret->setTailCall();
    }

    builder.CreateRetVoid();

    return entry;
}

llvm::Type *ir_struct::codegen(ir &ref) {
    std::vector<llvm::Type *> member;
    for (auto &p : m_member) {
        member.push_back(p->codegen(ref));
    }

    auto ret = llvm::StructType::get(ref.get_llvm_ctx(), member);

    if (ret)
        ret->setName(m_name);

    return ret;
}

llvm::Type *ir_usertype::codegen(ir &ref) {
    auto it = ref.get_struct_proto().find(m_name);
    if (it == ref.get_struct_proto().end())
        return nullptr;

    return it->second;
}

llvm::Function *ir_defun::mkproto(ir &ref) {
    // type of return values
    llvm::Type *type = m_ret->codegen(ref);
    if (type == nullptr)
        return nullptr;

    // type of arguments
    std::vector<llvm::Type *> args;
    for (auto &q : m_args) {
        llvm::Type *t = q->m_type->codegen(ref);
        if (t == nullptr)
            return nullptr;

        if (t->isVoidTy())
            t = llvm::Type::getInt1Ty(ref.get_llvm_ctx());

        args.push_back(t);
    }

    std::string name;
    if (m_name == "main")
        name = "main:lunar";
    else
        name = m_name;

    auto ftype = llvm::FunctionType::get(type, args, false);
    m_fun = llvm::Function::Create(ftype, llvm::Function::ExternalLinkage, name,
                                   &ref.get_llvm_module());

    m_fun->setCallingConv(llvm::CallingConv::Fast);

    return m_fun;
}

llvm::Function *ir_extern::mkproto(ir &ref) {
    // type of return values
    llvm::Type *type = m_ret->codegen(ref);
    if (type == nullptr)
        return nullptr;

    // type of arguments
    std::vector<llvm::Type *> args;
    for (auto &q : m_args) {
        llvm::Type *t = q->codegen(ref);
        if (t == nullptr)
            return nullptr;

        if (t->isVoidTy())
            t = llvm::Type::getInt1Ty(ref.get_llvm_ctx());

        args.push_back(t);
    }

    auto ftype = llvm::FunctionType::get(type, args, false);
    m_fun = llvm::Function::Create(ftype, llvm::Function::ExternalLinkage,
                                   m_name, &ref.get_llvm_module());

    if (m_is_fastcc)
        m_fun->setCallingConv(llvm::CallingConv::Fast);

    return m_fun;
}

llvm::Value *ir_id::codegen(ir &ref, ir_expr::id2val &vals) {
    auto it = vals.find(m_id);
    if (it != vals.end()) {
        return it->second.back();
    }

    return nullptr;
}

llvm::Value *ir_let::codegen(ir &ref, ir_expr::id2val &vals) {
    for (auto &p : m_def) {
        auto v = p->m_expr->codegen(ref, vals);

        if (p->m_expr->m_expr_type == EXPRVOID) {
            vals[p->m_id->m_id].push_back(VOIDVAL(ref.get_llvm_ctx()));
        } else {
            vals[p->m_id->m_id].push_back(v);
        }
    }

    auto e = m_expr->codegen(ref, vals);

    for (auto &p : m_def) {
        auto it = vals.find(p->m_id->m_id);
        it->second.pop_back();
        if (it->second.size() == 0) {
            vals.erase(it);
        }
    }

    return e;
}

llvm::Value *ir_mkvec::codegen(ir &ref, ir_expr::id2val &vals) {
    auto tmp = ((ir_ref *)m_type.get())->m_type;
    auto type = ((ir_vec *)tmp.get())->m_type->codegen(ref);
    if (!type)
        return nullptr;

    auto num = m_num->codegen(ref, vals);
    if (!num)
        return nullptr;

    return ref.get_llvm_builder().CreateAlloca(type, num);
}

llvm::Value *ir_decimal::codegen(ir &ref, ir_expr::id2val &vals) {
    auto t = (ir_scalar *)m_type.get();
    auto &ctx = ref.get_llvm_ctx();
    switch (t->m_type) {
    case TYPE_U64:
        return llvm::ConstantInt::get(
            ctx, llvm::APInt(64, boost::lexical_cast<uint64_t>(m_num), false));
    case TYPE_INT:
    case TYPE_S64:
        return llvm::ConstantInt::get(
            ctx, llvm::APInt(64, boost::lexical_cast<int64_t>(m_num), true));
    case TYPE_U32:
        return llvm::ConstantInt::get(
            ctx, llvm::APInt(32, boost::lexical_cast<uint32_t>(m_num), false));
    case TYPE_S32:
        return llvm::ConstantInt::get(
            ctx, llvm::APInt(32, boost::lexical_cast<int32_t>(m_num), true));
    case TYPE_U16:
        return llvm::ConstantInt::get(
            ctx, llvm::APInt(16, boost::lexical_cast<uint16_t>(m_num), true));
    case TYPE_S16:
        return llvm::ConstantInt::get(
            ctx, llvm::APInt(16, boost::lexical_cast<int16_t>(m_num), true));
    case TYPE_U8:
        return llvm::ConstantInt::get(
            ctx, llvm::APInt(8, boost::lexical_cast<uint8_t>(m_num), true));
    case TYPE_S8:
        return llvm::ConstantInt::get(
            ctx, llvm::APInt(8, boost::lexical_cast<int8_t>(m_num), true));
    default:
        return nullptr;
    }
    return nullptr;
}

llvm::Value *ir_float::codegen(ir &ref, ir_expr::id2val &vals) {
    auto t = (ir_scalar *)m_type.get();
    auto &ctx = ref.get_llvm_ctx();
    switch (t->m_type) {
    case TYPE_FP64: {
        double num = boost::lexical_cast<double>(m_num);
        return llvm::ConstantFP::get(ctx, llvm::APFloat(num));
    }
    case TYPE_FP32: {
        float num = boost::lexical_cast<float>(m_num);
        return llvm::ConstantFP::get(ctx, llvm::APFloat(num));
    }
    default:
        return nullptr;
    }

    return nullptr;
};

llvm::Value *ir::get_constant_str(std::string str) {
    auto it = m_constant_str.find(str);
    if (it != m_constant_str.end())
        return it->second;

    auto val = m_llvm_builder.CreateGlobalStringPtr(str, "str");
    m_constant_str[str] = val;

    return val;
}

llvm::Value *ir_str::codegen(ir &ref, ir_expr::id2val &vals) {
    return ref.get_constant_str(m_str);
}

llvm::Value *ir_bool::codegen(ir &ref, ir_expr::id2val &vals) {
    if (m_bool)
        return TRUEVAL(ref.get_llvm_ctx());
    else
        return FALSEVAL(ref.get_llvm_ctx());

    return nullptr;
}

#define ARITHMETIC(UIOP, SIOP, FOP, OP, NAME)                                  \
    do {                                                                       \
        if (m_expr.size() < 3) {                                               \
            SEMANTICERR(ref, this,                                             \
                        OP " requires more than or equal to 2 arguments");     \
        }                                                                      \
        auto e1 = m_expr[1]->codegen(ref, vals);                               \
        if (e1 == nullptr)                                                     \
            return nullptr;                                                    \
                                                                               \
        for (size_t i = 2; i < m_expr.size(); i++) {                           \
            auto e2 = m_expr[i]->codegen(ref, vals);                           \
            ir_scalar *s = (ir_scalar *)m_expr[i]->m_type.get();               \
            if (e2 == nullptr)                                                 \
                return nullptr;                                                \
                                                                               \
            switch (s->m_type) {                                               \
            case TYPE_U64:                                                     \
            case TYPE_U32:                                                     \
            case TYPE_U16:                                                     \
            case TYPE_U8:                                                      \
                e1 = ref.get_llvm_builder().UIOP(e1, e2, NAME);                \
                break;                                                         \
            case TYPE_S64:                                                     \
            case TYPE_S32:                                                     \
            case TYPE_S16:                                                     \
            case TYPE_S8:                                                      \
                e1 = ref.get_llvm_builder().SIOP(e1, e2, NAME);                \
                break;                                                         \
            case TYPE_FP64:                                                    \
            case TYPE_FP32:                                                    \
                e1 = ref.get_llvm_builder().FOP(e1, e2, NAME);                 \
            default:                                                           \
                return nullptr;                                                \
            }                                                                  \
        }                                                                      \
        return e1;                                                             \
    } while (0)

#define BINOP(UIOP, SIOP, FOP, OP, NAME)                                       \
    do {                                                                       \
        if (m_expr.size() != 3) {                                              \
            SEMANTICERR(ref, this, OP " requires exactly 2 arguments");        \
        }                                                                      \
        auto e1 = m_expr[1]->codegen(ref, vals);                               \
        if (e1 == nullptr)                                                     \
            return nullptr;                                                    \
                                                                               \
        auto e2 = m_expr[2]->codegen(ref, vals);                               \
        if (e2 == nullptr)                                                     \
            return nullptr;                                                    \
                                                                               \
        switch (((ir_scalar *)m_expr[1]->m_type.get())->m_type) {              \
        case TYPE_U64:                                                         \
        case TYPE_U32:                                                         \
        case TYPE_U16:                                                         \
        case TYPE_U8:                                                          \
            e1 = ref.get_llvm_builder().UIOP(e1, e2, NAME);                    \
            break;                                                             \
        case TYPE_S64:                                                         \
        case TYPE_S32:                                                         \
        case TYPE_S16:                                                         \
        case TYPE_S8:                                                          \
            e1 = ref.get_llvm_builder().SIOP(e1, e2, NAME);                    \
            break;                                                             \
        case TYPE_FP64:                                                        \
        case TYPE_FP32:                                                        \
            e1 = ref.get_llvm_builder().FOP(e1, e2, NAME);                     \
        case TYPE_INT: {                                                       \
            auto d1 = (ir_decimal *)m_expr[1].get();                           \
            auto d2 = (ir_decimal *)m_expr[1].get();                           \
            if (d1->m_num[0] == '-' || d2->m_num[0] == '-')                    \
                e1 = ref.get_llvm_builder().UIOP(e1, e2, NAME);                \
            else                                                               \
                e1 = ref.get_llvm_builder().SIOP(e1, e2, NAME);                \
        }                                                                      \
        default:                                                               \
            return nullptr;                                                    \
        }                                                                      \
        return e1;                                                             \
    } while (0)

llvm::Value *ir_apply::codegen(ir &ref, ir_expr::id2val &vals) {
    if (m_expr_type == EXPRVOID)
        return VOIDVAL(ref.get_llvm_ctx());

    auto first = m_expr.begin();
    if ((*first)->m_expr_type == ir_expr::EXPRID) {
        auto id = (ir_id *)(first->get());
        if (id->m_id.size() == 1) {
            switch (id->m_id[0]) {
            case '+':
                ARITHMETIC(CreateAdd, CreateAdd, CreateFAdd, "+", "addtmp");
            case '-':
                ARITHMETIC(CreateSub, CreateSub, CreateFSub, "-", "subtmp");
            case '*':
                ARITHMETIC(CreateMul, CreateMul, CreateFMul, "*", "multmp");
            case '/':
                ARITHMETIC(CreateUDiv, CreateSDiv, CreateFDiv, "/", "divtmp");
            case '<':
                BINOP(CreateICmpULT, CreateICmpSLT, CreateFCmpOLT, "<",
                      "lttmp");
            case '>':
                BINOP(CreateICmpUGT, CreateICmpSGT, CreateFCmpOGT, ">",
                      "gttmp");
            case '=':
                BINOP(CreateICmpEQ, CreateICmpEQ, CreateFCmpOEQ, "=", "eqtmp");
            default:
                // function call
                auto it = ref.get_struct_proto().find(id->m_id);
                if (it != ref.get_struct_proto().end()) {
                    return struct_gen(ref, vals, it->second);
                }
                return codegen_call(ref, vals, id->m_id);
            }
        } else if (id->m_id == "if") {
            return codegen_ifexpr(ref, vals);
        } else if (id->m_id == ">=") {
            BINOP(CreateICmpUGE, CreateICmpSGE, CreateFCmpOGE, ">=", "getmp");
        } else if (id->m_id == "<=") {
            BINOP(CreateICmpULE, CreateICmpSLE, CreateFCmpOLE, "<=", "letmp");
        } else if (id->m_id == "!=") {
            BINOP(CreateICmpNE, CreateICmpNE, CreateFCmpONE, "!=", "netmp");
        } else if (id->m_id == "print") {
            return codegen_print(ref, vals);
        } else if (id->m_id == "vec") {
            return codegen_vec(ref, vals);
        } else if (id->m_id == "elm") {
            return codegen_elm(ref, vals);
        } else if (id->m_id == "load") {
            return codegen_load(ref, vals);
        } else if (id->m_id == "store") {
        } else {
            // function call
            auto it = ref.get_struct_proto().find(id->m_id);
            if (it != ref.get_struct_proto().end()) {
                return struct_gen(ref, vals, it->second);
            }
            return codegen_call(ref, vals, id->m_id);
        }
    }

    return nullptr;
}

llvm::Value *ir_apply::codegen_print(ir &ref, id2val &vals) {
    auto &builder = ref.get_llvm_builder();
    auto &ctx = ref.get_llvm_ctx();

    int n = 0;
    for (auto it = m_expr.begin() + 1; it != m_expr.end(); ++it, n++) {
        if (n > 0) {
            std::vector<llvm::Value *> args;
            args.push_back(ref.get_constant_str(" "));
            builder.CreateCall(ref.get_function("print_utf8"), args, "");
        }

        auto val = (*it)->codegen(ref, vals);
        if (!val)
            return nullptr;

        switch ((*it)->m_type->m_irtype) {
        case ir_type::IRTYPE_SCALAR: {
            auto s = (ir_scalar *)(*it)->m_type.get();
            switch (s->m_type) {
            case TYPE_U32:
            case TYPE_U16:
            case TYPE_U8:
                val = builder.CreateIntCast(
                    val, llvm::IntegerType::get(ctx, 64), false, "uintcast");
            case TYPE_U64: {
                std::vector<llvm::Value *> args;
                args.push_back(val);
                builder.CreateCall(ref.get_function("print_unum"), args, "");
                break;
            }
            case TYPE_S32:
            case TYPE_S16:
            case TYPE_S8:
                val = builder.CreateIntCast(
                    val, llvm::IntegerType::get(ctx, 64), true, "sintcast");
            case TYPE_INT:
            case TYPE_S64: {
                std::vector<llvm::Value *> args;
                args.push_back(val);
                builder.CreateCall(ref.get_function("print_snum"), args, "");
                break;
            }
            case TYPE_VOID: {
                std::vector<llvm::Value *> args;
                args.push_back(ref.get_constant_str("void"));
                builder.CreateCall(ref.get_function("print_utf8"), args, "");
                break;
            }
            case TYPE_BOOL: {
                std::vector<llvm::Value *> args;
                args.push_back(val);
                builder.CreateCall(ref.get_function("print_boolean"), args, "");
                break;
            }
            case TYPE_FP64: {
                std::vector<llvm::Value *> args;
                args.push_back(val);
                builder.CreateCall(ref.get_function("print_fp64"), args, "");
                break;
            }
            case TYPE_FP32: {
                std::vector<llvm::Value *> args;
                args.push_back(val);
                builder.CreateCall(ref.get_function("print_fp32"), args, "");
                break;
            }
            default:;
            }
            break;
        }
        case ir_type::IRTYPE_REF: {
            std::vector<llvm::Value *> args;
            args.push_back(val);

            auto r = (ir_ref *)(*it)->m_type.get();
            if (r->m_type->m_irtype == ir_type::IRTYPE_UTF8)
                builder.CreateCall(ref.get_function("print_utf8"), args, "");
            else
                builder.CreateCall(ref.get_function("print_ptr"), args, "");

            break;
        }
        case ir_type::IRTYPE_VEC:
        case ir_type::IRTYPE_STRUCT:
        case ir_type::IRTYPE_FUN:
        case ir_type::IRTYPE_UTF8:
        case ir_type::IRTYPE_USER: {
            // TODO
            std::vector<llvm::Value *> args;
            args.push_back(ref.get_constant_str("NOT IMPLEMENTED"));
            builder.CreateCall(ref.get_function("print_utf8"), args, "");
        }
        }
    }

    std::vector<llvm::Value *> args;
    builder.CreateCall(ref.get_function("print_endl"), args, "");

    return VOIDVAL(ctx);
}

llvm::Value *ir_apply::codegen_vec(ir &ref, id2val &vals) {
    auto type = m_type->codegen(ref);
    if (!type)
        return nullptr;

    auto num = m_expr[2]->codegen(ref, vals);
    if (!num)
        return nullptr;

    auto &builder = ref.get_llvm_builder();
    auto alloc = builder.CreateAlloca(type, num);

    return alloc;
}

llvm::Value *ir_apply::codegen_elm(ir &ref, id2val &vals) {
    auto &builder = ref.get_llvm_builder();

    auto reftype = (ir_ref *)m_expr[1]->m_type.get();
    switch (reftype->m_type->m_irtype) {
    case ir_type::IRTYPE_STRUCT: {
        auto id = (ir_id *)m_expr[2].get();
        auto st = (ir_struct *)reftype->m_type.get();

        auto it = st->m_id2idx.find(id->m_id);
        assert(it != st->m_id2idx.end());

        auto val = m_expr[1]->codegen(ref, vals);
        return builder.CreateStructGEP(val, it->second, "elm");
    }
    case ir_type::IRTYPE_VEC: {
        auto val1 = m_expr[1]->codegen(ref, vals);
        auto val2 = m_expr[2]->codegen(ref, vals);
        return builder.CreateGEP(val1, val2, "elm");
    }
    default:;
    }
    return nullptr;
}

llvm::Value *ir_apply::codegen_load(ir &ref, id2val &vals) {
    auto &builder = ref.get_llvm_builder();
    auto val = m_expr[1]->codegen(ref, vals);
    return builder.CreateLoad(val, "load");
}

llvm::Value *ir_apply::struct_gen(ir &ref, id2val &vals,
                                  llvm::StructType *type) {
    auto &builder = ref.get_llvm_builder();
    auto alloc = builder.CreateAlloca(type);

    struct_gen2(ref, vals, type, m_expr, alloc);

    return alloc;
}

void ir_apply::struct_gen2(ir &ref, id2val &vals, llvm::StructType *type,
                           std::vector<ptr_ir_expr> &exprs, llvm::Value *gep) {
    auto &builder = ref.get_llvm_builder();

    int n = 0;
    for (auto it = exprs.begin() + 1; it != exprs.end(); ++it, n++) {
        auto ptr = builder.CreateStructGEP(type, gep, n);

        if (ref.is_structgen(it->get())) {
            auto apply = (ir_apply *)it->get();
            struct_gen2(ref, vals,
                        (llvm::StructType *)apply->m_type->codegen(ref),
                        apply->m_expr, ptr);
            continue;
        }

        auto t = (*it)->codegen(ref, vals);

        if (type->getElementType(n)->isArrayTy()) {
            auto vec = (llvm::ArrayType *)type->getElementType(n);
            auto &layout = ref.get_llvm_datalayout();
            auto vecsize = vec->getNumElements() *
                           layout.getTypeAllocSize(vec->getElementType());
            auto head =
                builder.CreateBitCast(ptr, I8PTR(ref.get_llvm_ctx()), "vec");
            ref.llvm_memcpy(head, t, vecsize);
        } else {
            builder.CreateStore(t, ptr);
        }
    }
}

llvm::Value *ir_apply::codegen_ifexpr(ir &ref, id2val &vals) {
    llvm::Value *condv = m_expr[1]->codegen(ref, vals);
    if (!condv)
        return nullptr;

    auto &builder = ref.get_llvm_builder();
    auto &ctx = ref.get_llvm_ctx();

    condv = builder.CreateICmpEQ(condv, TRUEVAL(ref.get_llvm_ctx()), "ifcond");

    llvm::Function *func = builder.GetInsertBlock()->getParent();

    // Create blocks for the then and else cases.  Insert the 'then'
    // block at the end of the function.
    llvm::BasicBlock *then_bb = llvm::BasicBlock::Create(ctx, "then", func);
    llvm::BasicBlock *else_bb = llvm::BasicBlock::Create(ctx, "else");
    llvm::BasicBlock *merge_bb = llvm::BasicBlock::Create(ctx, "ifcont");

    builder.CreateCondBr(condv, then_bb, else_bb);

    // Emit then value.
    builder.SetInsertPoint(then_bb);

    llvm::Value *thenv = m_expr[2]->codegen(ref, vals);
    if (!thenv)
        return nullptr;

    if (thenv->getType()->isVoidTy())
        thenv = VOIDVAL(ctx);

    builder.CreateBr(merge_bb);
    // Codegen of 'Then' can change the current block, update ThenBB for
    // the PHI.
    then_bb = builder.GetInsertBlock();

    // Emit else block.
    func->getBasicBlockList().push_back(else_bb);
    builder.SetInsertPoint(else_bb);

    llvm::Value *elsev = m_expr[3]->codegen(ref, vals);
    if (!elsev)
        return nullptr;

    if (elsev->getType()->isVoidTy())
        elsev = VOIDVAL(ctx);

    builder.CreateBr(merge_bb);
    // Codegen of 'Else' can change the current block, update ElseBB for
    // the PHI.
    else_bb = builder.GetInsertBlock();

    // Emit merge block.
    func->getBasicBlockList().push_back(merge_bb);
    builder.SetInsertPoint(merge_bb);
    llvm::PHINode *pn = builder.CreatePHI(elsev->getType(), 2, "iftmp");

    pn->addIncoming(thenv, then_bb);
    pn->addIncoming(elsev, else_bb);
    return pn;
}

llvm::Value *ir_apply::codegen_call(ir &ref, id2val &vals,
                                    const std::string &id) {
    auto fun = ref.get_function(id);
    if (!fun) {
        SEMANTICERR(ref, this, "%s is undefined", id.c_str());
        return nullptr;
    }

    std::vector<llvm::Value *> args;
    for (auto it = m_expr.begin() + 1; it != m_expr.end(); ++it) {
        auto t = (*it)->codegen(ref, vals);
        if (!t)
            return nullptr;

        if ((*it)->m_expr_type == EXPRVOID || t->getType()->isVoidTy())
            args.push_back(VOIDVAL(ref.get_llvm_ctx()));
        else
            args.push_back(t);
    }

    const char *s;
    if (fun->getReturnType()->isVoidTy())
        s = "";
    else
        s = "calltmp";

    auto ret = ref.get_llvm_builder().CreateCall(fun, args, s);

    if (fun->getCallingConv() == llvm::CallingConv::Fast) {
        ret->setCallingConv(llvm::CallingConv::Fast);
        ret->setTailCall();
    }

    return ret;
}

void ir::print_err(std::size_t line, std::size_t column) const {
    std::vector<std::string> lines;

    boost::split(lines, m_parsec.get_str(), boost::is_any_of("\n"));

    std::cerr << lines[line - 1] << std::endl;

    for (size_t i = 1; i < column; i++) {
        std::cerr << " ";
    }
    std::cerr << "^" << std::endl;
}

void ir::print() {
    std::cout << "{\"structure definition\":[";
    int n = 0;
    for (auto &p : m_struct) {
        if (n > 0)
            std::cout << ",";
        p->print();
        n++;
    }
    std::cout << "],\"function definition\":[";
    n = 0;
    for (auto &p : m_defuns) {
        if (n > 0)
            std::cout << ",";
        p->print();
        n++;
    }
    std::cout << "]}";
}

void ir_funtype::print() {
    std::cout << "{\"fun\":{\"ret\":";

    m_ret->print();

    std::cout << ",\"args\":[";
    auto n = m_args.size();
    for (auto &q : m_args) {
        q->print();
        n--;
        if (n > 0) {
            std::cout << ",";
        }
    }
    std::cout << "]}}";
}

void ir_struct::print() {
    std::cout << "{\"struct\":{\"name\":\"" << m_name << "\",\"member\":[";
    int n = 0;
    for (auto &p : m_member) {
        if (n > 0)
            std::cout << ",";
        p->print();
        n++;
    }
    std::cout << "]}}";
}

void ir_usertype::print() {
    std::cout << "{\"usertype\":\"" << m_name << "\"}";
}

void ir_ref::print() {
    std::cout << "{\"ref\":";
    m_type->print();
    std::cout << "}";
}

void ir_utf8::print() { std::cout << "\"utf8\""; }

void ir_vec::print() {
    std::cout << "{\"vec\":";
    m_type->print();
    std::cout << "}";
}

void ir_scalar::print() { std::cout << "\"" << str() << "\""; }

void ir_defun::print() {
    std::cout << "{\"defun\":{\"id\":\"" << m_name << "\","
              << "\"ret\":";
    m_ret->print();
    std::cout << ",\"args\":[";
    auto n = m_args.size();
    for (auto &q : m_args) {
        std::cout << "{\"type\":";
        q->m_type->print();
        std::cout << ",\"id\":\"" << q->m_id << "\"}";
        n--;
        if (n > 0) {
            std::cout << ",";
        }
    }
    std::cout << "],\"expr\":";
    m_expr->print();
    std::cout << "}}";
}

void ir_extern::print() {
    std::cout << "{\"extern\":{\"id\":\"" << m_name << "\","
              << "\"ret\":";
    m_ret->print();
    std::cout << ",\"args\":[";
    auto n = m_args.size();
    for (auto &q : m_args) {
        q->print();
        n--;
        if (n > 0) {
            std::cout << ",";
        }
    }
    std::cout << "]}}";
}

void ir_apply::print() {
    int n = 0;
    std::cout << "{\"apply\":[";
    for (auto &p : m_expr) {
        if (n > 0)
            std::cout << ",";
        p->print();
        n++;
    }
    std::cout << "]}";
}

void ir_bool::print() {
    std::cout << "{\"bool\":" << (m_bool ? "true" : "false") << "}";
}

void ir_decimal::print() { std::cout << "{\"decimal\":" << m_num << "}"; }

void ir_float::print() { std::cout << "{\"float\":" << m_num << "}"; }

void ir_str::print() {
    std::cout << "{\"str\":\"" << escape_json(m_str) << "\"}";
}

void ir_let::print() {
    int n = 0;
    std::cout << "{\"let\":{\"def\":[";
    for (auto &p : m_def) {
        if (n > 0)
            std::cout << ",";
        std::cout << "{\"type\":";
        p->m_id->m_type->print();
        std::cout << ",\"id\":\"" << p->m_id->m_id << "\",\"expr\":";
        p->m_expr->print();
        std::cout << "}";
        n++;
    }

    std::cout << "], \"expr\":";
    m_expr->print();
    std::cout << "}}";
}

void ir_mkvec::print() {
    int n = 0;
    std::cout << "{\"mkvec\":{\"type\":";
    m_vectype->print();
    std::cout << ",\"num\":";
    m_num->print();
    std::cout << "}}";
}

} // namespace lunar