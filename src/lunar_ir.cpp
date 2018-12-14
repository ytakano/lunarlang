#include "lunar_ir.hpp"

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

        for (int i = 0; i < t1->m_args.size(); i++) {
            if (!eq_type(t1->m_args[i].get(), t2->m_args[i].get()))
                return false;
        }

        return true;
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

        for (int i = 0; i < t1->m_member.size(); i++) {
            if (!eq_type(t1->m_member[i].get(), t2->m_member[i].get()))
                return false;
        }

        return true;
    }
    case ir_type::IRTYPE_USER: {
        return false;
    }
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
    case ir_type::IRTYPE_FUN:
    case ir_type::IRTYPE_REF:
    case ir_type::IRTYPE_STRUCT:
        if (!eq_type(lhs->m_type.get(), rhs->m_type.get()))
            return false;

        lhs->m_type = rhs->m_type;
        return true;
    case ir_type::IRTYPE_USER:
        return false;
    }

    assert(false);
    return false;
}

ir::ir(const std::string &filename, const std::string &str)
    : m_filename(filename), m_parsec(str), m_llvm_builder(m_llvm_ctx),
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
    m_parsec.space();
    if (m_parsec.is_fail()) {
        SYNTAXERR("expected whitespace");
        return nullptr;
    }

    auto name = parse_id();
    if (m_parsec.is_fail()) {
        SYNTAXERR("expected identifier");
        return nullptr;
    }

    m_parsec.space();
    if (m_parsec.is_fail()) {
        SYNTAXERR("expected whitespace");
        return nullptr;
    }
    m_parsec.spaces();

    auto st = std::make_unique<ir_struct>();

    st->m_name = name;

    int n = 0;
    for (;;) {
        m_parsec.character('(');
        if (m_parsec.is_fail()) {
            SYNTAXERR("expected (");
            return nullptr;
        }
        m_parsec.spaces();

        auto line = m_parsec.get_line();
        auto column = m_parsec.get_column();

        auto t = parse_type();
        if (!t)
            return nullptr;

        m_parsec.space();
        if (m_parsec.is_fail())
            return nullptr;

        auto id = parse_id();
        if (m_parsec.is_fail()) {
            SYNTAXERR("expected identifier");
            return nullptr;
        }

        m_parsec.spaces();
        m_parsec.character(')');
        if (m_parsec.is_fail()) {
            SYNTAXERR("expected )");
            return nullptr;
        }

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

        char tmp;
        PTRY(m_parsec, tmp, [](parsec &p) {
            p.spaces();
            return p.character(')');
        }(m_parsec));

        if (!m_parsec.is_fail())
            return st;

        m_parsec.space();
        if (m_parsec.is_fail()) {
            SYNTAXERR("expected whitespace");
            return nullptr;
        }
        m_parsec.spaces();
    }

    assert(false);
    return nullptr; // never reach hare
}

ptr_ir_expr ir::parse_expr() {
    std::string id;

    auto line = m_parsec.get_line();
    auto column = m_parsec.get_column();

    PTRY(m_parsec, id, parse_id());
    if (m_parsec.is_fail()) {

        char tmp;
        PTRY(m_parsec, tmp, m_parsec.character('('));
        if (m_parsec.is_fail()) {
            // DECIMAL
            ptr_ir_decimal dec;
            line = m_parsec.get_line();
            column = m_parsec.get_column();
            PTRY(m_parsec, dec, parse_decimal());

            if (dec) {
                dec->m_line = line;
                dec->m_column = column;
                return dec;
            }

            m_parsec.character('0');
            if (!m_parsec.is_fail()) {
                dec = std::make_unique<ir_decimal>();
                dec->m_num = "0";
                dec->m_line = line;
                dec->m_column = column;
                return dec;
            }

            SYNTAXERR("could not parse expression");

            return nullptr;
        }

        line = m_parsec.get_line();
        column = m_parsec.get_column();
        PTRY(m_parsec, id, m_parsec.str("let"));

        // LET
        if (!m_parsec.is_fail()) {
            m_parsec.space();
            if (m_parsec.is_fail()) {
                SYNTAXERR("expected whitespace");
                return nullptr;
            }

            auto let = parse_let();
            if (let) {
                let->m_line = line;
                let->m_column = column;
            }

            return let;
        }

        // APPLY
        auto apply = std::make_unique<ir_apply>();
        for (;;) {
            PTRY(m_parsec, tmp, [](parsec &p) {
                p.spaces();
                return p.character(')');
            }(m_parsec));

            if (!m_parsec.is_fail()) {
                if (apply->m_expr.size() == 0)
                    apply->m_expr_type = ir_expr::EXPRNOP;
                return apply;
            }

            if (apply->m_expr.size() > 0) {
                m_parsec.space();
                if (m_parsec.is_fail()) {
                    SYNTAXERR("expected whitespace");
                    return nullptr;
                }
            }
            m_parsec.spaces();

            auto e = parse_expr();
            if (!e)
                return nullptr;

            apply->m_line = line;
            apply->m_column = column;
            apply->m_expr.push_back(std::move(e));
        }
    } else {
        // BOOL
        if (id == "true") {
            auto b = std::make_unique<ir_bool>();
            b->m_bool = true;
            b->m_line = line;
            b->m_column = column;
            return b;
        } else if (id == "false") {
            auto b = std::make_unique<ir_bool>();
            b->m_bool = true;
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

    return nullptr;
}

ptr_ir_type ir::parse_type() {
    auto line = m_parsec.get_line();
    auto column = m_parsec.get_column();

    char tmp;
    PTRY(m_parsec, tmp, m_parsec.character('('));
    if (!m_parsec.is_fail()) {
        m_parsec.spaces();
        auto id = parse_id();
        if (m_parsec.is_fail()) {
            SYNTAXERR("expected identifier");
            return nullptr;
        }

        if (id == "ref") {
            auto ref = parse_ref();
            ref->m_line = line;
            ref->m_column = column;
            return ref;
        } else if (id == "struct") {
            auto st = parse_struct();
            st->m_line = line;
            st->m_column = column;
            return st;
        }

        return nullptr;
    }

    std::string s = parse_id();
    if (m_parsec.is_fail())
        return nullptr;

    if (s == "u64") {
        auto t = new ir_scalar;
        t->m_type = TYPE_U64;
        t->m_line = line;
        t->m_column = column;
        return ptr_ir_type((ir_type *)t);
    } else if (s == "s64") {
        auto t = new ir_scalar;
        t->m_type = TYPE_S64;
        t->m_line = line;
        t->m_column = column;
        return ptr_ir_type((ir_type *)t);
    } else if (s == "u32") {
        auto t = new ir_scalar;
        t->m_type = TYPE_U32;
        t->m_line = line;
        t->m_column = column;
        return ptr_ir_type((ir_type *)t);
    } else if (s == "s32") {
        auto t = new ir_scalar;
        t->m_type = TYPE_S32;
        t->m_line = line;
        t->m_column = column;
        return ptr_ir_type((ir_type *)t);
    } else if (s == "u16") {
        auto t = new ir_scalar;
        t->m_type = TYPE_U16;
        t->m_line = line;
        t->m_column = column;
        return ptr_ir_type((ir_type *)t);
    } else if (s == "s16") {
        auto t = new ir_scalar;
        t->m_type = TYPE_S16;
        t->m_line = line;
        t->m_column = column;
        return ptr_ir_type((ir_type *)t);
    } else if (s == "u8") {
        auto t = new ir_scalar;
        t->m_type = TYPE_U8;
        t->m_line = line;
        t->m_column = column;
        return ptr_ir_type((ir_type *)t);
    } else if (s == "s8") {
        auto t = new ir_scalar;
        t->m_type = TYPE_S8;
        t->m_line = line;
        t->m_column = column;
        return ptr_ir_type((ir_type *)t);
    } else if (s == "bool") {
        auto t = new ir_scalar;
        t->m_type = TYPE_BOOL;
        t->m_line = line;
        t->m_column = column;
        return ptr_ir_type((ir_type *)t);
    } else if (s == "fp64") {
        auto t = new ir_scalar;
        t->m_type = TYPE_FP32;
        t->m_line = line;
        t->m_column = column;
        return ptr_ir_type((ir_type *)t);
    } else if (s == "fp32") {
        auto t = new ir_scalar;
        t->m_type = TYPE_FP32;
        t->m_line = line;
        t->m_column = column;
        return ptr_ir_type((ir_type *)t);
    } else {
        auto user = std::make_unique<ir_usertype>();
        user->m_name = s;
        user->m_line = line;
        user->m_column = column;
        return user;
    }

    return nullptr;
}

ptr_ir_struct ir::parse_struct() {
    m_parsec.space();
    if (m_parsec.is_fail()) {
        SYNTAXERR("expected whitespace");
        return nullptr;
    }
    m_parsec.spaces();

    auto st = std::make_unique<ir_struct>();

    for (;;) {
        auto t = parse_type();
        if (!t)
            return nullptr;

        st->m_member.push_back(std::move(t));

        char tmp;
        PTRY(m_parsec, tmp, [](parsec &p) {
            p.spaces();
            return p.character(')');
        }(m_parsec));

        if (!m_parsec.is_fail())
            return st;

        m_parsec.space();
        if (m_parsec.is_fail()) {
            SYNTAXERR("expected whitespace");
            return nullptr;
        }
        m_parsec.spaces();
    }

    assert(false);
    return nullptr; // never reach hare
}

ptr_ir_type ir::parse_ref() {
    m_parsec.space();
    if (m_parsec.is_fail()) {
        SYNTAXERR("expected whitespace");
        return nullptr;
    }
    m_parsec.spaces();

    auto t = parse_type();
    if (!t)
        return nullptr;

    m_parsec.spaces();
    m_parsec.character(')');
    if (m_parsec.is_fail()) {
        SYNTAXERR("expected )");
        return nullptr;
    }

    auto ref = std::make_unique<ir_ref>();
    ref->m_type = std::move(t);

    return ref;
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
        SYNTAXERR("expected whitespace");
        return nullptr;
    }

    m_parsec.spaces();

    // parse return types
    auto t = parse_type();
    if (!t) {
        SYNTAXERR("expected a type specifier");
        return nullptr;
    }

    defun->m_ret = std::move(t);

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
                SYNTAXERR("expected a type specifier");
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

            auto p = std::make_unique<ir_id>();
            p->m_id = id;
            p->m_type = std::move(t);
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

    m_parsec.spaces();
    m_parsec.character(')');
    if (m_parsec.is_fail()) {
        SYNTAXERR("expected )");
        return nullptr;
    }

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

ptr_ir_decimal ir::parse_decimal() {
    std::string num;
    auto line = m_parsec.get_line();
    auto column = m_parsec.get_column();
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

ptr_ir_let ir::parse_let() {
    m_parsec.spaces();

    // parse definitions
    m_parsec.character('(');
    if (m_parsec.is_fail()) {
        SYNTAXERR("expected (");
        return nullptr;
    }

    auto let = std::make_unique<ir_let>();

    for (;;) {
        if (let->m_def.size() > 0) {
            m_parsec.space();
            if (m_parsec.is_fail()) {
                SYNTAXERR("expected whitespace");
                return nullptr;
            }
        }

        m_parsec.spaces();
        m_parsec.character('(');
        if (m_parsec.is_fail()) {
            SYNTAXERR("expected (");
            return nullptr;
        }

        auto type = parse_type();
        if (m_parsec.is_fail())
            return nullptr;

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

        m_parsec.space();
        if (m_parsec.is_fail()) {
            SYNTAXERR("expected whitespace");
            return nullptr;
        }

        auto e = parse_expr();
        if (!e)
            return nullptr;

        m_parsec.spaces();
        m_parsec.character(')');
        if (m_parsec.is_fail()) {
            SYNTAXERR("expected )");
            return nullptr;
        }

        auto p = std::make_unique<ir_let::var>();
        p->m_id = std::make_unique<ir_id>();
        p->m_id->m_id = id;
        p->m_id->m_type = std::shared_ptr<ir_type>(type->clone());
        p->m_expr = std::move(e);
        let->m_def.push_back(std::move(p));

        char tmp;
        PTRY(m_parsec, tmp, [](parsec &p) {
            p.spaces();
            return p.character(')');
        }(m_parsec));

        if (!m_parsec.is_fail())
            break;
    }

    m_parsec.space();
    if (m_parsec.is_fail()) {
        SYNTAXERR("expected whitespace");
        return nullptr;
    }

    auto e = parse_expr();
    if (!e)
        return nullptr;

    let->m_expr = std::move(e);

    m_parsec.spaces();
    m_parsec.character(')');
    if (m_parsec.is_fail()) {
        SYNTAXERR("expected )");
        return nullptr;
    }

    return let;
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
    case TYPE_REF:
    case TYPE_STRUCT:
        assert(m_type != TYPE_REF && m_type != TYPE_STRUCT);
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

bool ir::check_type() {
    for (auto &s : m_struct) {
        if (HASKEY(m_id2struct, s->m_name)) {
            SEMANTICERR(*this, s.get(), "%s is multiply defined",
                        s->m_name.c_str());
            return false;
        }
    }

    for (auto &s : m_struct) {
        std::unordered_set<std::string> used;
        used.insert(s->m_name);
        if (!check_recursive(s.get(), used))
            return false;

        m_id2struct[s->m_name] =
            std::unique_ptr<ir_struct>((ir_struct *)s->clone());
    }

    for (auto &p : m_defuns) {
        if (HASKEY(m_id2fun, p->m_name) || HASKEY(m_id2struct, p->m_name)) {
            SEMANTICERR(*this, p.get(), "%s is multiply defined",
                        p->m_name.c_str());
            return false;
        }
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
    }

    return true;
}

shared_ir_type ir::resolve_type(shared_ir_type type) const {
    switch (type->m_irtype) {
    case ir_type::IRTYPE_SCALAR:
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
        if (p->m_type->m_irtype == ir_type::IRTYPE_USER) {
            auto t = resolve_type(p->m_type);
            if (t)
                p->m_type = t;
            else
                return nullptr;
        }
        return type;
    }
    case ir_type::IRTYPE_USER: {
        auto p = (ir_usertype *)type.get();
        auto it = m_id2struct.find(p->m_name);
        if (it == m_id2struct.end()) {
            SEMANTICERR(*this, type, "%s is undefined", p->m_name.c_str());
            return nullptr;
        }
        return shared_ir_type(it->second->clone());
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

            q = std::shared_ptr<ir_type>(it->second->clone());
        }
    }

    return true;
}

bool ir_defun::check_type(const ir &ref) {
    ir_expr::id2type vars;

    resolve_funtype();

    m_funtype =
        std::static_pointer_cast<ir_funtype>(ref.resolve_type(m_funtype));
    if (!m_funtype)
        return false;

    int n = 0;
    for (auto &p : m_args) {
        vars[p->m_id].push_back(m_funtype->m_args[n]);
        n++;
    }

    if (!m_expr->check_type(ref, vars))
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

shared_ir_type ir_bool::check_type(const ir &ref, id2type &vars) {
    auto p = new ir_scalar;
    p->m_type = TYPE_BOOL;

    m_type = shared_ir_type(p);

    return m_type;
}

shared_ir_type ir_let::check_type(const ir &ref, id2type &vars) {
    for (auto &p : m_def) {
        p->m_id->m_type = ref.resolve_type(p->m_id->m_type);
        if (!p->m_id->m_type)
            return nullptr;

        if (!p->m_expr->check_type(ref, vars))
            return nullptr;

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

shared_ir_type ir_apply::check_type(const ir &ref, id2type &vars) {
    auto first = m_expr.begin();
    if (first == m_expr.end())
        return nullptr;

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

                for (int i = 2; i < m_expr.size(); i++) {
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
                return nullptr;
            }
        } else if (id->m_id == "if") {
            return check_ifexpr(ref, vars);
        } else if (id->m_id == ">=" || id->m_id == "<=") {
            return check_magnitude(ref, vars, id->m_id);
        } else if (id->m_id == "!=") {
            return check_eq(ref, vars);
        } else {
            // function call
            return check_call(ref, vars, id->m_id);
        }
    }

    // lambda or higher order function

    return nullptr;
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
                    std::shared_ptr<ir_type>(it->second->clone());
            }

            ir_id tmp;
            tmp.m_type = shared_ir_type(s->second->m_member[n]->clone());
            if (!unify_type(&tmp, it->get())) {
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
        auto id = (ir_id *)(m_expr[0].get());
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

bool ir::is_structgen(ir_expr *expr) {
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
    auto i8ptr =
        llvm::PointerType::getUnqual(llvm::IntegerType::get(m_llvm_ctx, 8));
    args.push_back(m_llvm_builder.CreateBitCast(dst, i8ptr));
    args.push_back(m_llvm_builder.CreateBitCast(src, i8ptr));
    args.push_back(
        llvm::ConstantInt::get(m_llvm_ctx, llvm::APInt(64, size, false)));
    args.push_back(
        llvm::ConstantInt::get(m_llvm_ctx, llvm::APInt(1, 0, false)));
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

    // Declare the memcpy
    std::vector<llvm::Type *> vec;
    auto i8ptr =
        llvm::PointerType::getUnqual(llvm::IntegerType::get(m_llvm_ctx, 8));
    vec.push_back(i8ptr); /* i8 */
    vec.push_back(i8ptr); /* i8 */
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
    case TYPE_INT:
    case TYPE_REF:
    case TYPE_STRUCT:
        assert(m_type != TYPE_INT && m_type != TYPE_REF &&
               m_type != TYPE_STRUCT);
        return nullptr;
    }
}

llvm::Type *ir_ref::codegen(ir &ref) {
    auto t = m_type->codegen(ref);
    return llvm::PointerType::getUnqual(t);
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
        builder.CreateRet(retval);

        // Validate the generated code, checking for consistency.
        // llvm::verifyFunction(fun);

        return m_fun;
    }

    return nullptr;
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
        args.push_back(t);
    }

    auto ftype = llvm::FunctionType::get(type, args, false);
    m_fun = llvm::Function::Create(ftype, llvm::Function::ExternalLinkage,
                                   m_name, &ref.get_llvm_module());

    // m_fun->setCallingConv(llvm::CallingConv::Fast);

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
    auto &builder = ref.get_llvm_builder();
    auto &ctx = ref.get_llvm_ctx();
    auto &layout = ref.get_llvm_datalayout();

    for (auto &p : m_def) {
        auto v = p->m_expr->codegen(ref, vals);
        if (!ref.is_structgen(p->m_expr.get())) {
            // deep copy
            auto type = p->m_expr->m_type->codegen(ref);
            auto ptr = builder.CreateAlloca(p->m_expr->m_type->codegen(ref));
            ref.llvm_memcpy(ptr, v, layout.getTypeAllocSize(type));
        }

        vals[p->m_id->m_id].push_back(v);
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

llvm::Value *ir_decimal::codegen(ir &ref, ir_expr::id2val &vals) {
    auto t = (ir_scalar *)m_type.get();
    auto &ctx = ref.get_llvm_ctx();
    switch (t->m_type) {
    case TYPE_U64:
        return llvm::ConstantInt::get(
            ctx, llvm::APInt(64, boost::lexical_cast<uint64_t>(m_num), false));
    case TYPE_S64:
        return llvm::ConstantInt::get(
            ctx, llvm::APInt(64, boost::lexical_cast<int64_t>(m_num), true));
    case TYPE_U32:
        return llvm::ConstantInt::get(
            ctx, llvm::APInt(32, boost::lexical_cast<uint32_t>(m_num), false));
    case TYPE_S32:
        return llvm::ConstantInt::get(
            ctx, llvm::APInt(32, boost::lexical_cast<int32_t>(m_num), true));
    default:
        return nullptr;
    }
    return nullptr;
}

llvm::Value *ir_bool::codegen(ir &ref, ir_expr::id2val &vals) {
    if (m_bool)
        return llvm::ConstantInt::get(ref.get_llvm_ctx(),
                                      llvm::APInt(1, 1, false));
    else
        return llvm::ConstantInt::get(ref.get_llvm_ctx(),
                                      llvm::APInt(1, 0, false));

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
        for (auto i = 2; i < m_expr.size(); i++) {                             \
            auto e2 = m_expr[i]->codegen(ref, vals);                           \
            ir_scalar *s = (ir_scalar *)m_expr[i]->m_type.get();               \
            if (e2 == nullptr)                                                 \
                return nullptr;                                                \
                                                                               \
            switch (s->m_type) {                                               \
            case TYPE_U64:                                                     \
            case TYPE_U32:                                                     \
                e1 = ref.get_llvm_builder().UIOP(e1, e2, NAME);                \
                break;                                                         \
            case TYPE_S64:                                                     \
            case TYPE_S32:                                                     \
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
            e1 = ref.get_llvm_builder().UIOP(e1, e2, NAME);                    \
            break;                                                             \
        case TYPE_S64:                                                         \
        case TYPE_S32:                                                         \
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
    auto first = m_expr.begin();
    if (first == m_expr.end())
        return nullptr;

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
                return nullptr;
            }
        } else if (id->m_id == "if") {
            return codegen_ifexpr(ref, vals);
        } else if (id->m_id == ">=") {
            BINOP(CreateICmpUGE, CreateICmpSGE, CreateFCmpOGE, ">=", "getmp");
        } else if (id->m_id == "<=") {
            BINOP(CreateICmpULE, CreateICmpSLE, CreateFCmpOLE, "<=", "letmp");
        } else if (id->m_id == "!=") {
            BINOP(CreateICmpNE, CreateICmpNE, CreateFCmpONE, "!=", "netmp");
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

llvm::Value *ir_apply::struct_gen(ir &ref, id2val vals,
                                  llvm::StructType *type) {
    auto &builder = ref.get_llvm_builder();
    auto alloc = builder.CreateAlloca(type);

    struct_gen2(ref, vals, type, m_expr, alloc);

    return alloc;
}

void ir_apply::struct_gen2(ir &ref, id2val vals, llvm::StructType *type,
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
        builder.CreateStore(t, ptr);
    }
}

llvm::Value *ir_apply::codegen_ifexpr(ir &ref, id2val vals) {
    llvm::Value *condv = m_expr[1]->codegen(ref, vals);
    if (!condv)
        return nullptr;

    auto &builder = ref.get_llvm_builder();
    auto &ctx = ref.get_llvm_ctx();

    condv = builder.CreateICmpEQ(
        condv, llvm::ConstantInt::get(ctx, llvm::APInt(1, 1, false)), "ifcond");

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

llvm::Value *ir_apply::codegen_call(ir &ref, id2val vals,
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
        args.push_back(t);
    }

    auto ret = ref.get_llvm_builder().CreateCall(fun, args, "calltmp");
    /*
    if (ret) {
        ret->setCallingConv(llvm::CallingConv::Fast);
        ret->setTailCall();
    } */

    return ret;
}

void ir::print_err(std::size_t line, std::size_t column) const {
    std::vector<std::string> lines;

    boost::split(lines, m_parsec.get_str(), boost::is_any_of("\n"));

    std::cerr << lines[line - 1] << std::endl;

    for (auto i = 0; i < column - 1; i++) {
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

void ir_funtype::print() {}

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

    n = 0;
    std::cout << "], \"expr\":";
    m_expr->print();
    std::cout << "}}";
}

} // namespace lunar