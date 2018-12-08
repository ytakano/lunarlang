#include "lunar_ir.hpp"

#include <iostream>

#define SYNTAXERR(M, ...)                                                      \
    fprintf(stderr, "%s:%lu:%lu: syntax error: " M "\n", m_filename.c_str(),   \
            m_parsec.get_line(), m_parsec.get_column(), ##__VA_ARGS__)

#define SEMANTICERR(IR, AST, M, ...)                                           \
    fprintf(stderr, "%s:%lu:%lu: semantic error: " M "\n",                     \
            (IR).get_filename().c_str(), (AST)->m_line + 1,                    \
            (AST)->m_column + 1, ##__VA_ARGS__)

namespace lunar {

static bool eq_type(const ir_type *lhs, const ir_type *rhs) {
    if (lhs->m_irtype != rhs->m_irtype)
        return false;

    if (lhs->m_irtype == ir_type::IRTYPE_SCALAR) {
        auto s1 = (const ir_scalar *)lhs;
        auto s2 = (const ir_scalar *)rhs;
        return s1->m_type == s2->m_type;
    }

    return false;
}

ir::ir(const std::string &filename, const std::string &str)
    : m_filename(filename), m_parsec(str), m_llvm_builder(m_llvm_ctx),
      m_llvm_module(filename, m_llvm_ctx) {

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

            line = m_parsec.get_line();
            column = m_parsec.get_column();
            auto e = parse_expr();
            if (!e)
                return nullptr;

            apply->m_line = line;
            apply->m_column = column;
            apply->m_expr.push_back(std::move(e));
        }
    } else {
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
        p->m_type = std::move(type);
        p->m_id = id;
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

bool ir_defun::check_type(const ir &ref) {
    /*
    std::unordered_map<std::string, std::deque<ir_type *>> vals;
    for (auto &p : m_args)
        vals[p->second].push_back(p->first.get());

    auto t = m_expr->get_type(ref, vals);

    if (!t)
        return false;

    if (!eqt(m_ret.begin()->get(), t.get())) {
        SEMANTICERR(ref.get_filename().c_str(), m_line, m_column,
                    "the type of the return value of the function \"%s\" "
                    "is different from the type of the value returned by "
                    "the expression",
                    m_name.c_str());
        return false;
    }
*/
    return true;
}
/*
ptr_ir_type ir_decimal::get_type(
    ir &ref, std::unordered_map<std::string, std::deque<ir_type *>> &vals) {
    auto t = std::make_unique<ir_scalar>();

    t->m_type = TYPE_INT;

    return t;
}

ptr_ir_type
ir_let::get_type(ir &ref,
                 std::unordered_map<std::string, std::deque<ir_type *>> &vals) {
    for (auto &p : m_def) {
        auto t = p->m_expr->get_type(ref, vals);
        if (!t)
            return nullptr;

        if (!eqt(p->m_type.get(), t.get())) {
            SEMANTICERR(ref.get_filename().c_str(), p->m_type->m_line,
                        p->m_type->m_column,
                        "the type of the \"%s\" is different from the type "
                        "of the value returned by the expression",
                        p->m_id.c_str());
            return nullptr;
        }

        vals[p->m_id].push_back(p->m_type.get());
    }

    auto e = m_expr->get_type(ref, vals);

    for (auto &p : m_def)
        vals[p->m_id].pop_back();

    return e;
}

ptr_ir_type
ir_id::get_type(ir &ref,
                std::unordered_map<std::string, std::deque<ir_type *>> &vals) {
    auto it = vals.find(m_id);
    if (it == vals.end())
        return nullptr;

    return it->second.back()->clone();
}

ptr_ir_type ir_apply::get_type(
    ir &ref, std::unordered_map<std::string, std::deque<ir_type *>> &vals) {

    auto first = m_expr.begin();
    if (first == m_expr.end())
        return nullptr;

    if ((*first)->m_expr_type == ir_expr::EXPRID) {
        auto id = (ir_id *)(first->get());
        if (id->m_id == "+" || id->m_id == "-" || id->m_id == "*") {
            if (m_expr.size() < 3) {
                SEMANTICERR(ref.get_filename().c_str(), m_line, m_column,
                            "%s requires more than or equal to 2 arguments",
                            id->m_id.c_str());
            }

            auto t1 = m_expr[1]->get_type(ref, vals);
            if (t1 == nullptr)
                return nullptr;

            if (t1->m_irtype != ir_type::IRTYPE_SCALAR) {
                SEMANTICERR(ref.get_filename().c_str(), m_expr[1]->m_line,
                            m_expr[1]->m_column, "value is not scalar type");
                return nullptr;
            }

            for (auto i = 2; i < m_expr.size(); i++) {
                auto t2 = m_expr[i]->get_type(ref, vals);
                if (t2 == nullptr)
                    return nullptr;

                if (t2->m_irtype != ir_type::IRTYPE_SCALAR) {
                    SEMANTICERR(ref.get_filename().c_str(), m_expr[i]->m_line,
                                m_expr[i]->m_column,
                                "value is not scalar type");
                    return nullptr;
                }

                auto s1 = (ir_scalar *)t1.get();
                auto s2 = (ir_scalar *)t2.get();

                if (s1->m_type == TYPE_INT) {
                    s1->m_type = s2->m_type;
                } else if (s2->m_type == TYPE_INT) {
                    continue;
                } else if (s1->m_type != s2->m_type) {
                    SEMANTICERR(ref.get_filename().c_str(), m_expr[i]->m_line,
                                m_expr[i]->m_column, "unexpected type");
                    return nullptr;
                }
            }
            return t1;
        }
    }

    return nullptr;
}
*/

ir_expr::shared_type ir_id::check_type(const ir &ref, id2type &vars) {
    auto it = vars.find(m_id);
    if (it == vars.end()) {
        SEMANTICERR(ref, this, "%s is undefined", m_id.c_str());
        return nullptr;
    }

    assert(it->second.size() > 0);

    m_type = it->second.back();
    return m_type;
}

ir_expr::shared_type ir_decimal::check_type(const ir &ref, id2type &vars) {
    auto p = new ir_scalar;
    p->m_type = TYPE_INT;

    m_type = shared_type(p);

    return m_type;
}

ir_expr::shared_type ir_let::check_type(const ir &ref, id2type &vars) {
    return nullptr;
}

ir_expr::shared_type ir_apply::check_type(const ir &ref, id2type &vars) {
    return nullptr;
}

std::string ir::codegen(std::list<ptr_ir_defun> &defuns) {
    for (auto &p : defuns) {
        p->codegen(*this);
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
    case TYPE_INT:
        return nullptr;
    }

    return nullptr;
}

llvm::Function *ir_defun::codegen(ir &ref) {
    // type of return values
    llvm::Type *type;
    if (m_ret.size() == 1) {
        type = (*m_ret.begin())->codegen(ref);
        if (type == nullptr)
            return nullptr;
    } else {
        std::vector<llvm::Type *> types;

        for (auto &p : m_ret) {
            auto t = p->codegen(ref);
            if (t == nullptr)
                return nullptr;
            types.push_back(t);
        }

        type = llvm::StructType::get(ref.get_llvm_ctx(), types);
        if (type == nullptr)
            return nullptr;
    }

    // type of arguments
    std::vector<llvm::Type *> args;
    for (auto &q : m_args) {
        auto t = q->first->codegen(ref);
        if (t == nullptr)
            return nullptr;
        args.push_back(t);
    }

    auto ftype = llvm::FunctionType::get(type, args, false);
    auto fun = llvm::Function::Create(ftype, llvm::Function::ExternalLinkage,
                                      m_name, &ref.get_llvm_module());

    // bind name to the arguments and
    std::unordered_map<std::string, std::deque<llvm::Value *>> vars;
    unsigned i = 0;
    for (auto &arg : fun->args()) {
        auto s = m_args[i]->second;
        arg.setName(s);
        vars[s].push_back(&arg);
        i++;
    }

    auto bb = llvm::BasicBlock::Create(ref.get_llvm_ctx(), "entry", fun);
    auto &builder = ref.get_llvm_builder();
    builder.SetInsertPoint(bb);

    llvm::Value *retval = m_expr->codegen(ref, vars);
    if (retval) {
        builder.CreateRet(retval);

        // Validate the generated code, checking for consistency.
        // llvm::verifyFunction(fun);

        return fun;
    }

    return nullptr;
}

llvm::Value *ir_id::codegen(
    ir &ref, std::unordered_map<std::string, std::deque<llvm::Value *>> &vals) {
    auto it = vals.find(m_id);
    if (it != vals.end()) {
        return it->second.back();
    }

    return nullptr;
}

llvm::Value *ir_let::codegen(
    ir &ref, std::unordered_map<std::string, std::deque<llvm::Value *>> &vals) {

    for (auto &p : m_def) {
        auto v = p->m_expr->codegen(ref, vals);
        vals[p->m_id].push_back(v);
    }

    auto e = m_expr->codegen(ref, vals);

    for (auto &p : m_def) {
        auto it = vals.find(p->m_id);
        it->second.pop_back();
        if (it->second.size() == 0) {
            vals.erase(it);
        }
    }

    return e;
}

llvm::Value *ir_decimal::codegen(
    ir &ref, std::unordered_map<std::string, std::deque<llvm::Value *>> &vals) {
    return nullptr;
}

#define BINARYOP(INTOP, FOP, OP, NAME)                                         \
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
            if (e2 == nullptr)                                                 \
                return nullptr;                                                \
                                                                               \
            e1 = ref.get_llvm_builder().INTOP(e1, e2, NAME);                   \
        }                                                                      \
        return e1;                                                             \
    } while (0)

llvm::Value *ir_apply::codegen(
    ir &ref, std::unordered_map<std::string, std::deque<llvm::Value *>> &vals) {

    auto first = m_expr.begin();
    if (first == m_expr.end())
        return nullptr;

    if ((*first)->m_expr_type == ir_expr::EXPRID) {
        auto id = (ir_id *)(first->get());
        if (id->m_id == "+") {
            BINARYOP(CreateAdd, CreateFAdd, "+", "addtmp");
        } else if (id->m_id == "-") {
            BINARYOP(CreateSub, CreateFSub, "-", "subtmp");
        } else if (id->m_id == "*") {
            BINARYOP(CreateMul, CreateFMul, "*", "multmp");
        }
    }

    return nullptr;
}

void ir_scalar::print() {
    std::cout << "\"";
    switch (m_type) {
    case TYPE_BOOL:
        std::cout << "bool";
        break;
    case TYPE_FP64:
        std::cout << "fp64";
        break;
    case TYPE_FP32:
        std::cout << "fp32";
        break;
    case TYPE_U64:
        std::cout << "u64";
        break;
    case TYPE_S64:
        std::cout << "s64";
        break;
    case TYPE_U32:
        std::cout << "u32";
        break;
    case TYPE_S32:
        std::cout << "s32";
        break;
    case TYPE_INT:
        std::cout << "int";
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

void ir_decimal::print() { std::cout << "{\"decimal\":" << m_num << "}"; }

void ir_let::print() {
    int n = 0;
    std::cout << "{\"let\":{\"def\":[";
    for (auto &p : m_def) {
        if (n > 0)
            std::cout << ",";
        std::cout << "{\"type\":";
        p->m_type->print();
        std::cout << ",\"id\":\"" << p->m_id << "\",\"expr\":";
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