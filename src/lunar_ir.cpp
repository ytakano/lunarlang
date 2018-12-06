#include "lunar_ir.hpp"

#include <iostream>

#define SYNTAXERR(M, ...)                                                      \
    fprintf(stderr, "%s:%lu:%lu: syntax error: " M "\n", m_filename.c_str(),   \
            m_parsec.get_line(), m_parsec.get_column(), ##__VA_ARGS__)

#define SEMANTICERR(M, ...)                                                    \
    fprintf(stderr, "%s:%lu:%lu: semantic error: " M "\n", m_filename.c_str(), \
            m_parsec.get_line(), m_parsec.get_column(), ##__VA_ARGS__)

namespace lunar {

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
        char tmp;
        PTRY(m_parsec, tmp, m_parsec.character('('));
        if (m_parsec.is_fail()) {
            // DECIMAL
            ptr_ir_decimal dec;
            PTRY(m_parsec, dec, parse_decimal());
            if (dec)
                return dec;

            SYNTAXERR("could not parse expression");

            return nullptr;
        }

        PTRY(m_parsec, id, m_parsec.str("let"));

        // LET
        if (!m_parsec.is_fail()) {
            m_parsec.space();
            if (m_parsec.is_fail()) {
                SYNTAXERR("expected whitespace");
                return nullptr;
            }

            return parse_let();
        }

        // APPLY
        auto apply = std::make_unique<ir_apply>();
        for (;;) {
            PTRY(m_parsec, tmp, [](parsec &p) {
                p.spaces();
                return p.character(')');
            }(m_parsec));

            if (!m_parsec.is_fail())
                return apply;

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

            apply->m_expr.push_back(std::move(e));
        }
    } else {
        // IDENTIFIER
        auto e = std::make_unique<ir_id>();
        e->m_id = id;
        return e;
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

ptr_ir_decimal ir::parse_decimal() {
    std::string num;
    char c = m_parsec.oneof(m_1to9);
    if (m_parsec.is_fail())
        return nullptr;

    num.push_back(c);
    PMANY(m_parsec, num, m_parsec.oneof(m_0to9));

    auto d = std::make_unique<ir_decimal>();
    d->m_num = num;

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

        auto p = std::make_unique<std::pair<std::string, ptr_ir_expr>>(
            id, std::move(e));
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

    // parse expr
    for (;;) {
        char tmp;
        PTRY(m_parsec, tmp, [](parsec &p) {
            p.spaces();
            return p.character(')');
        }(m_parsec));

        if (!m_parsec.is_fail())
            return let;

        if (let->m_expr.size() > 0) {
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

        let->m_expr.push_back(std::move(e));
    }

    return nullptr; // never reach here
}

std::string ir::codegen(std::list<ptr_ir_defun> &defuns) {
    for (auto &p : defuns)
        p->codegen(*this);

    std::string s;
    llvm::raw_string_ostream os(s);
    m_llvm_module.print(os, nullptr);

    return s;
}

llvm::Type *ir_scalar::codegen(ir &ref) {
    switch (m_type) {
    case TYPE_BOOL:
        return llvm::Type::getInt1Ty(ref.get_llvm_ctx());
    case TYPE_U32:
        return llvm::Type::getInt32Ty(ref.get_llvm_ctx());
    case TYPE_U64:
        return llvm::Type::getInt64Ty(ref.get_llvm_ctx());
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

    // set name to the arguments
    unsigned i = 0;
    for (auto &arg : fun->args())
        arg.setName(m_args[i++]->second);

    auto bb = llvm::BasicBlock::Create(ref.get_llvm_ctx(), "entry", fun);

    return fun;
}

llvm::Value *ir_id::codegen(
    ir &ref, std::unordered_map<std::string, std::deque<llvm::Value *>> &vals) {
    auto it = vals.find(m_id);
    if (it != vals.end()) {
        return it->second.back();
    }

    return nullptr;
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
        std::cout << "[\"" << p->first << "\",";
        p->second->print();
        std::cout << "]";
        n++;
    }

    n = 0;
    std::cout << "], \"expr\":[";
    for (auto &q : m_expr) {
        if (n > 0)
            std::cout << ",";
        q->print();
        n++;
    }
    std::cout << "]}}";
}

} // namespace lunar