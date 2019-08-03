#include "lunar_parser.hpp"
#include "lunar_print.hpp"

#include <fstream>

#define MODULENOTFOUND(M, AST, ID)                                             \
    do {                                                                       \
        fprintf(stderr, "%s:%lu:%lu:(%d) module \"%s\" was not found: \n",     \
                (M)->m_filename.c_str(), (AST)->m_line, (AST)->m_column,       \
                __LINE__, (ID).c_str());                                       \
        print_err(AST->m_line, AST->m_column, (M)->m_parsec.get_str());        \
    } while (0)

#define SYNTAXERR(M, ...)                                                      \
    do {                                                                       \
        fprintf(stderr, "%s:%lu:%lu:(%d) syntax error: " M "\n",               \
                m_filename.c_str(), m_parsec.get_line(),                       \
                m_parsec.get_column(), __LINE__, ##__VA_ARGS__);               \
        print_err(m_parsec.get_line(), m_parsec.get_column(),                  \
                  m_parsec.get_str());                                         \
    } while (0)

#define SYNTAXERR2(M, LINE, COLUMN, ...)                                       \
    do {                                                                       \
        fprintf(stderr, "%s:%lu:%lu:(%d) syntax error: " M "\n",               \
                m_filename.c_str(), LINE, COLUMN, __LINE__, ##__VA_ARGS__);    \
        print_err(LINE, COLUMN, m_parsec.get_str());                           \
    } while (0)

#define MULTIDEFERR(ID, FILE1, AST1, FILE2, AST2)                              \
    do {                                                                       \
        fprintf(stderr,                                                        \
                "(%s:%d) semantic error: \"%s\" is multiply defined\n",        \
                __FILE__, __LINE__, (ID).c_str());                             \
        fprintf(stderr, "%s:%lu:%lu:\n", (FILE1).c_str(), (AST1)->m_line,      \
                (AST1)->m_column);                                             \
        print_err((AST1)->m_line, (AST1)->m_column, m_parsec.get_str());       \
        fprintf(stderr, "%s:%lu:%lu:\n", (FILE2).c_str(), (AST2)->m_line,      \
                (AST2)->m_column);                                             \
        print_err((AST2)->m_line, (AST2)->m_column, m_parsec.get_str());       \
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
        (ID) = parse_tvar_id();                                                \
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

    m_prefix.insert('-');
    m_prefix.insert('*');

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

void module_tree::add(ptr_ast_import ptr, size_t n) {
    auto &id = ptr->m_id->m_ids[n]->m_id;
    auto it = m_children.find(id);
    if (it == m_children.end()) {
        m_children[id] = std::make_unique<module_tree>();
        it = m_children.find(id);
    }

    n++;
    if (n < ptr->m_id->m_ids.size()) {
        it->second->add(std::move(ptr), n);
    } else {
        it->second->m_import = std::move(ptr);
    }
}

const ast_import *module_tree::find(const std::vector<ptr_ast_id> &id,
                                    unsigned int &pos) const {
    auto it = m_children.find(id[pos]->m_id);

    if (it != m_children.end()) { // found
        pos++;
        if (it->second->m_import) {
            return it->second->m_import.get();
        } else {
            if (pos < id.size())
                return it->second->find(id, pos);
        }
    }

    return nullptr;
}

module::module(const std::string &filename, const std::string &str, parser &p)
    : m_parsec(str), m_filename(filename), m_parser(p), m_is_parsed(false),
      m_is_loaded_module(false) {
    fs::path fpath(filename.c_str());
    fs::path dir = fpath.parent_path();
    m_env.add(dir);
}

bool module::parse() {
    if (m_is_parsed)
        return true;

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

            // check multiple definition
            if (is_defined(fn->m_id->m_id, fn.get()))
                return false;

            m_id2defun[fn->m_id->m_id] = std::move(fn);
        } else if (id->m_id == "instance") {
            auto inst = parse_instance();
            if (!inst)
                return false;

            inst->m_line = id->m_line;
            inst->m_column = id->m_column;

            // TODO: check multiple definition
            m_id2inst.insert(std::pair<std::string, ptr_ast_instance>(
                inst->m_arg->m_id->get_id(), std::move(inst)));
        } else if (id->m_id == "class") {
            auto cls = parse_class();
            if (!cls)
                return false;

            cls->m_line = id->m_line;
            cls->m_column = id->m_column;

            // check multiple definition
            if (is_defined(cls->m_id->m_id, cls.get()))
                return false;

            for (auto &iface : cls->m_interfaces->m_interfaces) {
                for (auto &id : iface->m_id) {
                    m_id2interface[id->m_id] = iface.get();
                }
            }

            m_id2class[cls->m_id->m_id] = std::move(cls);
        } else if (id->m_id == "struct") {
            auto st = parse_struct();
            if (!st)
                return false;

            st->m_line = id->m_line;
            st->m_column = id->m_column;

            // check multiple definition
            if (is_defined(st->m_id->m_id, st.get()))
                return false;

            m_id2struct[st->m_id->m_id] = std::move(st);
        } else if (id->m_id == "union") {
            auto un = parse_union();
            if (!un)
                return false;

            un->m_line = id->m_line;
            un->m_column = id->m_column;

            // check multiple definition
            if (is_defined(un->m_id->m_id, un.get()))
                return false;

            m_id2union[un->m_id->m_id] = std::move(un);
        } else if (id->m_id == "import") {
            auto im = parse_import();
            if (!im)
                return false;

            im->m_line = id->m_line;
            im->m_column = id->m_column;

            if (im->m_as) {
                // check multiple definition
                if (is_defined(im->m_as->m_id, im.get()))
                    return false;

                m_id2import[im->m_as->m_id] = std::move(im);
            } else if (im->m_is_here) {
                m_vec_modules.push_back(std::move(im));
            } else {
                if (im->m_id->m_ids.size() == 1) {
                    // check multiple definition
                    if (is_defined(im->m_id->m_ids[0]->m_id, im.get()))
                        return false;
                }

                m_modules.add(std::move(im));
            }
        } else {
            SYNTAXERR2("unexpected identifier", id->m_line, id->m_column);
            return false;
        }
    }

    m_is_parsed = true;
    return true;
}

#define IS_DEFINED(CONTAINER)                                                  \
    do {                                                                       \
        auto it = CONTAINER.find(str);                                         \
        if (it != CONTAINER.end()) {                                           \
            MULTIDEFERR(str, m_filename, it->second->get_ast_id(), m_filename, \
                        ptr->get_ast_id());                                    \
            return true;                                                       \
        }                                                                      \
    } while (0)

bool module::is_defined(const std::string &str, ast *ptr) {
    IS_DEFINED(m_id2defun);
    IS_DEFINED(m_id2class);
    IS_DEFINED(m_id2struct);
    IS_DEFINED(m_id2union);
    IS_DEFINED(m_id2import);
    IS_DEFINED(m_id2union_mem);
    IS_DEFINED(m_id2interface);

    auto id = ptr->get_ast_id();
    auto it = m_modules.m_children.find(str);
    if (it != m_modules.m_children.end()) {
        fprintf(stderr,
                "(%s:%d) semantic error: \"%s\" is used as module name\n",
                __FILE__, __LINE__, str.c_str());
        fprintf(stderr, "%s:%lu:%lu:\n", m_filename.c_str(), id->m_line,
                id->m_column);
        print_err(id->m_line, id->m_column, m_parsec.get_str());
        return true;
    }

    return false;
}

module *module::find_module(const ast_dotid *dotid, unsigned int &pos) const {
    module *ret = nullptr;

    auto it = m_id2import.find(dotid->m_ids[pos]->m_id);
    if (it != m_id2import.end()) {
        auto it_mod = m_parser.m_modules.find(it->second->m_full_path);
        assert(it_mod != m_parser.m_modules.end());
        ret = it_mod->second.get();
        pos++;
    } else {
        auto ptr_im = m_modules.find(dotid->m_ids, pos);
        if (ptr_im != nullptr) {
            auto it_mod = m_parser.m_modules.find(ptr_im->m_full_path);
            assert(it_mod != m_parser.m_modules.end());
            ret = it_mod->second.get();
        }
    }

    return ret;
}

bool module::find_typeclass(const ast_dotid *dotid, std::string &path,
                            std::string &id, unsigned int pos) const {
    if (dotid->m_ids.size() - pos == 1) {
        auto it = m_id2class.find(dotid->m_ids[pos]->m_id);
        if (it != m_id2class.end()) {
            path = m_filename;
            id = it->second->m_id->m_id;
            return true;
        }
    }

    module *ptr_mod = find_module(dotid, pos);
    if (ptr_mod == nullptr)
        return false;

    return ptr_mod->find_typeclass(dotid, path, id, pos);
}

#define FIND_TYPE_UNIQ(C)                                                      \
    do {                                                                       \
        auto it = (C).find(dotid->m_ids[pos]->m_id);                           \
        if (it != (C).end()) {                                                 \
            path = m_filename;                                                 \
            id = it->first;                                                    \
            return it->second.get();                                           \
        }                                                                      \
    } while (0)

#define FIND_TYPE(C)                                                           \
    do {                                                                       \
        auto it = (C).find(dotid->m_ids[pos]->m_id);                           \
        if (it != (C).end()) {                                                 \
            path = m_filename;                                                 \
            id = it->first;                                                    \
            return it->second;                                                 \
        }                                                                      \
    } while (0)

ast *module::find_type(const ast_dotid *dotid, std::string &path,
                       std::string &id, unsigned int pos) const {
    if (dotid->m_ids.size() - pos == 1) {
        FIND_TYPE_UNIQ(m_id2struct);
        FIND_TYPE_UNIQ(m_id2union);
        FIND_TYPE(m_id2union_mem);
    }

    module *ptr_mod = find_module(dotid, pos);
    if (ptr_mod == nullptr) {
        for (auto it = m_vec_modules.rbegin(); it != m_vec_modules.rend();
             ++it) {
            auto it_mod = m_parser.m_modules.find((*it)->m_full_path);
            assert(it_mod != m_parser.m_modules.end());
            return it_mod->second->find_type(dotid, path, id, pos);
        }
        return nullptr;
    }

    return ptr_mod->find_type(dotid, path, id, pos);
}

ast *module::find_func(const ast_dotid *dotid, std::string &path,
                       std::string &id, unsigned int pos) const {
    if (dotid->m_ids.size() - pos == 1) {
        FIND_TYPE_UNIQ(m_id2defun);
        FIND_TYPE(m_id2interface);
    }

    module *ptr_mod = find_module(dotid, pos);
    if (ptr_mod == nullptr) {
        for (auto it = m_vec_modules.rbegin(); it != m_vec_modules.rend();
             ++it) {
            auto it_mod = m_parser.m_modules.find((*it)->m_full_path);
            assert(it_mod != m_parser.m_modules.end());
            return it_mod->second->find_func(dotid, path, id, pos);
        }
        return nullptr;
    }

    return ptr_mod->find_func(dotid, path, id, pos);
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

// $DOTID = $ID | $ID . $DOTID
ptr_ast_dotid module::parse_dotid() {
    auto ret = std::make_unique<ast_dotid>();

    ret->set_pos(m_parsec);

    ptr_ast_id id;
    PARSEID(id, m_parsec);
    ret->m_ids.push_back(std::move(id));

    for (;;) {
        char c;
        PTRY(m_parsec, c, [](module &m) {
            m.parse_spaces();
            return m.m_parsec.character('.');
        }(*this));

        if (m_parsec.is_fail())
            break;

        parse_spaces();
        PARSEID(id, m_parsec);
        ret->m_ids.push_back(std::move(id));
    }

    return ret;
}

// $IMPORT := import $DOTID $HEREAS?
// $HEREAS := here | $AS
// $AS := as $ID
ptr_ast_import module::parse_import() {
    auto ret = std::make_unique<ast_import>();

    SPACEPLUS();

    ret->m_id = parse_dotid();
    if (!ret->m_id)
        return nullptr;

    bool is_here;
    PTRY(m_parsec, is_here, [](module &m) {
        m.parse_spaces_plus();
        if (m.m_parsec.is_fail())
            return false;
        m.m_parsec.str("here");
        return !m.m_parsec.is_fail();
    }(*this))

    if (is_here) {
        ret->m_is_here = true;
        return ret;
    }

    bool is_as;
    PTRY(m_parsec, is_as, [](module &m) {
        m.parse_spaces_plus();
        if (m.m_parsec.is_fail())
            return false;
        m.m_parsec.str("as");
        return !m.m_parsec.is_fail();
    }(*this))

    if (is_as) {
        // as $ID
        SPACEPLUS();
        PARSEID(ret->m_as, m_parsec);
    }

    return ret;
}

// $TVAR := `$ID
ptr_ast_id module::parse_tvar_id() {
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
ptr_ast_tvar module::parse_tvarkind() {
    auto ret = std::make_unique<ast_tvar>();

    ret->set_pos(m_parsec);
    PARSETVAR(ret->m_id, m_parsec);

    parse_spaces();

    char c;
    PEEK(c, m_parsec);

    if (c == ':') {
        PARSESTR("::", m_parsec);
        parse_spaces();

        ret->m_kind = parse_kind();
        if (!ret->m_kind)
            return nullptr;
    }

    return ret;
}

// $TVARKINDS := $TVARKIND | $TVARKIND , $TVARKINDS
// $TVARS := <$TVARKINDS>
ptr_ast_tvars module::parse_tvars() {
    auto tvars = std::make_unique<ast_tvars>();

    tvars->set_pos(m_parsec);
    PARSECHAR('<', m_parsec);

    parse_spaces();

    for (;;) {
        auto arg = parse_tvarkind();
        if (!arg)
            return nullptr;

        parse_spaces();

        tvars->m_args.push_back(std::move(arg));

        char c;
        PEEK(c, m_parsec);

        if (c == '>')
            break;

        PARSECHAR(',', m_parsec);
        parse_spaces();
    }

    m_parsec.character('>');
    return tvars;
}

// $TYPE := $IDTVAR <$TYPES>? | func ( $TYPES? ) $TYPESPEC |
//          ( $TYPES? ) | [ $TYPE $ARRNUM? ]
// $ARRNUM := * $DECIMAL | * $DECIMAL $ARRNUM
// $IDTVAR := $DOTID | $TVAR
// $TYPESPEC := : $TYPE
ptr_ast_type module::parse_type(bool is_funret = false) {
    char c;
    PEEK(c, m_parsec);

    switch (c) {
    case '`': {
        // $TVAR <$TYPES>?
        auto ret = std::make_unique<ast_normaltype>();
        ret->set_pos(m_parsec);
        PARSETVAR(ret->m_tvar, m_parsec);

        parse_arg_types(ret->m_args);

        return ret;
    }
    case '(': {
        // ( $TYPES? )
        auto ret = std::make_unique<ast_tupletype>();
        ret->set_pos(m_parsec);
        m_parsec.character('(');
        parse_spaces();

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
    case '[': {
        // [ $TYPE $ARRNUM? ]
        auto ret = std::make_unique<ast_vectype>();
        ret->set_pos(m_parsec);
        m_parsec.character('[');
        parse_spaces();

        ret->m_vectype = parse_type();
        if (!ret->m_vectype)
            return nullptr;

        parse_spaces();
        PTRY(m_parsec, c, m_parsec.character(']'));
        if (m_parsec.is_fail()) {
            // * $DECIMAL
            PARSECHAR('*', m_parsec);
            parse_spaces();
            for (;;) {
                auto dec = parse_num();
                if (!dec)
                    return nullptr;

                ret->m_nums.push_back(std::move(dec));

                parse_spaces();

                char c;
                PEEK(c, m_parsec);
                if (c == ']') {
                    PARSECHAR(']', m_parsec);
                    break;
                } else if (c == '*') {
                    PARSECHAR('*', m_parsec);
                    parse_spaces();
                } else {
                    SYNTAXERR("expected '*' or ']'");
                }
            }
        }

        return ret;
    }
    default: {
        bool is_func;
        auto line = m_parsec.get_line();
        auto column = m_parsec.get_column();

        PTRY(m_parsec, is_func, [](module &m) {
            std::string id;
            PMANYONE(m.m_parsec, id,
                     m.m_parsec.oneof_not(m.m_parser.m_no_id_char));

            if (id == "func")
                return true;

            m.m_parsec.set_fail(true);
            return false;
        }(*this));

        if (is_func) {
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

            ret->m_line = line;
            ret->m_column = column;

            return ret;
        } else {
            // $TYPE <$TYPES>?
            auto ret = std::make_unique<ast_normaltype>();
            ret->m_id = parse_dotid();
            if (!ret->m_id)
                return nullptr;

            ret->m_line = line;
            ret->m_column = column;

            if (!parse_arg_types(ret->m_args))
                return nullptr;

            return ret;
        }
    }
    }

    return nullptr; // never reach here
}

// <$TYPES>?
bool module::parse_arg_types(ptr_ast_types &types) {
    char c;
    PTRY(m_parsec, c, [](module &m) {
        m.parse_spaces();
        return m.m_parsec.character('<');
    }(*this));

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

// $PRED := $DOTID <$TYPE>
ptr_ast_pred module::parse_pred() {
    auto ret = std::make_unique<ast_pred>();

    ret->set_pos(m_parsec);
    ret->m_id = parse_dotid();
    if (!ret->m_id)
        return nullptr;

    parse_spaces();
    PARSECHAR('<', m_parsec);
    parse_spaces();

    // arguments
    ret->m_arg = parse_type();
    if (!ret->m_arg)
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

// $CLASSDECL := class $ID < $TVARKIND > $PREDS? { $INTERFACES $WHITESPACE3* }
ptr_ast_class module::parse_class() {
    SPACEPLUS();

    auto ret = std::make_unique<ast_class>();

    // parse class name
    PARSEID(ret->m_id, m_parsec);

    parse_spaces();

    PARSECHAR('<', m_parsec);
    parse_spaces();

    // parse type variable arguments
    ret->m_tvar = parse_tvarkind();
    if (!ret->m_tvar)
        return nullptr;

    parse_spaces();
    PARSECHAR('>', m_parsec);
    parse_spaces();

    char c;
    PTRY(m_parsec, c, m_parsec.character('{'));

    if (m_parsec.is_fail()) {
        // parse predicates
        ret->m_preds = parse_preds();
        if (!ret->m_preds)
            return nullptr;

        PARSECHAR('{', m_parsec);
    }

    parse_spaces();

    // interfaces
    ret->m_interfaces = parse_interfaces();
    if (!ret->m_interfaces)
        return nullptr;

    return ret;
}

// $INTERFACE := $INTNAMES :: func ( $TYPES? ) $TYPESPEC
// $INTNAMES := $INTNAME | $INTNAME , $INTNAMES
// $INTNAME := $ID | infix $INFIX
ptr_ast_interface module::parse_interface() {
    auto ret = std::make_unique<ast_interface>();

    ret->set_pos(m_parsec);

    for (;;) {
        ptr_ast_id id;
        PARSEID(id, m_parsec);
        if (id->m_id == "infix") {
            parse_spaces();
            auto infix = parse_infix();
            if (!infix)
                return nullptr;
            ret->m_infix.push_back(std::move(infix));
        } else {
            ret->m_id.push_back(std::move(id));
        }

        parse_spaces();

        char c;
        PEEK(c, m_parsec);
        if (c == ':') {
            m_parsec.character(':');
            PARSECHAR(':', m_parsec);
            break;
        } else if (c == ',') {
            m_parsec.character(',');
            parse_spaces();
        } else {
            m_parsec.any();
            SYNTAXERR("unexpected character");
            return nullptr;
        }
    }

    parse_spaces();

    // func $INTNAME
    PARSESTR("func", m_parsec);

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

ptr_ast_prefix module::parse_prefix() {
    char c;

    auto line = m_parsec.get_line();
    auto column = m_parsec.get_column();
    PTRY(m_parsec, c, m_parsec.oneof(m_parser.m_prefix));
    if (!m_parsec.is_fail()) {
        auto ret = std::make_unique<ast_prefix>(c);
        ret->m_line = line;
        ret->m_column = column;
        return ret;
    }

    return nullptr;
}

// $EXPR0 := $PREFIX? $EXPR0'
// $EXPR0' := $ID | $IF | $LET | ( $EXPR , ) | ( $EXPR ) | ( $EXPRS_? ) |
//           { $DICT } | { $EXPRS } | [ $EXPRS_? ] | $LITERAL | $MALLOC
ptr_ast_expr module::parse_expr0() {
    auto prefix = parse_prefix();
    if (prefix)
        parse_spaces();

    char c;
    PEEK(c, m_parsec);

    switch (c) {
    case '(': {
        // ( $EXPR , ) | ( $EXPR ) | ( $EXPRS_? )
        auto ret = parse_parentheses();
        if (ret)
            ret->m_prefix = std::move(prefix);
        return ret;
    }
    case '{': {
        // { $DICT } | { $EXPRS }
        auto ret = parse_braces();
        if (ret)
            ret->m_prefix = std::move(prefix);
        return ret;
    }
    case '[': {
        // [ $EXPRS_? ]
        auto ret = parse_brackets();
        if (ret)
            ret->m_prefix = std::move(prefix);
        return ret;
    }
    case '"': {
        auto ret = parse_str();
        if (ret)
            ret->m_prefix = std::move(prefix);
        return ret;
    }
    }

    if ('0' <= c && c <= '9') {
        // number
        auto ret = parse_num();
        if (ret)
            ret->m_prefix = std::move(prefix);
        return ret;
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
        ret->m_prefix = std::move(prefix);
        return ret;
    } else if (id->m_id == "let") {
        // $LET
        auto ret = parse_let();
        if (!ret)
            return nullptr;

        ret->m_line = id->m_line;
        ret->m_column = id->m_column;
        ret->m_prefix = std::move(prefix);
        return ret;
    } else if (id->m_id == "new" || id->m_id == "shared" ||
               id->m_id == "stack") {
        ast_malloc::MEM memtype;
        if (id->m_id == "new") {
            memtype = ast_malloc::MEM_NEW;
        } else if (id->m_id == "shared") {
            memtype = ast_malloc::MEM_SHARED;
        } else {
            memtype = ast_malloc::MEM_STACK;
        }

        auto ret = parse_malloc(memtype);
        if (!ret)
            return nullptr;

        ret->m_line = id->m_line;
        ret->m_column = id->m_column;
        ret->m_prefix = std::move(prefix);
        return ret;
    } else {
        // $ID
        auto ret = std::make_unique<ast_expr_id>();
        ret->m_line = id->m_line;
        ret->m_column = id->m_column;
        ret->m_id = std::move(id);
        ret->m_prefix = std::move(prefix);
        return ret;
    }

    return nullptr;
}

// $DOTID <$TYPES>? $APPLY
ptr_ast_malloc module::parse_malloc(ast_malloc::MEM memtype) {
    SPACEPLUS();

    auto ret = std::make_unique<ast_malloc>(memtype);

    ret->m_id = parse_dotid();
    if (!ret->m_id)
        return nullptr;

    parse_spaces();

    if (!parse_arg_types(ret->m_types))
        return nullptr;

    parse_spaces();

    ret->m_args = parse_apply();
    if (!ret->m_args)
        return nullptr;

    return ret;
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
        auto ret = std::make_unique<ast_apply>();
        ret->m_args = parse_apply();
        if (!ret->m_args)
            return nullptr;

        ret->m_line = lhs->m_line;
        ret->m_column = lhs->m_column;
        ret->m_func = std::move(lhs);

        return parse_exprp(std::move(ret));
    }
    }

    return lhs;
}

// $APPLY := ( $EXPRS_? )
ptr_ast_exprs module::parse_apply() {
    auto ret = std::make_unique<ast_exprs>();

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

        ret->m_exprs.push_back(std::move(e));

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

// $STR {
bool module::parse_st_un(const char *str) {
    std::string s;
    PTRY(m_parsec, s, [](module &m, const char *p) {
        auto ret = m.m_parsec.str(p);
        m.parse_spaces();
        m.m_parsec.character('{');
        return ret;
    }(*this, str));

    return !m_parsec.is_fail();
}

// $PROD := $PRODTYPE | $PRODTYPE $SEP $PROD
// $PRODTYPE := $ID $TYPESPEC
ptr_ast_member module::parse_prod() {
    auto mem = std::make_unique<ast_member>();
    mem->m_line = m_parsec.get_line();
    mem->m_column = m_parsec.get_column();
    PARSEID(mem->m_id, m_parsec);

    parse_spaces();

    PARSECHAR(':', m_parsec);

    parse_spaces();

    mem->m_type = parse_type();
    if (!mem->m_type)
        return nullptr;

    return mem;
}

// $SUM := $SUMTYPE | $SUMTYPE $SEP $SUM
// $SUMTYPE := $ID | $ID $TYPESPEC
ptr_ast_member module::parse_sum() {
    auto mem = std::make_unique<ast_member>();
    mem->m_line = m_parsec.get_line();
    mem->m_column = m_parsec.get_column();
    PARSEID(mem->m_id, m_parsec);

    char c;
    PTRY(m_parsec, c, [](module &m) {
        m.parse_spaces();
        return m.m_parsec.character(':');
    }(*this));
    if (m_parsec.is_fail())
        return mem;

    parse_spaces();

    mem->m_type = parse_type();
    if (!mem->m_type)
        return nullptr;

    return mem;
}

#define PARSE_MEMBERS(F)                                                       \
    do {                                                                       \
        auto ret = std::make_unique<ast_members>();                            \
        parse_spaces();                                                        \
        for (;;) {                                                             \
            auto p = F();                                                      \
            if (!p)                                                            \
                return nullptr;                                                \
                                                                               \
            ret->m_vars.push_back(std::move(p));                               \
            char c;                                                            \
            PTRY(m_parsec, c, [](module &m) {                                  \
                m.parse_spaces_sep();                                          \
                return m.m_parsec.character('}');                              \
            }(*this));                                                         \
            if (!m_parsec.is_fail())                                           \
                break;                                                         \
            if (!parse_sep())                                                  \
                return nullptr;                                                \
        }                                                                      \
        return ret;                                                            \
    } while (0)

// $PROD }
ptr_ast_members module::parse_prods() { PARSE_MEMBERS(parse_prod); }

// $SUM }
ptr_ast_members module::parse_sums() { PARSE_MEMBERS(parse_sum); }

#define PARSEUTYPE(T, F)                                                       \
    do {                                                                       \
        auto ret = std::make_unique<T>();                                      \
        SPACEPLUS();                                                           \
        PARSEID(ret->m_id, m_parsec);                                          \
        parse_spaces();                                                        \
                                                                               \
        char c;                                                                \
        PEEK(c, m_parsec);                                                     \
        if (c == '<') {                                                        \
            ret->m_tvars = parse_tvars();                                      \
            if (!ret->m_tvars)                                                 \
                return nullptr;                                                \
            parse_spaces();                                                    \
        }                                                                      \
                                                                               \
        PTRY(m_parsec, c, m_parsec.character('{'));                            \
        if (m_parsec.is_fail()) {                                              \
            ret->m_preds = parse_preds();                                      \
            if (!ret->m_preds)                                                 \
                return nullptr;                                                \
            parse_spaces();                                                    \
            PARSECHAR('{', m_parsec);                                          \
        }                                                                      \
                                                                               \
        ret->m_members = F();                                                  \
        if (!ret->m_members)                                                   \
            return nullptr;                                                    \
        return ret;                                                            \
    } while (0)

// $STRUCT := struct $ID $TVARS? $PREDS? { $PROD }
ptr_ast_struct module::parse_struct() { PARSEUTYPE(ast_struct, parse_prods); }

// $UNION := union $ID $TVARS? $PREDS? { $SUM }
ptr_ast_union module::parse_union() { PARSEUTYPE(ast_union, parse_sums); }

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

    ret->m_arg = parse_pred();
    if (!ret->m_arg)
        return nullptr;

    parse_spaces();

    char c;
    PTRY(m_parsec, c, m_parsec.character('{'));

    if (m_parsec.is_fail()) {
        // parse require
        ret->m_req = parse_preds();

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

        if (HASKEY(ret->m_id2defun, fn->m_id->m_id)) {
            SYNTAXERR2("multiply defined method", fn->m_id->m_line,
                       fn->m_id->m_column);
            return nullptr;
        }

        ret->m_id2defun[fn->m_id->m_id] = std::move(fn);
    }
}

// $NEWLINE := \r | \n | ;
// $WHITESPACE2 := space | tab
// $WHITESPACE3 := space | tab | \r | \n | \r\n | ;
// $SEP := $WHITESPACE2* $NEWLINE+ $WHITESPACE3*
bool module::parse_sep() {
    // $WHITESPACE2*
    std::string tmp;
    PMANY(m_parsec, tmp, m_parsec.oneof(m_parser.m_wsp2));

    // skip comment
    std::string s;
    PTRY(m_parsec, s, m_parsec.str("//"));
    if (!m_parsec.is_fail())
        PMANY(m_parsec, s, m_parsec.oneof_not(m_parser.m_newline));

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

// skip spaces and comments
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

// skip spaces, comments, and ;
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

void parser::add_load_path(const char *p) { m_env.add(p); }

bool parser::add_module(const std::string &filename) {
    fs::path p(filename.c_str());
    if (!fs::exists(p)) {
        fprintf(stderr, "%s does not exist\n", filename.c_str());
        return false;
    }

    if (!fs::is_regular(fs::path(filename.c_str()))) {
        fprintf(stderr, "%s is not regular file\n", filename.c_str());
        return false;
    }

    std::ifstream ifs(filename);
    if (ifs.fail())
        return false;

    // TODO: check file size

    std::string content((std::istreambuf_iterator<char>(ifs)),
                        (std::istreambuf_iterator<char>()));

    p = fs::absolute(p);
    p = fs::canonical(p);
    std::string s = p.string();
    auto m = std::make_unique<module>(s, content, *this);

    m_modules[s] = std::move(m);

    return true;
}

bool parser::load_module_tree(module *m, module_tree *tree) {
    if (tree->m_import) {
        if (!load_module(m, tree->m_import.get()))
            return false;
    }

    for (auto &c : tree->m_children) {
        if (!load_module_tree(m, c.second.get()))
            return false;
    }

    return true;
}

bool parser::load_module(module *m, ast_import *im) {
    std::string id = im->get_id();

    // find module in current path first of all
    auto path = m->m_env.get_module_path(id);
    if (path.empty()) {
        // fallback
        path = m_env.get_module_path(id);
        if (path.empty()) {
            MODULENOTFOUND(m, im, id);
            return false;
        }
    }

    std::string str = path.string();
    im->m_full_path = str;
    if (!HASKEY(m_modules, str)) {
        add_module(str);
        if (!parse_module(str))
            return false;

        if (!load_all_module(m_modules[str].get()))
            return false;
    }

    return true;
}

bool parser::load_all_module(module *m) {
    if (m->m_is_loaded_module)
        return true;

    for (auto &p : m->m_id2import) {
        if (!load_module(m, p.second.get()))
            return false;
    }

    for (auto &p : m->m_vec_modules) {
        if (!load_module(m, p.get()))
            return false;
    }

    // scan m_modules
    if (!load_module_tree(m, &m->m_modules))
        return false;

    m->m_is_loaded_module = true;
    return true;
}

bool parser::parse_module(const std::string &str) {
    assert(HASKEY(m_modules, str));
    return m_modules[str]->parse();
}

bool parser::parse() {
    for (auto &p : m_modules) {
        if (!p.second->parse())
            return false;
        if (!load_all_module(p.second.get()))
            return false;
    }

    return true;
}

void parser::print() const {
    std::cout << "[";
    int n = 0;
    for (auto &p : m_modules) {
        if (n > 0)
            std::cout << ",";
        p.second->print();
        n++;
    }

    std::cout << "]" << std::endl;
}

void module_tree::print(size_t &n) const {
    if (m_import) {
        if (n > 0)
            std::cout << ",";

        m_import->print();
        n++;
    }

    for (auto &p : m_children) {
        p.second->print(n);
    }
}

void module::print() const {
    std::cout << "{\"path\":\"" << m_filename << "\",\"import\":[";
    size_t n = 0;
    m_modules.print(n);

    for (auto &p : m_id2import) {
        if (n > 0)
            std::cout << ",";
        p.second->print();
        n++;
    }

    std::cout << "],\"classes\":[";
    n = 1;
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

    std::cout << "],\"structs\":[";
    n = 1;
    for (auto &p : m_id2struct) {
        p.second->print();
        if (n < m_id2struct.size())
            std::cout << ",";

        n++;
    }

    std::cout << "],\"unions\":[";
    n = 1;
    for (auto &p : m_id2union) {
        p.second->print();
        if (n < m_id2union.size())
            std::cout << ",";

        n++;
    }

    std::cout << "]";

    std::cout << "}";
}

void ast_id::print() const { std::cout << "\"" << m_id << "\""; }

void ast_dotid::print() const {
    std::cout << "\"";
    int n = 0;
    for (auto &id : m_ids) {
        if (n > 0)
            std::cout << ".";
        std::cout << id->m_id;
        n++;
    }
    std::cout << "\"";
}

void ast_kfun::print() const {
    std::cout << "{\"left\":";
    m_left->print();
    std::cout << ",\"right\":";
    m_right->print();
    std::cout << "}";
}

void ast_kstar::print() const { std::cout << "\"*\""; }

void ast_tvar::print() const {
    std::cout << "{\"id\":";
    m_id->print();
    if (m_kind) {
        std::cout << ",\"kind\":";
        m_kind->print();
    }

    std::cout << "}";
}

void ast_tvars::print() const {
    std::cout << "[";
    size_t n = 1;
    for (auto &v : m_args) {
        v->print();

        if (n < m_args.size())
            std::cout << ",";

        n++;
    }
    std::cout << "]";
}

void ast_class::print() const {
    std::cout << "{\"id\":";
    m_id->print();

    std::cout << ",\"type variable\":";
    m_tvar->print();

    if (m_preds) {
        std::cout << ",\"predicates\":";
        m_preds->print();
    }

    std::cout << ",\"interfaces\":";
    m_interfaces->print();

    std::cout << "}";
}

void ast_normaltype::print() const {
    std::cout << "{\"type\": \"normal\",\"id\":";

    if (m_tvar)
        m_tvar->print();
    else
        m_id->print();

    if (m_args) {
        std::cout << ",\"type arguments\":";
        m_args->print();
    }

    std::cout << "}";
}

void ast_funtype::print() const {
    std::cout << "{\"type\": \"function\",\"arguments\":";
    m_args->print();

    std::cout << ",\"return\":";
    m_ret->print();

    std::cout << "}";
}

void ast_tupletype::print() const {
    std::cout << "{\"type\": \"tuple\",\"types\":";
    m_types->print();
    std::cout << "}";
}

void ast_vectype::print() const {
    std::cout << "{\"type\": \"vector\",\"vector type\":";
    m_vectype->print();
    std::cout << ",\"nums\":[";
    int n = 0;
    for (auto &num : m_nums) {
        if (n > 0)
            std::cout << ",";
        std::cout << num->m_num;
        n++;
    }
    std::cout << "]}";
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

void ast_types::print() const { PRINTLIST(m_types); }

void ast_pred::print() const {
    std::cout << "{\"id\":";
    m_id->print();

    std::cout << ",\"arguments\":";
    m_arg->print();

    std::cout << "}";
}

void ast_preds::print() const { PRINTLIST(m_preds); }

void ast_infix::print() const { std::cout << "\"" << m_infix << "\""; }

void ast_interface::print() const {
    std::cout << "{\"id\":";

    PRINTLIST(m_id);
    std::cout << ",\"infix\":";
    PRINTLIST(m_infix);

    if (m_args) {
        std::cout << ",\"arguments\":";
        m_args->print();
    }

    std::cout << ",\"return\":";
    m_ret->print();
    std::cout << "}";
}

void ast_interfaces::print() const { PRINTLIST(m_interfaces); }

void ast_arg::print() const {
    std::cout << "{\"id\":";
    m_id->print();
    if (m_type) {
        std::cout << ",\"type\":";
        m_type->print();
    }
    std::cout << "}";
}

void ast_args::print() const { PRINTLIST(m_args); }

void ast_defun::print() const {
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

void ast_exprs::print() const {
    std::cout << "{\"exprs\":";
    PRINTLIST(m_exprs);
    std::cout << "}";
}

#define PRINTPREFIX                                                            \
    do {                                                                       \
        if (m_prefix) {                                                        \
            std::cout << "\"prefix\":\"";                                      \
            m_prefix->print();                                                 \
            std::cout << "\",";                                                \
        }                                                                      \
    } while (0)

void ast_expr_id::print() const {
    std::cout << "{";
    PRINTPREFIX;
    std::cout << "\"id\":\"" << m_id->m_id << "\"}";
}

void ast_malloc::print() const {
    std::cout << "{\"";
    if (m_memtype == MEM_NEW)
        std::cout << "new";
    else
        std::cout << "shared";

    std::cout << "\":{\"id\":";
    m_id->print();
    std::cout << ",\"args\":";
    m_args->print();
    std::cout << "}}";
}

void ast_apply::print() const {
    std::cout << "{\"apply\":{\"func\":";
    m_func->print();
    std::cout << ",\"arguemnts\":";
    m_args->print();
    std::cout << "}}";
}

void ast_if::print() const {
    std::cout << "{";
    PRINTPREFIX;
    std::cout << "\"if\":{\"condition\":";
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

void ast_defvar::print() const {
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

void ast_defvars::print() const {
    std::cout << "{\"defvars\":";
    PRINTLIST(m_defs);
    std::cout << "}";
}

void ast_let::print() const {
    std::cout << "{";
    PRINTPREFIX;
    std::cout << "\"let\":{\"defvars\":";
    m_defvars->print();

    if (m_in) {
        std::cout << ",\"in\":";
        m_in->print();
    }

    std::cout << "}}";
}

void ast_tuple::print() const {
    std::cout << "{";
    PRINTPREFIX;
    std::cout << "\"tuple\":";
    PRINTLIST(m_exprs);
    std::cout << "}";
}

void ast_vector::print() const {
    std::cout << "{";
    PRINTPREFIX;
    std::cout << "\"vector\":";
    PRINTLIST(m_exprs);
    std::cout << "}";
}

void ast_dictelm::print() const {
    std::cout << "{\"key\":";
    m_key->print();
    std::cout << ",\"value\":";
    m_val->print();
    std::cout << "}";
}

void ast_dict::print() const {
    std::cout << "{";
    PRINTPREFIX;
    std::cout << "\"key\":";
    PRINTLIST(m_elms);
    std::cout << "}";
}

void ast_block::print() const {
    std::cout << "{";
    PRINTPREFIX;
    std::cout << "\"block\":";
    PRINTLIST(m_exprs);
    std::cout << "}";
}

void ast_index::print() const {
    std::cout << "{\"indexing\":{\"array\":";
    m_array->print();
    std::cout << ",\"index\":";
    m_index->print();
    std::cout << "}}";
}

void ast_binexpr::print() const {
    std::cout << "{\"binary expression\":{\"operator\":";
    m_op->print();
    std::cout << ",\"left\":";
    m_left->print();
    std::cout << ",\"right\":";
    m_right->print();
    std::cout << "}}";
}

void ast_num::print() const {
    std::cout << "{";
    PRINTPREFIX;
    std::cout << "\"number\":{\"type\":";

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

void ast_str::print() const {
    std::cout << "{";
    PRINTPREFIX;
    std::cout << "\"string\":\"" << m_str << "\"}";
}

void ast_parenthesis::print() const {
    std::cout << "{";
    PRINTPREFIX;
    std::cout << "\"parenthesis\":";
    m_expr->print();
    std::cout << "}";
}

void ast_instance::print() const {
    std::cout << "{\"instance\":{\"pred\":";
    m_arg->print();

    if (m_req) {
        std::cout << ",\"require\":";
        m_req->print();
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

void ast_member::print() const {
    std::cout << "{\"id\":";
    m_id->print();
    if (m_type) {
        std::cout << ",\"type\":";
        m_type->print();
    }
    std::cout << "}";
}

void ast_members::print() const { PRINTLIST(m_vars); }

void ast_import::print() const {
    std::cout << "{\"import\":{\"id\":\"" << get_id() << "\"";

    if (m_as) {
        std::cout << ",\"as\":";
        m_as->print();
    }

    std::cout << ",\"here\":";

    if (m_is_here)
        std::cout << "true";
    else
        std::cout << "false";

    std::cout << ",\"path\":\"" << m_full_path << "\"}}";
}

#define PRINTSTUN(STR)                                                         \
    do {                                                                       \
        std::cout << "{\"" STR "\":{";                                         \
        if (m_id) {                                                            \
            std::cout << "\"id\":";                                            \
            m_id->print();                                                     \
            std::cout << ",";                                                  \
        }                                                                      \
        if (m_tvars) {                                                         \
            std::cout << "\"type arguments\":";                                \
            m_tvars->print();                                                  \
            std::cout << ",";                                                  \
        }                                                                      \
        if (m_preds) {                                                         \
            std::cout << "\"type arguments\":";                                \
            m_preds->print();                                                  \
            std::cout << ",";                                                  \
        }                                                                      \
        std::cout << "\"members\":";                                           \
        m_members->print();                                                    \
        std::cout << "}}";                                                     \
    } while (0)

void ast_struct::print() const { PRINTSTUN("struct"); }

void ast_union::print() const { PRINTSTUN("union"); }

void ast_prefix::print() const { std::cout << m_prefix; }

} // namespace lunar
