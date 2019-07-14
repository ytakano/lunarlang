#ifndef LUNAR_TYPE_HPP
#define LUNAR_TYPE_HPP

#include "lunar_common.hpp"
#include "lunar_parser.hpp"

#include <assert.h>

#include <map>
#include <memory>
#include <string>
#include <unordered_map>
#include <vector>

#include <boost/bimap/bimap.hpp>
#include <boost/bimap/unordered_set_of.hpp>

namespace lunar {

struct type_id {
    std::string m_path;
    std::string m_id;

    void print() {
        std::cout << "{\"path\":\"" << m_path << "\",\"id\":\"" << m_id
                  << "\"}";
    }

    bool operator==(const type_id &rhs) const {
        return m_path == rhs.m_path && m_id == rhs.m_id;
    }

    bool operator!=(const type_id &rhs) const { return !(*this == rhs); }
};

} // namespace lunar

namespace std {

template <> struct std::hash<lunar::type_id> {
  public:
    size_t operator()(const lunar::type_id &data) const {
        std::size_t seed = 0;
        boost::hash_combine(seed, data.m_path);
        boost::hash_combine(seed, data.m_id);
        return seed;
    }
};

} // namespace std

namespace lunar {

struct ast;
struct ast_type;
struct ast_class;
struct ast_instance;
struct ast_pred;
class module;
class parser;
class pred;

class substitution;

// a kind is the type of a type constructor or a higher order type operator
// e.g.
//   *
//   * -> *
//   * -> (* -> *)
class kind {
  public:
    kind() {}
    virtual ~kind(){};

    virtual void print() = 0;
    virtual std::string to_str() = 0;

    bool m_is_star;
};

typedef std::shared_ptr<kind> shared_kind;

// normal type
class star : public kind {
  public:
    star() { m_is_star = true; }
    virtual ~star() {}
    virtual void print() { std::cout << "*"; }
    virtual std::string to_str() { return "*"; }
};

typedef std::shared_ptr<star> shared_star;

// higher order type
class kfun : public kind {
  public:
    kfun() { m_is_star = false; }
    virtual ~kfun() {}

    virtual void print() {
        m_left->print();
        std::cout << " -> ";
        m_right->print();
    }

    virtual std::string to_str() {
        auto left = m_left->to_str();
        auto right = m_right->to_str();
        return left + " -> " + right;
    }

    shared_kind m_left;
    shared_kind m_right;
};

typedef std::shared_ptr<kfun> shared_kfun;

int cmp_kind(const kind *lhs, const kind *rhs);

// type
class type {
  public:
    type() {}
    virtual ~type(){};

    virtual void print() = 0;

    enum subtype {
        TYPE_CONST, // constant type
        TYPE_VAR,   // type variable
        TYPE_APP,   // higher order type application
    };

    subtype m_subtype;

    static std::shared_ptr<type> make(const module *ptr_mod,
                                      const ast_type *ptr);

    virtual shared_kind get_kind() = 0;
};

typedef std::shared_ptr<type> shared_type;

bool eq_type(type *lhs, type *rhs);

// constant type
// e.g.
//   num : *
//   vec : * -> *
class type_const : public type {
  public:
    virtual ~type_const() {}
    virtual void print();

    enum CTYPE {
        CTYPE_PRIMTIVE, // primitive type
        CTYPE_STRUCT,   // struct type
        CTYPE_UNION,    // union type
        CTYPE_FUNC,     // function type
    };

    bool operator==(const type_const &lhs) const {
        return m_ctype == lhs.m_ctype && m_id == lhs.m_id &&
               cmp_kind(m_kind.get(), lhs.m_kind.get()) == 0;
    }

    bool operator!=(const type_const &lhs) const { return !(*this == lhs); }

    virtual shared_kind get_kind() { return m_kind; }
    const type_id &get_id() { return m_id; }

    // id: type name
    // numtargs: the number of type arguments
    static shared_type make(const type_id &id, unsigned int numtargs,
                            CTYPE ctype = CTYPE_PRIMTIVE);

  protected:
    type_const(CTYPE ctype) : m_ctype(ctype) { m_subtype = TYPE_CONST; }
    type_id m_id; // identifier of type (e.g. num, bool)
    shared_kind m_kind;
    CTYPE m_ctype;
};

typedef std::shared_ptr<type_const> shared_type_const;

// type variable
class type_var : public type {
  public:
    type_var(const std::string &id, shared_kind k) : m_id(id), m_kind(k) {
        m_subtype = TYPE_VAR;
    }
    virtual ~type_var() {}

    bool operator==(const type_var &lhs) const {
        return m_id == lhs.m_id &&
               cmp_kind(m_kind.get(), lhs.m_kind.get()) == 0;
    }

    bool operator!=(const type_var &lhs) const { return !(*this == lhs); }

    bool operator<(const type_var &lhs) const {
        int ret = m_id.compare(lhs.m_id);
        if (ret == 0) {
            return cmp_kind(m_kind.get(), lhs.m_kind.get());
        } else {
            return ret;
        }
    }

    virtual shared_kind get_kind() { return m_kind; }
    const std::string &get_id() const { return m_id; }
    void set_id(const std::string &s) { m_id = s; }

    // id: type variable name
    static shared_type make(const std::string &id, unsigned int numtargs);

    // id: type variable name, k: kind
    static shared_type make(const std::string &id, shared_kind k) {
        auto ret = std::shared_ptr<type_var>(new type_var);
        ret->m_id = id;
        ret->m_kind = k;
        return ret;
    }

    virtual void print();

  private:
    type_var() { m_subtype = TYPE_VAR; }
    std::string m_id; // type variable name
    shared_kind m_kind;
};

typedef std::shared_ptr<type_var> shared_type_var;

// application of higher order type
// e.g. (1)
//   struct dict<T1, T2> {...} is denoted as * -> (* -> *)
//   dict<int> is denoted as
//     tapp1.left  = * -> (* -> *)
//     tapp1.right = *
//   dict<int, bool> is denoted as
//     tapp2.left  = tapp1
//     type2.right = *
//
// e.g. (2)
//   struct somedata<T1, T2, T3> {...}
//   somedata<int>
//     tapp1.left  = * -> (* -> (* -> *))
//     tapp1.right = *
//   somedata<int, int>
//     tapp2.left  = tapp1
//     tapp2.right = *
//   somedata<int, int, int>
//     tapp3.left  = tapp2
//     tapp3.right = *
class type_app : public type {
  public:
    virtual ~type_app() {}

    bool operator==(const type_app &lhs) const {
        return eq_type(m_left.get(), m_right.get()) &&
               eq_type(lhs.m_left.get(), lhs.m_right.get());
    }

    // e.g.
    // if
    //   left:  * -> (* -> *)
    //   right: *
    // then output
    //   * -> *
    virtual shared_kind get_kind() {
        auto k = m_left->get_kind();
        assert(!k->m_is_star);

        auto kf = std::static_pointer_cast<kfun>(k);

        return kf->m_right;
    }
    static shared_type make(shared_type lhs, shared_type rhs);

    // require
    //   m_left->get_kind()->m_left == m_right->get_kind()
    //   m_right->get_kind() == *
    shared_type m_left;
    shared_type m_right;

    virtual void print();

  private:
    type_app() { m_subtype = TYPE_APP; }
};

// substitution from (id, kind) to type
// type variable -> type
// e.g.
//   substitution:
//     {(`a : *) -> int}
//   apply:
//     {(`a : *) -> int} `a : * -> int
//   composition:
//     {`b : * -> int} {(`a : *) -> vec (`b : *)} -> {(`a : *) -> vec int}
class substitution {
  public:
    substitution() {}
    virtual ~substitution() {}

    // substitute type variables in the argument
    shared_type apply(shared_type type);
    std::unique_ptr<pred> apply(pred *p);

    std::map<type_var, shared_type> m_subst;
};

typedef std::shared_ptr<substitution> shared_subst;

// a predicate asserts that m_types are members of a class named m_id
// e.g.
//   num<`a>: class name = num, m_args = [`a]
//   ClassA<`a, `b>: class name = ClassA, m_args = [`a, `b]
class pred {
  public:
    pred() {}
    virtual ~pred() {}

    void print();

    static std::shared_ptr<pred> make(const module *ptr_mod,
                                      const ast_pred *ptr);

    bool operator==(const pred &rhs) const {
        if (m_id != rhs.m_id || m_args.size() != rhs.m_args.size())
            return false;

        for (int i = 0; i < m_args.size(); i++) {
            if (!eq_type(m_args[i].get(), rhs.m_args[i].get()))
                return false;
        }

        return true;
    }

    bool in_hnf() {
        for (auto &arg : m_args) {
            if (!hnf(arg.get()))
                return false;
        }
        return true;
    }

    type_id m_id; // class name_id
    std::vector<shared_type> m_args;

  private:
    static bool hnf(type *p) {
        switch (p->m_subtype) {
        case type::TYPE_VAR:
            return true;
        case type::TYPE_APP: {
            auto ap = (type_app *)p;
            return hnf(ap->m_left.get());
        }
        case type::TYPE_CONST:
            return false;
        }
    }
};

typedef std::shared_ptr<pred> shared_pred;

typedef boost::bimaps::bimap<boost::bimaps::unordered_set_of<std::string>,
                             boost::bimaps::unordered_set_of<std::string>>
    bimap_ss;

// qualified type
// e.g.
//   qualified class declaration:
//     class ord<`a> require eq<`a>
//   qualified class instance:
//     inst ord<either `a> require ord<'a>
//   qualified type declaration:
//     fn myfun (x : `a, y : `b) : `a require num<`a>, bool<`b>
//     struct data<`a, `b> require num<`a>, bool<`b>
class qual {
  public:
    qual() {}
    virtual ~qual() {}

    void print_preds();

    std::vector<shared_pred> m_preds; // require

    // de Bruijn index <-> original type variable
    bimap_ss m_idx_tvar;

    const ast *m_ast;
    const module *m_module;
};

// class declaration
class typeclass : public qual {
  public:
    enum ASYCLIC {
        ASYCLIC_NONE,
        ASYCLIC_YES,
        ASYCLIC_NO,
    };
    typeclass() : m_is_asyclic(ASYCLIC_NONE) {}
    virtual ~typeclass() {}

    void print();

    type_id m_id; // class name

    // satisfy
    // x ∈ tv(m_args)
    // y, z ∈ tv(m_funcs)
    // ∀x ∀y x->m_id = y->m_id -> x->m_kind = y->m_kind
    // ∀y ∀z y->m_id = z->m_id -> y->m_kind = z->m_kind
    std::vector<shared_type> m_tvar_constraint; // type variable constraint
    std::vector<std::string> m_args;            // arguments
    std::unordered_map<type_id, shared_type> m_funcs; // interfaces

    ASYCLIC m_is_asyclic;

    bool apply_super(std::vector<shared_type> &args,
                     std::vector<std::unique_ptr<pred>> &ret);

    bool check_kind_constraint(const std::string &id, kind *k);
    bool add_constraints(pred *p);
    bool add_constraints(type *p);
};

typedef std::shared_ptr<typeclass> shared_typeclass;

// class instance declaration
class inst : public qual {
  public:
    inst() {}
    virtual ~inst() {}

    void print();

    pred m_pred;
    std::unordered_map<type_id, shared_type> m_funcs; // interfaces
};

typedef std::shared_ptr<inst> shared_inst;

// qualified type
// function, struct, or union types are denoted by this class
class qual_type : public qual {
  public:
    qual_type() {}
    virtual ~qual_type() {}

    shared_type m_type;
    std::vector<shared_type_var> m_args; // type arguments
};

class classenv {
  public:
    classenv() : m_de_bruijn_idx(0) {}
    virtual ~classenv() {}

    static std::unique_ptr<classenv> make(const parser &ps);

    void print();

  private:
    struct env {
        shared_typeclass m_class; // class declaration

        // multiply defined instances are prohibited
        // 0 <= i, j < m_insts.size()
        // s: substitution (mgu)
        // ∀i ∀j ∃s (s m_insts[i].m_args = s m_insts[j].m_args) -> error
        std::vector<shared_inst> m_insts; // instance declarations
    };

    typedef std::unique_ptr<env> ptr_env;

    // class name -> (class declarations, class instance declarations)
    std::unordered_map<type_id, ptr_env> m_env;

    // function name -> class name
    std::unordered_map<type_id, std::string> m_func2class;

    uint64_t m_de_bruijn_idx;

    bool add_class(const module *ptr_mod, const ast_class *ptr);
    bool add_instance(const module *ptr_mod, const ast_instance *ptr_ast);
    bool add_instance(std::vector<std::unique_ptr<pred>> &ps,
                      const module *ptr_mod, const ast_instance *ptr_ast);
    inst *overlap(pred &ptr);
    bool is_asyclic();
    bool is_asyclic(const module *ptr_mod, typeclass *ptr,
                    std::unordered_set<type_id> &visited);
    void de_bruijn(typeclass *ptr);
    void de_bruijn(inst *ptr);
    void de_bruijn(qual *ptr, type *ptr_type);
    std::string gensym();

    bool by_super(pred *pd, std::vector<std::unique_ptr<pred>> &ret);
    void by_inst(pred *pd, std::vector<std::unique_ptr<pred>> &ret);
    TRIVAL entail(std::vector<std::unique_ptr<pred>> &ps, pred *pd);
    bool to_hnfs(std::vector<std::unique_ptr<pred>> &ps,
                 std::vector<std::unique_ptr<pred>> &ret);
    bool to_hnf(std::unique_ptr<pred> pd,
                std::vector<std::unique_ptr<pred>> &ret);
    bool simplify(std::vector<std::unique_ptr<pred>> &ps);
    bool reduce(std::vector<std::unique_ptr<pred>> &ps,
                std::vector<std::unique_ptr<pred>> &ret);
};

} // namespace lunar

#endif // LUNAR_TYPE_HPP