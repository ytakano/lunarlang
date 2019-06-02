#ifndef LUNAR_TYPE_HPP
#define LUNAR_TYPE_HPP

#include "lunar_common.hpp"

#include <assert.h>

#include <map>
#include <memory>
#include <string>
#include <unordered_map>
#include <vector>

#include <boost/multi_index/hashed_index.hpp>
#include <boost/multi_index/identity.hpp>
#include <boost/multi_index/member.hpp>
#include <boost/multi_index/sequenced_index.hpp>
#include <boost/multi_index_container.hpp>

namespace mi = boost::multi_index;

namespace lunar {

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

    bool m_is_star;
};

typedef std::shared_ptr<kind> shared_kind;

// normal type
class star : public kind {
  public:
    star() { m_is_star = true; }
    virtual ~star() {}
};

typedef std::shared_ptr<star> shared_star;

// higher order type
class kfun : public kind {
  public:
    kfun() { m_is_star = false; }
    virtual ~kfun() {}

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

    enum subtype {
        TYPE_CONST, // constant type
        TYPE_VAR,   // type variable
        TYPE_APP,   // higher order type application
    };

    subtype m_subtype;

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

    enum CTYPEFOR {
        CTYPE_PRIMTIVE, // primitive type
        CTYPE_SET,      // struct or union type
        CTYPE_FUN,      // function type
    };

    bool operator==(const type_const &lhs) const {
        return m_id == lhs.m_id &&
               cmp_kind(m_kind.get(), lhs.m_kind.get()) == 0;
    }

    virtual shared_kind get_kind() { return m_kind; }
    const std::string &get_id() { return m_id; }

    // id: type name
    // numtargs: the number of type arguments
    static shared_type make(const std::string &id, unsigned int numtargs);

  protected:
    type_const(CTYPEFOR ctype) : m_ctypefor(ctype) { m_subtype = TYPE_CONST; }
    static shared_kind make_kind(unsigned int numtargs);
    std::string m_id; // identifier of type (e.g. num, bool)
    shared_kind m_kind;
    CTYPEFOR m_ctypefor;
};

typedef std::shared_ptr<type_const> shared_type_const;

// type variable
class type_var : public type {
  public:
    virtual ~type_var() {}

    bool operator==(const type_var &lhs) const {
        return m_id == lhs.m_id &&
               cmp_kind(m_kind.get(), lhs.m_kind.get()) == 0;
    }

    bool operator!=(const type_var &lhs) const { return !(*this == lhs); }

    bool operator<(const type_var &lhs) const {
        int ret = m_id.compare(lhs.m_id);
        if (ret == 0) {
            return cmp_kind(m_kind.get(), lhs.m_kind.get()) == -1;
        } else {
            return ret == -1;
        }
    }

    virtual shared_kind get_kind() { return m_kind; }
    const std::string &get_id() { return m_id; }

    // id: type variable name
    static shared_type make(const std::string &id);

  private:
    type_var() { m_subtype = TYPE_VAR; }
    std::string m_id;   // type variable name
    shared_kind m_kind; // must be *
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

  private:
    type_app() { m_subtype = TYPE_APP; }
};

// class for struct or union type
class type_set : public type_const {
  public:
    virtual ~type_set() {}

    enum SETFOR {
        TYPE_STRUCT,
        TYPE_UNION,
    };

    // id: type name
    // numtargs: the number of type arguments
    static shared_type make(SETFOR setfor, const std::string &id,
                            unsigned int numtargs);

    void add_member(const std::string &id, shared_type t) {
        m_id2member.insert(id2type(id, t));
    }

    bool operator==(const type_set &lhs) const;

  private:
    struct id2type {
        id2type(const std::string &id, shared_type t) : m_id(id), m_type(t) {}
        std::string m_id;
        shared_type m_type;
    };

    struct id {};

    mi::multi_index_container<
        id2type,
        mi::indexed_by<
            mi::hashed_unique<mi::tag<id>,
                              mi::member<id2type, std::string, &id2type::m_id>>,
            mi::sequenced<>>>
        m_id2member;

    SETFOR m_setfor; // for struct or union

    type_set(SETFOR setfor) : m_setfor(setfor), type_const(CTYPE_SET) {}
};

typedef std::shared_ptr<type_set> shared_type_struct;
typedef std::shared_ptr<type_set> shared_type_union;

// class for function type
class type_fun : public type_const {};

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

    std::map<type_var, shared_type> m_subst;
};

typedef std::shared_ptr<substitution> shared_subst;

// predicate that m_types are members of the class named m_id
// e.g.
//   num<`a>
//   ClassA<`a, `b>
class pred {
  public:
    pred() {}
    virtual ~pred() {}

    std::string m_id; // class name
    std::vector<shared_type> m_types;
};

typedef std::shared_ptr<pred> shared_pred;

// qualified type
// e.g.
//   qualified class declaration:
//     class ord<`a> implies eq<`a>
//   qualified class instance:
//     inst ord<either `a> implies ord<'a>
//   qualified type declaration:
//     fn myfun (x : `a, y : `b) : `a implies num<`a>, bool<`b>
class qual {
  public:
    qual() {}
    virtual ~qual() {}

    std::vector<shared_pred> m_preds;
};

// class declaration
class typeclass : public qual {
  public:
    typeclass() {}
    virtual ~typeclass() {}

    std::string m_id; // class name

    // satisfy
    // x ∈ tv(m_args)
    // y, z ∈ tv(m_funcs)
    // ∀x ∀y x->m_id = y->m_id -> x->m_kind = y->m_kind
    // ∀y ∀z y->m_id = z->m_id -> y->m_kind = z->m_kind
    std::vector<shared_type_var> m_args;                  // arguments
    std::unordered_map<std::string, shared_type> m_funcs; // interfaces

    bool apply(std::vector<shared_type> &args);
};

typedef std::shared_ptr<typeclass> shared_typeclass;

// class instance declaration
class inst : public qual {
  public:
    inst() {}
    virtual ~inst() {}

    std::string m_id;                                     // name
    std::vector<shared_type> m_args;                      // arguments
    std::unordered_map<std::string, shared_type> m_funcs; // interfaces
};

typedef std::shared_ptr<inst> shared_inst;

// qualified type
// function, struct, or union types must be denoted as this class
class qual_type : public qual {
  public:
    qual_type() {}
    virtual ~qual_type() {}

    shared_type_const m_type;
    std::vector<shared_type_var> m_args; // type arguments
};

class classenv {
  public:
    classenv() {}
    virtual ~classenv() {}

  private:
    struct env {
        shared_typeclass m_class; // class declaration

        // multiply defined instances are prohibited
        // 0 <= i, j < m_insts.size()
        // s: substitution (mgu)
        // ∀i ∀j ∃s (s m_insts[i].m_args = s m_insts[j].m_args) -> error
        std::vector<shared_inst> m_insts; // instance declarations
    };

    // class name -> (class declarations, class instance declarations)
    std::unordered_map<std::string, env> m_env;

    // function name -> class name
    std::unordered_map<std::string, std::string> m_func2class;
};

} // namespace lunar

#endif // LUNAR_TYPE_HPP
