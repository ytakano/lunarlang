#ifndef LUNAR_TYPE_HPP
#define LUNAR_TYPE_HPP

#include <memory>
#include <string>
#include <unordered_map>
#include <vector>

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

bool cmp_kind(const kind *lhs, const kind *rhs);

// type
class type {
  public:
    type() {}
    virtual ~type(){};

    enum subtype {
        TYPE_CONST, // constant type
        TYPE_VAR,   // type variable
        TYPE_APP,   // function application
    };

    subtype m_subtype;

    virtual shared_kind get_kind() = 0;
    virtual void apply(const substitution &sub) {}
};

typedef std::shared_ptr<type> shared_type;

// constant type
// e.g.
//   num : *
//   vec : * -> *
//   fn : * -> (* -> *)
class type_const : public type {
  public:
    type_const() { m_subtype = TYPE_CONST; }
    virtual ~type_const() {}

    bool operator==(const type_const &lhs) const {
        return m_id == lhs.m_id && cmp_kind(m_kind.get(), lhs.m_kind.get());
    }

    virtual shared_kind get_kind() { return m_kind; }

    std::string m_id;
    shared_kind m_kind;
};

// type variable
class type_var : public type {
  public:
    type_var() { m_subtype = TYPE_VAR; }
    virtual ~type_var() {}

    bool operator==(const type_var &lhs) const {
        return m_id == lhs.m_id && cmp_kind(m_kind.get(), lhs.m_kind.get());
    }

    virtual shared_kind get_kind() { return m_kind; }
    virtual void apply(const substitution &sub) {}

    std::string m_id;
    shared_kind m_kind;
};

// higher order type application
class type_app : public type {
  public:
    type_app() { m_subtype = TYPE_APP; }
    virtual ~type_app() {}

    virtual shared_kind get_kind() {
        auto k = m_left->get_kind();
        assert(!k->m_is_star);

        auto kf = std::static_pointer_cast<kfun>(k);

        return kf->m_right;
    }

    // satisfy
    // m_left->get_kind()->m_left == m_right->get_kind()

    shared_type m_left;
    shared_type m_right;
};

shared_type mk_funtype(shared_type lhs, shared_type rhs);

// substitution from (id, kind) to type
// type variable -> type
// e.g.
//    (`a : *) -> `a
class substitution {
  public:
    substitution() {}
    virtual ~substitution() {}

    class subst {
      public:
        subst(shared_kind kind, shared_type type)
            : m_kind(kind), m_type(type) {}
        virtual ~subst() {}

        shared_kind m_kind;
        shared_type m_type;
    };

    typedef std::unique_ptr<subst> ptr_subst;

    bool add_subst(const std::string &id, shared_kind k, shared_type t) {
        auto s = std::make_unique<subst>(k, t);
        m_subst[id] = std::move(s);
        return true;
    }

    // substitute type variables in the argument
    // Applying a substituton of
    // (`a : *) -> int
    // to a type
    // (`a : * -> *)
    // fails because the kinds are different.
    // nullptr is returned when substitution was failed.
    shared_type apply(shared_type type);

    std::unordered_map<std::string, ptr_subst> m_subst;
};

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
//     class ord<`a> where eq<`a>
//   qualified class instance:
//     inst ord<either `a> where ord<'a>
//   qualifid type declaration:
//     fn myfun (x : `a, y : `b) -> `a where num<`a>, bool<`b>
class qual {
  public:
    qual() {}
    virtual ~qual() {}

    std::vector<shared_pred> m_preds;
};

// class decralation
class typeclass : public qual {
  public:
    typeclass() {}
    virtual ~typeclass() {}

    std::string m_id;                 // class name
    std::vector<shared_type> m_args;  // arguments
    std::vector<shared_type> m_funcs; // interface functions

    bool apply(std::vector<shared_type> &args);
};

typedef std::shared_ptr<typeclass> shared_typeclass;

// class instance declaration
class inst : public qual {
  public:
    inst() {}
    virtual ~inst() {}

    std::string m_id;                 // class name
    std::vector<shared_type> m_args;  // arguments
    std::vector<shared_type> m_funcs; // interface functions
};

typedef std::shared_ptr<inst> shared_inst;

// qualified type
class qtype : public qual {
  public:
    qtype() {}
    virtual ~qtype() {}

    shared_type m_type;
};

class classenv {
  public:
    classenv() {}
    virtual ~classenv() {}

  private:
    // satisfy
    // ∀x x ∈ {class names of m_instances} -> x ∈ {class names of m_classes}

    // class name -> class declarations
    std::unordered_map<std::string, shared_typeclass> m_classes;
    // class name -> instance declarations
    std::unordered_map<std::string, std::vector<shared_inst>> m_instances;
};

} // namespace lunar

#endif // LUNAR_TYPE_HPP