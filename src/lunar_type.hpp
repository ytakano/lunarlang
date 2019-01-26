#ifndef LUNAR_TYPE_HPP
#define LUNAR_TYPE_HPP

#include <memory>
#include <string>
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
//   "num", *
//   "vec", * -> *
//   "(->)", * -> (* -> *)
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

// function application
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

    shared_type m_left;
    shared_type m_right;
};

shared_type mk_funtype(shared_type lhs, shared_type rhs);

// substitution from (id, kind) to type
// type variable -> type
// e.g.
//    `a, * -> `a
class substitution {
  public:
    substitution() {}
    virtual ~substitution() {}

    class subst {
      public:
        subst(const std::string &id, shared_kind kind, shared_type type)
            : m_id(id), m_kind(kind), m_type(type) {}
        virtual ~subst() {}

        std::string m_id;
        shared_kind m_kind;
        shared_type m_type;
    };

    typedef std::unique_ptr<subst> ptr_subst;

    void add_subst(const std::string &id, shared_kind k, shared_type t) {
        auto s = std::make_unique<subst>(id, k, t);
        m_subst.push_back(std::move(s));
    }

    std::vector<ptr_subst> m_subst;
};

// predicate that m_types are members of the class named m_class
// e.g.
//   "num", `a
//   "ClassA", `a, `b
class pred {
  public:
    pred() {}
    virtual ~pred() {}

    std::string m_class;
    std::vector<shared_type> m_types;
};

typedef std::shared_ptr<pred> shared_pred;

// qualified type
// e.g.
//   qualifid type:
//     "num", `a => `a -> "int"
//   qualified class instance:
//     "eq", `a => "num", `a
class qual {
  public:
    qual() {}
    virtual ~qual() {}

    std::vector<shared_pred> m_preds;
};

// class instance declaration
class inst : public qual {
  public:
    inst() {}
    virtual ~inst() {}

    shared_pred m_pred;
};

typedef std::shared_ptr<inst> shared_inst;

// qualified type
class qtype : public qual {
  public:
    qtype() {}
    virtual ~qtype() {}

    shared_type m_type;
};

// class decralation
// e.g.
//   "eq", `a => class `a, `b
class typeclass {
  public:
    typeclass() {}
    virtual ~typeclass() {}

    std::vector<shared_pred> m_preds;
    std::vector<shared_inst> m_insts;
};

} // namespace lunar

#endif // LUNAR_TYPE_HPP