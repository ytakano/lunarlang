#ifndef LUNAR_TYPE_HPP
#define LUNAR_TYPE_HPP

#include <memory>
#include <string>

namespace lunar {

// kind is type of type
class kind {
  public:
    kind() {}
    virtual ~kind() = 0;

    bool m_is_star;
};

typedef std::shared_ptr<kind> ptr_kind;

class star : public kind {
  public:
    star() { m_is_star = true; }
    virtual ~star() {}
};

typedef std::shared_ptr<star> ptr_star;

class kfun : public kind {
  public:
    kfun() { m_is_star = false; }
    virtual ~kfun() {}

    ptr_kind m_left;
    ptr_kind m_right;
};

typedef std::shared_ptr<kfun> ptr_kfun;

bool cmp_kind(const kind *lhs, const kind *rhs);

// type
class type {
  public:
    type() {}
    virtual ~type() = 0;

    enum subtype {
        TYPE_CONST,
        TYPE_VAR,
        TYPE_APP,
    };

    subtype m_subtype;
};

typedef std::shared_ptr<type> ptr_type;

// primitive datatypes
class type_const : public type {
  public:
    type_const() { m_subtype = TYPE_CONST; }
    virtual ~type_const() {}

    bool operator==(const type_const &lhs) const {
        return m_id == lhs.m_id && cmp_kind(m_kind.get(), lhs.m_kind.get());
    }

    std::string m_id;
    ptr_kind m_kind;
};

// type variable
class type_var : public type {
  public:
    type_var() { m_subtype = TYPE_VAR; }
    virtual ~type_var() {}

    bool operator==(const type_var &lhs) const {
        return m_id == lhs.m_id && cmp_kind(m_kind.get(), lhs.m_kind.get());
    }

    std::string m_id;
    ptr_kind m_kind;
};

// function application
class type_app : public type {
  public:
    type_app() { m_subtype = TYPE_APP; }
    virtual ~type_app() {}

    ptr_type m_left;
    ptr_type m_right;
};

ptr_type mk_funtype(ptr_type lhs, ptr_type rhs);

} // namespace lunar

#endif // LUNAR_TYPE_HPP