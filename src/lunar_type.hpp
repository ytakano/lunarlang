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

class star : public kind {
  public:
    star() { m_is_star = true; }
    virtual ~star() {}
};

class kind_sub : public kind {
  public:
    kind_sub() { m_is_star = false; }
    virtual ~kind_sub() {}

    std::unique_ptr<kind> m_left;
    std::unique_ptr<kind> m_right;
};

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

// primitive datatypes
class type_const : public type {
  public:
    type_const() { m_subtype = TYPE_CONST; }
    virtual ~type_const() {}

    bool operator==(const type_const &lhs) const {
        return m_id == lhs.m_id && cmp_kind(m_kind.get(), lhs.m_kind.get());
    }

    std::string m_id;
    std::unique_ptr<kind> m_kind;
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
    std::unique_ptr<kind> m_kind;
};

// function application
class type_app : public type {
  public:
    type_app() { m_subtype = TYPE_APP; }
    virtual ~type_app() {}

    std::unique_ptr<type> m_left;
    std::unique_ptr<type> m_right;
};

} // namespace lunar

#endif // LUNAR_TYPE_HPP