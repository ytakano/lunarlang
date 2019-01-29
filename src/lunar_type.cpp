#include "lunar_type.hpp"

#include <assert.h>

namespace lunar {

static inline shared_star mk_star() { return std::make_unique<star>(); }
static inline shared_kfun mk_kfun() { return std::make_unique<kfun>(); }

// * -> *
static inline shared_type mk_kind1(const std::string &id) {
    auto arr = std::make_shared<type_const>();
    arr->m_id = id;

    auto k = mk_kfun();
    k->m_left = mk_star();
    k->m_right = mk_star();

    arr->m_kind = k;

    return arr;
}

// * -> (* -> *)
static inline shared_type mk_kind2(const std::string &id) {
    auto arr = std::make_shared<type_const>();
    arr->m_id = id;

    auto k = mk_kfun();
    k->m_left = mk_star();

    auto right = mk_kfun();
    right->m_left = mk_star();
    right->m_right = mk_star();

    arr->m_kind = k;

    return arr;
}

static inline shared_type mk_vec() { return mk_kind1("vec"); }
static inline shared_type mk_fn() { return mk_kind2("fn"); }
static inline shared_type mk_tuple2() { return mk_kind2("tuple"); }

// if * , * then 0
// if * , (* -> *) then -1
// if * -> * , * then 1
int cmp_kind(const kind *lhs, const kind *rhs) {
    if (lhs->m_is_star == true) {
        if (rhs->m_is_star == true)
            return 0;
    } else {
        if (rhs->m_is_star == false) {
            auto lsub = (kfun *)lhs;
            auto rsub = (kfun *)rhs;

            int ret = cmp_kind(lsub->m_left.get(), rsub->m_left.get());
            if (ret != 0)
                return ret;

            return cmp_kind(lsub->m_right.get(), rsub->m_right.get());
        }
    }

    // lhs->m_is_star xor rhs->m_is_star
    if (lhs->m_is_star)
        return -1;
    else
        return 1;
}

#define MK_TYPE2(RET, FUN, LHS, RHS)                                           \
    do {                                                                       \
        auto app = std::make_shared<type_app>();                               \
        auto arr = std::make_shared<type_app>();                               \
                                                                               \
        arr->m_left = FUN();                                                   \
        arr->m_right = LHS;                                                    \
                                                                               \
        app->m_left = arr;                                                     \
        app->m_right = RHS;                                                    \
                                                                               \
        RET = app;                                                             \
    } while (0);

shared_type mk_funtype(shared_type lhs, shared_type rhs) {
    shared_type ret;
    MK_TYPE2(ret, mk_fn, lhs, rhs);
    return ret;
}

shared_type mk_tuple(shared_type lhs, shared_type rhs) {
    shared_type ret;
    MK_TYPE2(ret, mk_tuple2, lhs, rhs);
    return ret;
}

bool eq_type(type *lhs, type *rhs) {
    if (lhs->m_subtype != rhs->m_subtype)
        return false;

    switch (lhs->m_subtype) {
    case type::TYPE_APP: {
        auto lapp = (type_app *)lhs;
        auto rapp = (type_app *)rhs;
        return *lapp == *rapp;
    }
    case type::TYPE_CONST: {
        auto lc = (type_const *)lhs;
        auto rc = (type_const *)rhs;
        return *lc == *rc;
    }
    case type::TYPE_VAR: {
        auto lv = (type_var *)lhs;
        auto rv = (type_var *)rhs;
        return *lv == *rv;
    }
    }
}

shared_type substitution::apply(shared_type type) {
    switch (type->m_subtype) {
    case type::TYPE_CONST:
        return type;
    case type::TYPE_VAR: {
        auto tvar = std::static_pointer_cast<type_var>(type);
        auto s = m_subst.find(*tvar);
        if (s == m_subst.end())
            return type;

        return s->second;
    }
    case type::TYPE_APP:
        auto tapp = std::static_pointer_cast<type_app>(type);

        auto lhs = apply(tapp->m_left);
        tapp->m_left = lhs;

        auto rhs = apply(tapp->m_right);
        tapp->m_right = rhs;

        return tapp;
    }

    return nullptr; // never reach here
}

bool typeclass::apply(std::vector<shared_type> &args) {
    if (args.size() != m_args.size()) {
        // TODO: print error
        return false;
    }

    // check kind
    for (int i = 0; i < args.size(); i++) {
        if (cmp_kind(m_args[i]->get_kind().get(), args[i]->get_kind().get()) ==
            0) {
            // TODO: print error
            return false;
        }
    }

    // check predicates

    // check functions

    return true;
}

// s1 = {x1 -> t1, ..., xn -> tn}
// s2 = {y1 -> u1, ..., ym -> um}
// 1.
//   s3 = {y1 -> s1 u1, ..., ym -> s1 um}
// 2.
//   s4 = {s | s ∈ s1 ∧ ¬(dom(s) ∈ dom(s2))}
// 3.
//   s3 ∪ s4
static shared_subst compose(substitution &s1, substitution &s2) {
    auto ret = std::make_shared<substitution>();
    for (auto &y : s2.m_subst) {
        auto t = s1.apply(y.second);
        if (t->m_subtype == type::TYPE_VAR) {
            auto tvar = (type_var *)t.get();
            if (*tvar != y.first)
                ret->m_subst[y.first] = t;
        }
    }

    for (auto &x : s1.m_subst) {
        if (!HASKEY(s2.m_subst, x.first))
            ret->m_subst[x.first] = x.second;
    }

    return ret;
}

// if tvar ∈ tv(ty)
// then true
// else false
static bool occurs_check(type_var &tvar, type *ty) {
    switch (ty->m_subtype) {
    case type::TYPE_VAR: {
        auto tv2 = (type_var *)ty;
        if (tvar == *tv2)
            return true;
        break;
    }
    case type::TYPE_CONST:
        return false;
    case type::TYPE_APP: {
        auto tapp = (type_app *)ty;
        return occurs_check(tvar, tapp->m_left.get()) ||
               occurs_check(tvar, tapp->m_right.get());
    }
    }

    return false;
}

static shared_subst var_bind(shared_type var, shared_type ty) {
    auto tv1 = std::static_pointer_cast<type_var>(var);
    if (ty->m_subtype == type::TYPE_VAR) {
        auto tv2 = std::static_pointer_cast<type_var>(ty);
        if (*tv1 == *tv2)
            return std::make_shared<substitution>();
    }

    if (occurs_check(*tv1, ty.get())) {
        // TODO: print error
        return nullptr;
    }

    if (var->get_kind() != ty->get_kind()) {
        // TODO: print error
        return nullptr;
    }

    auto s = std::make_shared<substitution>();
    s->m_subst[*tv1] = ty;
    return s;
}

// L, R ∈ {types}
// s ∈ {subsutitutions}
// if ∃s (s L) = (s R)
// then s (s is the most general unifier)
// else nullptr
shared_subst mgu(shared_type lhs, shared_type rhs) {
    if (lhs->m_subtype == type::TYPE_APP && rhs->m_subtype == type::TYPE_APP) {
        auto l = std::static_pointer_cast<type_app>(lhs);
        auto r = std::static_pointer_cast<type_app>(rhs);

        auto s1 = mgu(l->m_left, r->m_left);
        if (!s1)
            return nullptr;

        auto s2 = mgu(s1->apply(l->m_right), s1->apply(r->m_right));
        if (!s2)
            return nullptr;

        return compose(*s2, *s1);
    } else if (lhs->m_subtype == type::TYPE_VAR) {
        return var_bind(lhs, rhs);
    } else if (rhs->m_subtype == type::TYPE_VAR) {
        return var_bind(rhs, lhs);
    } else if (lhs->m_subtype == type::TYPE_CONST &&
               rhs->m_subtype == type::TYPE_CONST) {
        return std::make_shared<substitution>();
    }

    return nullptr;
}

// s1, s2: substitution
// x ∈ dom(s1)
// y ∈ dom(s2)
// if ∀x ∀y (x = y) -> (s1 x = s2 x)
// then s1 ∪ s2
// else nullptr
static shared_subst merge(substitution &s1, substitution &s2) {
    auto ret = std::make_shared<substitution>();
    for (auto &it1 : s1.m_subst) {
        auto it2 = s2.m_subst.find(it1.first);
        if (it2 != s2.m_subst.end()) {
            if (!eq_type(it1.second.get(), it2->second.get()))
                return nullptr;
        }
        ret->m_subst[it1.first] = it1.second;
    }

    for (auto s : s2.m_subst) {
        ret->m_subst[s.first] = s.second;
    }

    return ret;
}

// L, R ∈ {types}
// s ∈ {subsutitutions}
// if ∃s (s L) = R
// then s
// else nullptr
shared_subst match(shared_type lhs, shared_type rhs) { return nullptr; }

} // namespace lunar