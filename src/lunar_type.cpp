#include "lunar_type.hpp"

namespace lunar {

static inline shared_star mk_star() { return std::make_unique<star>(); }
static inline shared_kfun mk_kfun() { return std::make_unique<kfun>(); }

static inline shared_type mk_kind1(const std::string &id) {
    auto arr = std::make_shared<type_const>();
    arr->m_id = id;

    auto k = mk_kfun();
    k->m_left = mk_star();
    k->m_right = mk_star();

    arr->m_kind = k;

    return arr;
}

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
static inline shared_type mk_arrow() { return mk_kind2("fun"); }
static inline shared_type mk_tuple2() { return mk_kind2("tuple"); }

bool cmp_kind(const kind *lhs, const kind *rhs) {
    if (lhs->m_is_star == rhs->m_is_star)
        return true;

    auto lsub = (kfun *)lhs;
    auto rsub = (kfun *)rhs;

    return cmp_kind(lsub->m_left.get(), rsub->m_left.get()) &&
           cmp_kind(lsub->m_right.get(), rsub->m_right.get());
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
    MK_TYPE2(ret, mk_arrow, lhs, rhs);
    return ret;
}

shared_type mk_tuple(shared_type lhs, shared_type rhs) {
    shared_type ret;
    MK_TYPE2(ret, mk_tuple2, lhs, rhs);
    return ret;
}

} // namespace lunar