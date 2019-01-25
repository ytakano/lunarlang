#include "lunar_type.hpp"

namespace lunar {

static inline ptr_star mk_star() { return std::make_unique<star>(); }
static inline ptr_kfun mk_kfun() { return std::make_unique<kfun>(); }

static inline ptr_type mk_kind1(std::string id) {
    auto arr = std::make_shared<type_const>();
    arr->m_id = id;

    auto k = mk_kfun();
    k->m_left = mk_star();
    k->m_right = mk_star();

    arr->m_kind = k;

    return arr;
}

static inline ptr_type mk_kind2(std::string id) {
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

static inline ptr_type mk_vec() { mk_kind1("vec"); }
static inline ptr_type mk_arrow() { mk_kind2("fun"); }
static inline ptr_type mk_tuple2() { mk_kind2("tuple"); }

bool cmp_kind(const kind *lhs, const kind *rhs) {
    if (lhs->m_is_star == rhs->m_is_star)
        return true;

    auto lsub = (kfun *)lhs;
    auto rsub = (kfun *)rhs;

    return cmp_kind(lsub->m_left.get(), rsub->m_left.get()) &&
           cmp_kind(lsub->m_right.get(), rsub->m_right.get());
}

ptr_type mk_funtype(ptr_type lhs, ptr_type rhs) {
    auto app = std::make_shared<type_app>();
    auto arr = std::make_shared<type_app>();

    arr->m_left = mk_arrow();
    arr->m_right = lhs;

    app->m_left = arr;
    app->m_right = rhs;

    return app;
}

} // namespace lunar