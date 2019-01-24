#include "lunar_type.hpp"

namespace lunar {

bool cmp_kind(const kind *lhs, const kind *rhs) {
    if (lhs->m_is_star == rhs->m_is_star)
        return true;

    auto lsub = (kind_sub *)lhs;
    auto rsub = (kind_sub *)rhs;

    return cmp_kind(lsub->m_left.get(), rsub->m_left.get()) &&
           cmp_kind(lsub->m_right.get(), rsub->m_right.get());
}

} // namespace lunar