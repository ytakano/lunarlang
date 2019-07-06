#include "lunar_type.hpp"
#include "lunar_print.hpp"

#include <assert.h>

#define TYPEERR(M, MSG, AST)                                                   \
    do {                                                                       \
        fprintf(stderr, "%s:%lu:%lu:(%d) " MSG "\n",                           \
                (M)->get_filename().c_str(), (AST)->m_line, (AST)->m_column,   \
                __LINE__);                                                     \
        print_err(AST->m_line, AST->m_column, (M)->get_parsec().get_str());    \
    } while (0)

namespace lunar {

static inline shared_star mk_star() { return std::make_unique<star>(); }
static inline shared_kfun mk_kfun() { return std::make_unique<kfun>(); }

// numtargs = 0 then return *
//            1 then return * -> *
//            2 then return * -> * -> *
static shared_kind make_kind(unsigned int numtargs) {
    if (numtargs == 0) {
        return mk_star();
    } else {
        auto kf = mk_kfun();
        kf->m_left = mk_star();
        kf->m_right = mk_star();
        for (unsigned int i = 1; i < numtargs; i++) {
            auto tmp = kf;
            kf = mk_kfun();
            kf->m_left = mk_star();
            kf->m_right = tmp;
        }
        return kf;
    }
}

shared_type type_const::make(const type_id &id, unsigned int numtargs,
                             CTYPE ctype) {
    auto ret = std::shared_ptr<type_const>(new type_const(CTYPE_PRIMTIVE));

    ret->m_id = id;
    ret->m_kind = make_kind(numtargs);

    return ret;
}

shared_type type_var::make(const std::string &id, unsigned int numtargs) {
    auto ret = std::shared_ptr<type_var>(new type_var);
    ret->m_id = id;
    ret->m_kind = make_kind(numtargs);
    return ret;
}

shared_type type_app::make(shared_type lhs, shared_type rhs) {
    auto ret = std::shared_ptr<type_app>(new type_app);
    ret->m_left = lhs;
    ret->m_right = rhs;
    return ret;
}

static inline shared_type mk_vec(unsigned int dim = 1) {
    type_id id;
    id.m_id = "vec";
    return type_const::make(id, dim);
}

static inline shared_type mk_tuple(unsigned int num) {
    type_id id;
    id.m_id = "tuple";
    return type_const::make(id, num);
}

// make function type
// input:
//   num: 1 + the sum of the number of the arguments and the type arguments
static inline shared_type mk_func(unsigned int num) {
    type_id id;
    id.m_id = "func";
    return type_const::make(id, num, type_const::CTYPE_FUNC);
}

// make natural number type
// input:
//   num:
static inline shared_type mk_nat(const std::string &num) {
    type_id id;
    id.m_id = num;
    return type_const::make(id, 0, type_const::CTYPE_PRIMTIVE);
}

#define APP_TYPES(RET, TS)                                                     \
    do {                                                                       \
        for (auto &ty : TS) {                                                  \
            auto t = make(ptr_mod, ty.get());                                  \
            if (!t)                                                            \
                return nullptr;                                                \
            RET = type_app::make(RET, t);                                      \
        }                                                                      \
    } while (0)

std::shared_ptr<type> type::make(const module *ptr_mod, const ast_type *ptr) {
    switch (ptr->m_type) {
    case ast_type::TYPE_NORMAL: {
        auto t = (const ast_normaltype *)ptr;
        if (t->m_tvar) {
            // type variable
            if (t->m_args) {
                auto ret =
                    type_var::make(t->m_tvar->m_id, t->m_args->m_types.size());
                APP_TYPES(ret, t->m_args->m_types);
                return ret;
            } else {
                return type_var::make(t->m_tvar->m_id, 0);
            }
        } else {
            // normal type, which is either user defined or primitive type
            type_id id;
            if (!ptr_mod->find_type(t->m_id.get(), id.m_path, id.m_id)) {
                // error, no such that type
                TYPEERR(ptr_mod, "undefined type", t);
                return nullptr;
            }
            auto ret = type_const::make(id, t->m_args->m_types.size());
            APP_TYPES(ret, t->m_args->m_types);
            return ret;
        }
    }
    case ast_type::TYPE_FUN: {
        auto t = (const ast_funtype *)ptr;
        auto ret = mk_func(t->m_args->m_types.size() + 1);

        auto tret = make(ptr_mod, t->m_ret.get());
        if (!tret)
            return nullptr;

        ret = type_app::make(ret, tret);
        APP_TYPES(ret, t->m_args->m_types);
        return ret;
    }
    case ast_type::TYPE_TUPLE: {
        auto t = (const ast_tupletype *)ptr;
        auto ret = mk_tuple(t->m_types->m_types.size());
        APP_TYPES(ret, t->m_types->m_types);
        return ret;
    }
    case ast_type::TYPE_VEC: {
        auto t = (const ast_vectype *)ptr;
        auto ret = mk_vec(t->m_nums.size() + 1);
        auto tval = make(ptr_mod, t->m_vectype.get());
        if (!tval)
            return nullptr;

        for (auto &num : t->m_nums) {
            auto n = mk_nat(num->m_num);
            ret = type_app::make(ret, n);
        }
        return ret;
    }
    default:
        assert(false);
        break;
    }
    return nullptr;
}

// if * , * then 0
// if * , (* -> *) then -1
// if * -> * , * then 1
int cmp_kind(const kind *lhs, const kind *rhs) {
    if (lhs->m_is_star == true) {
        if (rhs->m_is_star == true)
            return 0;
    } else {
        if (rhs->m_is_star == false) {
            auto lsub = (const kfun *)lhs;
            auto rsub = (const kfun *)rhs;

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
    for (size_t i = 0; i < args.size(); i++) {
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
// s ∈ {substitutions}
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
// s ∈ {substitutions}
// if ∃s (s L) = R
// then s
// else nullptr
shared_subst match(shared_type lhs, shared_type rhs) {
    if (lhs->m_subtype == type::TYPE_APP && rhs->m_subtype == type::TYPE_APP) {
        auto lapp = std::static_pointer_cast<type_app>(lhs);
        auto rapp = std::static_pointer_cast<type_app>(rhs);
        auto sl = match(lapp->m_left, rapp->m_left);
        auto sr = match(lapp->m_right, rapp->m_right);
        return merge(*sl, *sr);
    }

    if (lhs->m_subtype == type::TYPE_VAR) {
        auto tv = std::static_pointer_cast<type_var>(lhs);
        if (cmp_kind(tv->get_kind().get(), rhs->get_kind().get()) == 0) {
            auto s = std::make_shared<substitution>();
            s->m_subst[*tv] = rhs;
            return s;
        }
    }

    if (lhs->m_subtype == type::TYPE_CONST &&
        rhs->m_subtype == type::TYPE_CONST) {
        if (eq_type(lhs.get(), rhs.get()) == 0)
            return std::make_shared<substitution>();
    }

    // TODO: print error
    return nullptr;
}

bool classenv::add_class(const module *ptr_mod, const ast_class *ptr) {
    auto cls = std::make_shared<typeclass>();

    cls->m_id.m_path = ptr_mod->get_filename();
    cls->m_id.m_id = ptr->m_id->m_id;

    if (ptr->m_preds) {
        for (auto &p : ptr->m_preds->m_preds) {
            auto pd = std::make_shared<pred>();
            if (!ptr_mod->find_typeclass(p->m_id.get(), pd->m_id.m_path,
                                         pd->m_id.m_id)) {
                // error, no such type class
                TYPEERR(ptr_mod, "undefined type class", p);
                return false;
            }

            for (auto &arg : p->m_args->m_types) {
                auto ta = type::make(ptr_mod, arg.get());
                if (!ta)
                    return false;

                pd->m_args.push_back(ta);
            }

            cls->m_preds.push_back(pd);
        }
    }

    // TODO: add arguments and functions

    if (HASKEY(m_env, cls->m_id)) {
        TYPEERR(ptr_mod, "multiply defined class", ptr);
        return false;
    }

    auto e = std::make_unique<env>();
    e->m_class = cls;
    m_env[cls->m_id] = std::move(e);

    return true;
}

bool classenv::add_instance(const module *ptr_mod, const ast_instance *ptr) {
    auto ret = std::make_shared<inst>();
    if (!ptr_mod->find_typeclass(ptr->m_arg->m_id.get(), ret->m_id.m_path,
                                 ret->m_id.m_id)) {
        // error, no such type class
        TYPEERR(ptr_mod, "undefined type class", ptr->m_arg->m_id);
        return false;
    }

    auto cls = m_env.find(ret->m_id);
    assert(cls != m_env.end());

    for (auto &arg : ptr->m_arg->m_args->m_types) {
        auto ta = type::make(ptr_mod, arg.get());
        if (!ta)
            return false;
        ret->m_args.push_back(ta);
    }

    cls->second->m_insts.push_back(ret);

    return true;
}

std::unique_ptr<classenv> classenv::make(const parser &ps) {
    auto ret = std::make_unique<classenv>();
    for (auto &mod : ps.get_modules()) {
        for (auto &cls : mod.second->get_classes()) {
            if (!ret->add_class(mod.second.get(), cls.second.get()))
                return nullptr;
        }
    }

    for (auto &mod : ps.get_modules()) {
        for (auto &inst : mod.second->get_instances()) {
            if (!ret->add_instance(mod.second.get(), inst.second.get()))
                return nullptr;
        }
    }

    return ret;
}

void type_const::print() {
    std::cout << "{\"type\":\"const\",\"id\":";
    m_id.print();
    std::cout << ",\"kind\":\"";
    m_kind->print();
    std::cout << "\"}";
}

void type_var::print() {
    std::cout << "{\"type\":\"tvar\",\"id\":\"" << m_id << "\",\"kind\":\"";
    m_kind->print();
    std::cout << "\"}";
}

void type_app::print() {
    std::cout << "{\"type\":\"app\",\"left\":";
    m_left->print();
    std::cout << ",\"right\":";
    m_right->print();
    std::cout << "}";
}

void pred::print() {
    std::cout << "{\"id\":";
    m_id.print();
    std::cout << ",\"args\":[";
    int n = 0;
    for (auto &arg : m_args) {
        if (n > 0)
            std::cout << ",";
        arg->print();
        n++;
    }
    std::cout << "]}";
}

void qual::print_preds() {
    std::cout << "[";
    int n = 0;
    for (auto &p : m_preds) {
        if (n > 0)
            std::cout << ",";
        p->print();
        n++;
    }
    std::cout << "]";
}

void typeclass::print() {
    std::cout << "{\"id\":";
    m_id.print();
    std::cout << ",\"predicates\":";
    print_preds();

    // TODO: print arguments and functions

    std::cout << "}";
}

void classenv::print() {
    std::cout << "[";
    int n = 0;
    for (auto &e : m_env) {
        if (n > 0)
            std::cout << ",";

        std::cout << "{\"class\":";
        e.second->m_class->print();

        int m = 0;
        std::cout << ",\"instance\":[";
        for (auto &inst : e.second->m_insts) {
            if (m > 0)
                std::cout << ",";
            inst->print();
            m++;
        }
        std::cout << "]}";

        n++;
    }
    std::cout << "]";
}

void inst::print() {
    std::cout << "{\"id\":";
    m_id.print();

    int n = 0;
    std::cout << ",\"args\":[";
    for (auto &arg : m_args) {
        if (n > 0)
            std::cout << ",";
        arg->print();
        n++;
    }
    std::cout << "]}";
}

void typing(ptr_ast_type &ast) {}

} // namespace lunar
