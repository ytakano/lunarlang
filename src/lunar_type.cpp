#include "lunar_type.hpp"
#include "lunar_print.hpp"

#include <assert.h>

#include <boost/lexical_cast.hpp>

#define TYPEINFO(MSG, M, AST)                                                  \
    do {                                                                       \
        fprintf(stderr, "%s:%lu:%lu:(%d) %s\n", (M)->get_filename().c_str(),   \
                (AST)->m_line, (AST)->m_column, __LINE__, MSG);                \
        print_err(AST->m_line, AST->m_column, (M)->get_parsec().get_str());    \
    } while (0)

#define TYPEERR(MSG, M, AST)                                                   \
    do {                                                                       \
        fprintf(stderr, "%s:%lu:%lu:(%d) error: %s\n",                         \
                (M)->get_filename().c_str(), (AST)->m_line, (AST)->m_column,   \
                __LINE__, MSG);                                                \
        print_err(AST->m_line, AST->m_column, (M)->get_parsec().get_str());    \
    } while (0)

#define TYPEERR2(MSG, M1, M2, AST1, AST2)                                      \
    do {                                                                       \
        fprintf(stderr, "(%d) error: " MSG "\n", __LINE__);                    \
        fprintf(stderr, "%s:%lu:%lu:\n", (M1)->get_filename().c_str(),         \
                (AST1)->m_line, (AST1)->m_column);                             \
        print_err(AST1->m_line, AST1->m_column, (M1)->get_parsec().get_str()); \
        fprintf(stderr, "%s:%lu:%lu:\n", (M2)->get_filename().c_str(),         \
                (AST2)->m_line, (AST2)->m_column);                             \
        print_err(AST2->m_line, AST2->m_column, (M2)->get_parsec().get_str()); \
    } while (0)

namespace lunar {

static inline shared_star mk_star() { return std::make_unique<star>(); }
static inline shared_kfun mk_kfun() { return std::make_unique<kfun>(); }

class built_in_type {
  public:
    built_in_type() {
        m_1st.insert("u64");
        m_1st.insert("s64");
        m_1st.insert("u32");
        m_1st.insert("s32");
        m_1st.insert("u16");
        m_1st.insert("s16");
        m_1st.insert("u8");
        m_1st.insert("s8");
        m_1st.insert("fp64");
        m_1st.insert("fp32");
        m_1st.insert("void");
        m_1st.insert("bool");
    }

    shared_type make(const std::string &s) {
        if (HASKEY(m_1st, s)) {
            type_id id;
            id.m_id = s;
            id.m_path = "built-in";
            return type_const::make(id, 0);
        }
        return nullptr;
    }

  private:
    std::unordered_set<std::string> m_1st;
};

static built_in_type gen_built_in;

static uint64_t de_bruijn_idx = 0;

std::string gensym() {
    return "T" + boost::lexical_cast<std::string>(de_bruijn_idx++);
}

static void de_bruijn(qual *ptr, type *ptr_type) {
    switch (ptr_type->m_subtype) {
    case type::TYPE_APP: {
        auto app = (type_app *)ptr_type;
        de_bruijn(ptr, app->m_left.get());
        de_bruijn(ptr, app->m_right.get());
        break;
    }
    case type::TYPE_VAR: {
        auto var = (type_var *)ptr_type;
        auto id = var->get_id();
        if (id[0] == 'T')
            return;

        auto s = ptr->find_tvar_idx(id);
        if (s == "") {
            var->set_id(gensym());
            ptr->m_idx_tvar.insert(bimap_ss::value_type(var->get_id(), id));
        } else {
            var->set_id(s);
        }
        break;
    }
    case type::TYPE_CONST:
        break;
    }
}

static void de_bruijn(inst *ptr) {
    de_bruijn(ptr, ptr->m_pred.m_arg.get());

    for (auto &p : ptr->m_preds) {
        de_bruijn(ptr, p->m_arg.get());
    }

    for (auto &f : ptr->m_funcs) {
        for (auto &p : f.second->m_preds) {
            de_bruijn(f.second.get(), p->m_arg.get());
        }
        de_bruijn(f.second.get(), f.second->m_type.get());
    }
}

static void de_bruijn(typeclass *ptr) {
    std::string tmp = ptr->m_arg;
    ptr->m_arg = gensym();
    ptr->m_idx_tvar.insert(bimap_ss::value_type(ptr->m_arg, tmp));

    for (auto &p : ptr->m_preds) {
        de_bruijn(ptr, p->m_arg.get());
    }

    for (auto &fun : ptr->m_funcs) {
        de_bruijn(fun.second.get(), fun.second->m_type.get());
    }

    for (auto &tv : ptr->m_tvar_constraint) {
        assert(tv->m_subtype == type::TYPE_VAR);
        auto t = (type_var *)tv.get();
        auto it = ptr->m_idx_tvar.right.find(t->get_id());
        if (it != ptr->m_idx_tvar.right.end())
            t->set_id(it->second);
    }
}

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

uint16_t kind::num_args(const ast_kind *ptr) {
    uint16_t n = 0; // the number of arguments
    for (auto k = ptr; k->m_asttype != ast::AST_KSTAR;
         k = ((ast_kfun *)k)->m_right.get()) {
        assert(k->m_asttype == ast::AST_KFUN);
        assert(((ast_kfun *)k)->m_right->m_asttype == ast::AST_KSTAR);
        n++;
    }
    return n;
}

shared_type type_const::make(const type_id &id, unsigned int numtargs) {
    auto ret = std::shared_ptr<type_const>(new type_const());

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
//   num: 1 + the sum of the number of the arguments
static inline shared_type mk_func(unsigned int num) {
    type_id id;
    id.m_id = "func";
    return type_const::make(id, num);
}

// make natural number type
// input:
//   num:
static inline shared_type mk_nat(const std::string &num) {
    type_id id;
    id.m_id = num;
    return type_const::make(id, 0);
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
            auto ptr_ast =
                ptr_mod->find_type(t->m_id.get(), id.m_path, id.m_id);
            if (!ptr_ast) {
                // make built-in type
                if (t->m_id->m_ids.size() == 1) {
                    auto ret = gen_built_in.make(t->m_id->m_ids[0]->m_id);
                    if (ret)
                        return ret;
                }

                // error, no such that type
                TYPEERR("undefined type", ptr_mod, t);
                return nullptr;
            }

            int argnum = 0;
            if (ptr_ast->m_asttype != ast::AST_MEMBER) {
                assert(ptr_ast->m_asttype == ast::AST_TYPE);
                auto tn = (ast_type *)ptr_ast;
                switch (tn->m_type) {
                case ast_type::TYPE_STRUCT: {
                    auto st = (ast_struct *)tn;
                    if (st->m_tvars)
                        argnum = st->m_tvars->m_args.size();
                    break;
                }
                case ast_type::TYPE_UNION: {
                    auto un = (ast_union *)tn;
                    if (un->m_tvars)
                        argnum = un->m_tvars->m_args.size();
                    break;
                }
                default:
                    assert(false); // never reach here
                }
            }

            if (t->m_args) {
                if (t->m_args->m_types.size() != argnum) {
                    TYPEERR("the number of arguments is different", ptr_mod, t);
                    return nullptr;
                }

                auto ret = type_const::make(id, t->m_args->m_types.size());
                APP_TYPES(ret, t->m_args->m_types);
                return ret;
            } else {
                return type_const::make(id, argnum);
            }
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

std::unique_ptr<defun> defun::make(const module *ptr_mod, const qual *parent,
                                   const ast_defun *ast) {
    auto ret = std::make_unique<defun>();
    ret->m_ast = ast;
    ret->m_module = ptr_mod;
    ret->m_parent = parent;

    // predicates
    if (ast->m_preds) {
        for (auto &p : ast->m_preds->m_preds) {
            auto pd = pred::make(ptr_mod, p.get());
            if (!pd)
                return nullptr;

            if (!ret->add_constraints(pd.get()))
                return nullptr;

            ret->m_preds.push_back(std::move(pd));
        }
    }

    shared_type ft;
    if (ast->m_args) {
        // type of function
        ft = mk_func(ast->m_args->m_args.size() + 1);

        // types of arguments
        std::unordered_set<std::string> ids;
        star ks;
        for (auto &arg : ast->m_args->m_args) {
            if (HASKEY(ids, arg->m_id->m_id)) {
                TYPEERR("same argument name is used", ptr_mod, arg->m_id);
                return nullptr;
            }

            ids.insert(arg->m_id->m_id);

            shared_type t;
            if (arg->m_type) {
                t = type::make(ptr_mod, arg->m_type.get());
                if (!t)
                    return nullptr;

                if (cmp_kind(t->get_kind().get(), &ks) != 0) {
                    TYPEERR("not normal type", ptr_mod, arg->m_type);
                    return nullptr;
                }
            } else {
                t = type_var::make(gensym(), 0);
            }

            ret->m_args.push_back(
                std::pair<std::string, shared_type>(arg->m_id->m_id, t));
            ret->m_assump[arg->m_id->m_id] = t;
            ft = type_app::make(ft, t);
        }
    } else {
        ft = mk_func(1);
    }

    // type of return value
    shared_type rt;
    if (ast->m_ret) {
        rt = type::make(ptr_mod, ast->m_ret.get());
        if (!rt)
            return nullptr;
    } else {
        rt = type_var::make(gensym(), 0);
    }
    ret->m_ret = rt;
    ft = type_app::make(ft, rt);

    ret->m_type = std::move(ft);

    // check kind constraints
    if (!ret->add_constraints(ret->m_type.get()))
        return nullptr;

    return ret;
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
    case type::TYPE_APP: {
        auto tapp = std::static_pointer_cast<type_app>(type);
        return type_app::make(apply(tapp->m_left), apply(tapp->m_right));
    }
    }

    return nullptr; // never reach here
}

// [T0 -> t :: *]
// (T0 :: * -> *, T1 :: *)
// (T0 :: * -> *, t :: *)

uniq_pred substitution::apply(pred *p) {
    auto ret = std::make_unique<pred>();

    ret->m_arg = apply(p->m_arg);
    ret->m_id = p->m_id;

    return ret;
}

// s1 = {x1 -> t1, ..., xn -> tn}
// s2 = {y1 -> u1, ..., ym -> um}
// 1.
//   s3 = {y1 -> s1 u1, ..., ym -> s1 um}
// 2.
//   s4 = {s | s ∈ s1 ∧ ¬(dom(s) ∈ dom(s2))}
// 3.
//   s3 ∪ s4
static uniq_subst compose(substitution &s1, substitution &s2) {
    auto ret = std::make_unique<substitution>();
    for (auto &y : s2.m_subst) {
        ret->m_subst[y.first] = s1.apply(y.second);
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

static uniq_subst var_bind(shared_type var, shared_type ty) {
    auto tv1 = std::static_pointer_cast<type_var>(var);
    if (ty->m_subtype == type::TYPE_VAR) {
        auto tv2 = std::static_pointer_cast<type_var>(ty);
        if (*tv1 == *tv2)
            return std::make_unique<substitution>();
    }

    if (occurs_check(*tv1, ty.get()))
        return nullptr;

    if (cmp_kind(var->get_kind().get(), ty->get_kind().get()) != 0)
        return nullptr;

    auto s = std::make_unique<substitution>();
    s->m_subst[*tv1] = ty;
    return s;
}

// L, R ∈ {types}
// s ∈ {substitutions}
// if ∃s (s L) = (s R)
// then s (s is the most general unifier)
// else nullptr
uniq_subst mgu(shared_type lhs, shared_type rhs) {
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
        if (*((type_const *)lhs.get()) != *((type_const *)rhs.get())) {
            return nullptr;
        }
        return std::make_unique<substitution>();
    }

    return nullptr;
}

// s1, s2: substitution
// x ∈ dom(s1)
// y ∈ dom(s2)
// if ∀x ∀y (x = y) -> (s1 x = s2 x)
// then s1 ∪ s2
// else nullptr
static uniq_subst merge(substitution &s1, substitution &s2) {
    auto ret = std::make_unique<substitution>();
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
uniq_subst match(shared_type lhs, shared_type rhs) {
    if (lhs->m_subtype == type::TYPE_APP && rhs->m_subtype == type::TYPE_APP) {
        auto lapp = std::static_pointer_cast<type_app>(lhs);
        auto rapp = std::static_pointer_cast<type_app>(rhs);

        auto sl = match(lapp->m_left, rapp->m_left);
        if (!sl)
            return nullptr;

        auto sr = match(lapp->m_right, rapp->m_right);
        if (!sl)
            return nullptr;

        return merge(*sl, *sr);
    }

    if (lhs->m_subtype == type::TYPE_VAR) {
        auto tv = std::static_pointer_cast<type_var>(lhs);
        if (cmp_kind(tv->get_kind().get(), rhs->get_kind().get()) == 0) {
            auto s = std::make_unique<substitution>();
            s->m_subst[*tv] = rhs;
            return s;
        }
    }

    if (lhs->m_subtype == type::TYPE_CONST &&
        rhs->m_subtype == type::TYPE_CONST) {
        if (eq_type(lhs.get(), rhs.get()))
            return std::make_unique<substitution>();
    }

    return nullptr;
}

uniq_subst mgu_pred(pred *lhs, pred *rhs) {
    if (lhs->m_id != rhs->m_id)
        return nullptr;

    return mgu(lhs->m_arg, rhs->m_arg);
}

uniq_subst match_pred(pred *lhs, pred *rhs) {
    if (lhs->m_id != rhs->m_id)
        return nullptr;

    return match(lhs->m_arg, rhs->m_arg);
}

uniq_pred pred::make(const module *ptr_mod, const ast_pred *ptr) {
    auto ret = std::make_unique<pred>();
    if (!ptr_mod->find_typeclass(ptr->m_id.get(), ret->m_id.m_path,
                                 ret->m_id.m_id)) {
        // error, no such type class
        TYPEERR("undefined type class", ptr_mod, ptr);
        return nullptr;
    }

    ret->m_arg = type::make(ptr_mod, ptr->m_arg.get());
    if (!ret->m_arg)
        return nullptr;

    return ret;
}

bool typeclass::apply_super(shared_type arg, std::vector<uniq_pred> &ret,
                            const module *ptr_mod, const ast *ptr_ast) {
    bool found;
    if (!check_kind_constraint(m_arg, arg->get_kind().get(), found)) {
        return false;
    }

    // make substitution
    auto tv = type_var::make(m_arg, arg->get_kind());
    auto s = var_bind(tv, arg);

    // make predicates by applying the substitution to the super classes
    for (auto &p : m_preds) {
        auto p0 = s->apply(p.get());
        ret.push_back(std::move(p0));
    }

    return true;
}

bool qual::add_constraints(pred *p) { return add_constraints(p->m_arg.get()); }

bool qual::add_constraints(type *p) {
    switch (p->m_subtype) {
    case type::TYPE_CONST:
        return true;
    case type::TYPE_VAR: {
        auto tv = (type_var *)p;
        auto k = tv->get_kind();
        bool found;
        if (!check_kind_constraint(tv->get_id(), k.get(), found))
            return false;

        if (!found) {
            auto r = type_var::make(tv->get_id(), k);
            m_tvar_constraint.push_back(r);
        }

        return true;
    }
    case type::TYPE_APP: {
        auto ta = (type_app *)p;
        if (!add_constraints(ta->m_left.get()))
            return false;
        if (!add_constraints(ta->m_right.get()))
            return false;
        return true;
    }
    }

    return true; // never reach here
}

shared_type typeclass::make_funtype(const module *ptr_mod,
                                    ast_interface *ptr_ast) {
    auto ft = mk_func(ptr_ast->m_args->m_types.size() + 1);
    for (auto &arg : ptr_ast->m_args->m_types) {
        auto t = type::make(ptr_mod, arg.get());
        if (!t)
            return nullptr;

        ft = type_app::make(ft, t);
    }

    auto r = type::make(ptr_mod, ptr_ast->m_ret.get());
    if (!r)
        return nullptr;

    ft = type_app::make(ft, r);

    return ft;
}

bool typeclass::add_interface(const module *ptr_mod, const std::string &id,
                              ast_interface *ptr_ast) {
    type_id tid;
    tid.m_path = m_id.m_path;

    tid.m_id = id;
    if (HASKEY(m_funcs, tid)) {
        TYPEERR("multiply defined method", ptr_mod, ptr_ast);
        return false;
    }

    auto ft = make_funtype(ptr_mod, ptr_ast);
    if (!ft)
        return false;

    auto qt = std::make_unique<qual_type>();
    qt->m_type = ft;
    qt->m_parent = this;
    qt->m_ast = ptr_ast;
    qt->m_module = ptr_mod;

    if (!qt->add_constraints(ft.get()))
        return false;

    m_funcs[tid] = std::move(qt);

    return true;
}

bool classenv::add_class(const module *ptr_mod, const ast_class *ptr) {
    auto cls = std::make_unique<typeclass>();

    cls->m_id.m_path = ptr_mod->get_filename();
    cls->m_id.m_id = ptr->m_id->m_id;

    if (HASKEY(m_env, cls->m_id)) {
        TYPEERR("multiply defined class", ptr_mod, ptr);
        return false;
    }

    // add type arguments
    cls->m_arg = ptr->m_tvar->m_id->m_id;

    // get the number of arguments of the class
    if (ptr->m_tvar->m_kind) {
        auto nargs = kind::num_args(ptr->m_tvar->m_kind.get());
        auto v = type_var::make(ptr->m_tvar->m_id->m_id, nargs);
        cls->m_tvar_constraint.push_back(v);
    }

    cls->m_ast = ptr;
    cls->m_module = ptr_mod;

    // add super classes
    if (ptr->m_preds) {
        for (auto &p : ptr->m_preds->m_preds) {
            auto pd = pred::make(ptr_mod, p.get());
            if (!pd)
                return false;

            if (!cls->add_constraints(pd.get()))
                return false;

            if (!pd->in_hnf()) {
                TYPEERR("the predicate must be head normal form", ptr_mod, p);
                return false;
            }

            if (!pd->is_head_var(cls->m_arg)) {
                TYPEERR("the head of the type arguments must be same as "
                        "the class's argument",
                        ptr_mod, p->m_arg);
                return false;
            }

            cls->m_preds.push_back(std::move(pd));
        }
    }

    // add interfaces
    type_id id;
    id.m_path = cls->m_id.m_path;
    int idx = 0;
    for (auto &fun : ptr->m_interfaces->m_interfaces) {
        // add interface
        for (auto &i : fun->m_id) {
            cls->add_interface(ptr_mod, i->m_id, fun.get());
        }

        // add infix
        for (auto &i : fun->m_infix) {
            cls->add_interface(ptr_mod, i->m_infix, fun.get());
        }
    }

    de_bruijn(cls.get());

    auto e = std::make_unique<env>();
    e->m_class = std::move(cls);
    m_env[e->m_class->m_id] = std::move(e);

    return true;
}

// return nullptr if no overlapped instance is found
// otherwise return the pointer to the overlapped instance
inst *classenv::overlap(pred &ptr) {
    auto is = m_env.find(ptr.m_id);
    assert(is != m_env.end());

    for (auto &it : is->second->m_insts) {
        auto s = mgu_pred(&it->m_pred, &ptr);
        if (s)
            return it.get();
    }

    return nullptr;
}

bool classenv::is_inherit_instance(const pred &p, const module *ptr_mod,
                                   const ast *ptr_ast) {

    auto cls = m_env.find(p.m_id);
    assert(cls != m_env.end());

    std::vector<uniq_pred> super;
    if (!cls->second->m_class->apply_super(p.m_arg, super, ptr_mod, ptr_ast)) {
        TYPEINFO("instantiated by", ptr_mod, ptr_ast);
        return false;
    }

    if (super.empty())
        return true;

    int idx = 0;
    for (auto &sp : super) {
        std::vector<uniq_pred> ret;

        auto c = m_env.find(sp->m_id);
        assert(c != m_env.end());
        auto clsast = (const ast_class *)cls->second->m_class->m_ast;

        for (auto &in : c->second->m_insts) {
            auto sbst = match_pred(&in->m_pred, sp.get());
            if (sbst)
                goto found;
        }

        {
            if (ret.empty()) {
                auto errmsg = sp->to_str() + " was not instantiated";
                TYPEERR(errmsg.c_str(), cls->second->m_class->m_module,
                        clsast->m_preds->m_preds[idx]);
                TYPEINFO("instantiated by", ptr_mod, ptr_ast);
                return false;
            }
        }

    found:

        if (!is_inherit_instance(*sp, cls->second->m_class->m_module,
                                 clsast->m_preds->m_preds[idx].get())) {
            TYPEINFO("instantiated by", ptr_mod, ptr_ast);
            return false;
        }

        idx++;
    }

    return true;
}

bool classenv::add_instance(const module *ptr_mod,
                            const ast_instance *ptr_ast) {
    auto ret = std::make_unique<inst>();
    if (!ptr_mod->find_typeclass(ptr_ast->m_arg->m_id.get(),
                                 ret->m_pred.m_id.m_path,
                                 ret->m_pred.m_id.m_id)) {
        // error, no such type class
        TYPEERR("undefined type class", ptr_mod, ptr_ast->m_arg->m_id);
        return false;
    }

    ret->m_ast = ptr_ast;
    ret->m_module = ptr_mod;

    auto cls = m_env.find(ret->m_pred.m_id);
    assert(cls != m_env.end());

    // type arguments
    ret->m_pred.m_arg = type::make(ptr_mod, ptr_ast->m_arg->m_arg.get());
    if (!ret->m_pred.m_arg)
        return false;

    if (!ret->add_constraints(ret->m_pred.m_arg.get()))
        return false;

    // check the instance is overlapped
    auto is = overlap(ret->m_pred);
    if (is) {
        TYPEERR2("instance declarations are overlapped", ptr_mod, is->m_module,
                 ptr_ast->m_arg, ((ast_instance *)is->m_ast)->m_arg);
        return false;
    }

    // add requirements
    if (ptr_ast->m_req) {
        for (auto &p : ptr_ast->m_req->m_preds) {
            auto pd = pred::make(ptr_mod, p.get());
            if (!pd)
                return false;

            if (!ret->add_constraints(pd.get()))
                return false;

            ret->m_preds.push_back(std::move(pd));
        }
    }

    // add interfaces
    for (auto &fun : ptr_ast->m_id2defun) {
        auto pf = defun::make(ptr_mod, ret.get(), fun.second.get());
        if (!pf)
            return false;

        ret->m_funcs[fun.first] = std::move(pf);
    }

    de_bruijn(ret.get());

    cls->second->m_insts.push_back(std::move(ret));

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

    if (!ret->is_asyclic())
        return nullptr;

    // reduce contexts of instances' predicates
    for (auto &e : ret->m_env) {
        for (auto &it : e.second->m_insts) {
            int idx = 0;
            if (!ret->reduce(it->m_preds, idx)) {
                assert(it->m_ast->m_asttype == ast::AST_INSTANCE);
                auto p = (ast_instance *)it->m_ast;
                TYPEERR("failed context reduction. predicates must be head "
                        "normal form, or could be reduced to head normal form",
                        it->m_module, p->m_req->m_preds[idx]);
                return nullptr;
            }
        }
    }

    // check wheter instances are also the instances of the super classes
    for (auto &e : ret->m_env) {
        for (auto &it : e.second->m_insts) {
            auto inast = (const ast_instance *)it->m_ast;
            if (!ret->is_inherit_instance(it->m_pred, it->m_module,
                                          inast->m_arg.get())) {
                return nullptr;
            }
        }
    }

    for (auto &e : ret->m_env) {
        for (auto &in : e.second->m_insts) {
            for (auto &f : in->m_funcs) {
                // check interfaces' type is compatible with the class's
                auto sbst = ret->mgu_if_type(e.second->m_class.get(), in.get(),
                                             f.first, f.second.get());
                if (!sbst)
                    return nullptr;

                f.second->m_type = sbst->apply(f.second->m_type);

                for (auto &p : in->m_preds) {
                    p->m_arg = sbst->apply(p->m_arg);
                }

                for (auto &p : f.second->m_preds) {
                    p->m_arg = sbst->apply(p->m_arg);
                }
            }

            for (auto &sf : e.second->m_class->m_funcs) {
                if (!HASKEY(in->m_funcs, sf.first.m_id)) {
                    std::string err = "class method \"" + sf.first.m_id +
                                      "\" is not implemented";
                    TYPEERR(err.c_str(), in->m_module, in->m_ast);
                    return nullptr;
                }
            }
        }
    }

    return ret;
}

uniq_subst classenv::mgu_if_type(typeclass *cls, inst *in,
                                 const std::string &id, defun *qt) {
    type_id tid;
    tid.m_id = id;
    tid.m_path = cls->m_id.m_path;
    auto parent = cls->m_funcs.find(tid);
    if (parent == cls->m_funcs.end()) {
        std::string err = "class \"" + cls->m_id.m_id + "\" has no such method";
        TYPEERR(err.c_str(), in->m_module, in->m_funcs[id]->m_ast);
        return nullptr;
    }

    substitution sbst;
    type_var tv(cls->m_arg, in->m_pred.m_arg->get_kind());
    sbst.m_subst[tv] = in->m_pred.m_arg;

    auto pt = sbst.apply(parent->second->m_type);

    auto ret = mgu(pt, qt->m_type);
    if (!ret) {
        std::string err =
            "method type is incompatible with the definition of the class";
        TYPEERR(err.c_str(), in->m_module, in->m_funcs[id]->m_ast);
        TYPEINFO("defined by", cls->m_module, parent->second->m_ast);
        return nullptr;
    }

    return ret;
}

std::string qual::find_tvar_idx(const std::string &id) const {
    auto it = m_idx_tvar.right.find(id);
    if (it == m_idx_tvar.right.end()) {
        if (m_parent)
            return m_parent->find_tvar_idx(id);
        else
            return "";
    }

    return it->second;
}

bool classenv::is_asyclic() {
    for (auto &it : m_env) {
        std::unordered_set<type_id> visited;
        if (!is_asyclic(it.second->m_class->m_module, it.second->m_class.get(),
                        visited)) {
            return false;
        }
    }

    return true;
}

bool classenv::is_asyclic(const module *ptr_mod, typeclass *ptr,
                          std::unordered_set<type_id> &visited) {
    if (ptr->m_is_asyclic == typeclass::ASYCLIC_YES)
        return true;

    if (ptr->m_preds.size() == 0) {
        ptr->m_is_asyclic = typeclass::ASYCLIC_YES;
        return true;
    }

    visited.insert(ptr->m_id);
    for (auto &it : ptr->m_preds) {
        if (HASKEY(visited, it->m_id)) {
            ptr->m_is_asyclic = typeclass::ASYCLIC_NO;
            TYPEERR("class is cyclically defined", ptr_mod, ptr->m_ast);
            return false;
        }

        auto e = m_env.find(it->m_id);
        assert(e != m_env.end());

        if (!is_asyclic(ptr_mod, e->second->m_class.get(), visited)) {
            ptr->m_is_asyclic = typeclass::ASYCLIC_NO;
            return false;
        }
    }

    ptr->m_is_asyclic = typeclass::ASYCLIC_YES;
    return true;
}

bool qual::check_kind_constraint(const std::string &id, kind *k, bool &found) {
    found = false;
    for (const qual *p = this; p != nullptr; p = p->m_parent) {
        for (auto &tv : p->m_tvar_constraint) {
            if (id == ((type_var *)tv.get())->get_id()) {
                found = true;
                if (cmp_kind(k, tv->get_kind().get()) != 0) {
                    std::string oid;

                    auto it = p->m_idx_tvar.left.find(id);
                    if (it != p->m_idx_tvar.left.end()) {
                        oid = it->second;
                    } else {
                        oid = id;
                    }

                    std::string err =
                        "kinds are different. \"" + oid + "\" must be \"" +
                        tv->get_kind()->to_str() + "\" but used as was \"" +
                        k->to_str() + "\"";
                    TYPEERR(err.c_str(), m_module, m_ast);

                    return false;
                }
            }
        }
    }
    return true;
}

bool classenv::by_super(pred *pd, std::vector<uniq_pred> &ret) {
    auto p = std::make_unique<pred>(*pd);
    ret.push_back(std::move(p));

    auto it = m_env.find(pd->m_id);
    assert(it != m_env.end());

    // check wheter the kind of the argument satsfies the constraints
    auto const &id = it->second->m_class->m_arg;
    auto const k = pd->m_arg->get_kind();
    bool found;
    if (!it->second->m_class->check_kind_constraint(id, k.get(), found)) {
        return false;
    }

    // get variable bindings
    substitution sbst;
    type_var tv(id, k);
    sbst.m_subst[tv] = pd->m_arg;

    for (auto &s : it->second->m_class->m_preds) { // get super classes
        // apply the substitution to the super classes
        pred sup;
        sup.m_id = s->m_id;
        sup.m_arg = sbst.apply(s->m_arg);
        by_super(&sup, ret);
    }

    return true;
}

// find the instance of a predicate, and return predicates required
// by the instance
void classenv::by_inst(pred *pd, std::vector<uniq_pred> &ret) {
    auto it = m_env.find(pd->m_id);
    assert(it != m_env.end());

    for (auto &is : it->second->m_insts) {
        auto sbst = match_pred(&is->m_pred, pd);
        if (sbst) {
            for (auto &p : is->m_preds) {
                if (!p)
                    continue;
                auto np = sbst->apply(p.get());
                ret.push_back(std::move(np));
            }
        }
    }
}

TRIVAL classenv::entail(std::vector<uniq_pred> &ps, pred *p) {
    {
        for (auto &p0 : ps) {
            std::vector<uniq_pred> super;
            if (p0 && !by_super(p0.get(), super))
                return TRI_FAIL;

            for (auto &s : super) {
                if (*s == *p)
                    return TRI_TRUE;
            }
        }
    }

    std::vector<uniq_pred> qs;
    by_inst(p, qs);
    if (qs.empty())
        return TRI_FALSE;

    for (auto &s : qs) {
        auto r = entail(ps, s.get());
        if (r == TRI_TRUE)
            continue;
        return r;
    }

    return TRI_TRUE;
}

bool classenv::to_hnfs(std::vector<uniq_pred> &ps, std::vector<uniq_pred> &ret,
                       int &idx) {
    for (auto &p : ps) {
        if (!to_hnf(std::move(p), ret)) {
            return false;
        }
        idx++;
    }
    return true;
}

bool classenv::to_hnf(uniq_pred pd, std::vector<uniq_pred> &ret) {
    if (pd->in_hnf()) {
        ret.push_back(std::move(pd));
        return true;
    }

    std::vector<uniq_pred> ps;
    by_inst(pd.get(), ps);
    if (ps.empty())
        return false;

    int idx = 0;
    return to_hnfs(ps, ret, idx);
}

bool classenv::simplify(std::vector<uniq_pred> &ps) {
    for (int i = 0; i < ps.size(); i++) {
        assert(ps[i]);
        auto tmp = std::move(ps[i]);
        switch (entail(ps, tmp.get())) {
        case TRI_TRUE: {
            ps[i] = std::move(ps[ps.size() - 1]);
            ps.pop_back();
            i--;
            break;
        }
        case TRI_FALSE:
            ps[i] = std::move(tmp);
            break;
        case TRI_FAIL:
            return false;
        }
    }

    return true;
}

bool classenv::reduce(std::vector<uniq_pred> &ps, int &idx) {
    std::vector<uniq_pred> r;
    if (!to_hnfs(ps, r, idx))
        return false;

    ps.clear();
    for (auto &it : r) {
        ps.push_back(std::move(it));
    }

    return simplify(ps);
}

std::unique_ptr<funcenv> funcenv::make(const parser &ps) {
    auto ret = std::make_unique<funcenv>();

    for (auto &m : ps.get_modules()) {
        for (auto &fun : m.second->get_funcs()) {
            auto f = defun::make(m.second.get(), nullptr, fun.second.get());
            if (!f)
                return nullptr;

            for (auto &p : f->m_preds) {
                de_bruijn(f.get(), p->m_arg.get());
            }

            de_bruijn(f.get(), f->m_type.get());

            type_id id;
            id.m_id = fun.first;
            id.m_path = m.second->get_filename();

            ret->m_defuns[id] = std::move(f);
        }
    }

    return ret;
}

bool typeenv::check_tvar(type *ptr,
                         const std::unordered_set<std::string> &tvars) {
    switch (ptr->m_subtype) {
    case type::TYPE_APP: {
        auto app = (type_app *)ptr;
        return check_tvar(app->m_left.get(), tvars) &&
               check_tvar(app->m_right.get(), tvars);
    }
    case type::TYPE_CONST:
        return true;
    case type::TYPE_VAR: {
        auto tv = (type_var *)ptr;
        if (!HASKEY(tvars, tv->get_id()))
            return false;

        if (!tv->get_kind()->m_is_star)
            return false;

        return true;
    }
    }
}

std::shared_ptr<typeenv::typeinfo>
typeenv::make_typeinfo(const module *ptr_mod, const ast_userdef *ptr_ast) {
    auto ret = std::make_shared<typeinfo>();

    ret->m_ast = ptr_ast;
    ret->m_module = ptr_mod;

    type_id id;
    id.m_id = ptr_ast->m_id->m_id;
    id.m_path = ptr_mod->get_filename();

    unsigned int tn = ptr_ast->m_tvars ? ptr_ast->m_tvars->m_args.size() : 0;
    ret->m_type.m_type = type_const::make(id, tn);

    std::unordered_set<std::string> tvars;
    // type arguments
    if (ptr_ast->m_tvars) {
        for (auto &arg : ptr_ast->m_tvars->m_args) {
            // check kind
            auto nargs = arg->m_kind ? kind::num_args(arg->m_kind.get()) : 0;
            if (nargs != 0) {
                // kind must be *
                TYPEERR("kind must be *", ptr_mod, arg->m_kind);
                return nullptr;
            }

            // must not use same type variable name
            if (HASKEY(tvars, arg->m_id->m_id)) {
                TYPEERR("same argument name is used", ptr_mod, arg);
                return nullptr;
            }
            tvars.insert(arg->m_id->m_id);

            auto ta = type_var::make(arg->m_id->m_id, make_kind(0));
            ret->m_args.push_back(ta);

            // apply the argument
            ret->m_type.m_type = type_app::make(ret->m_type.m_type, ta);
        }
    }

    // predicates
    if (ptr_ast->m_preds) {
        for (auto &p : ptr_ast->m_preds->m_preds) {
            // make predicate
            auto pd = pred::make(ptr_mod, p.get());
            if (!pd)
                return nullptr;

            // the argument must be a type variable whose kind is *
            if (pd->m_arg->m_subtype != type::TYPE_VAR) {
                TYPEERR("the predicate must be a type variable whose "
                        "kind is *",
                        ptr_mod, p);
                return nullptr;
            }

            // the predicate's argument must be
            // in the type variable arguments
            if (!HASKEY(tvars, ((type_var *)pd->m_arg.get())->get_id())) {
                TYPEERR("undefined type variable is used", ptr_mod, p->m_arg);
                return nullptr;
            }

            ret->m_type.m_preds.push_back(std::move(pd));
        }
    }

    // members
    for (auto &mem : ptr_ast->m_members->m_vars) {
        // create type
        shared_type tp;
        if (mem->m_type) {
            tp = type::make(ptr_mod, mem->m_type.get());
            if (!tp)
                return nullptr;
        } else {
            tp = gen_built_in.make("void");
            assert(tp);
        }

        // only a type variable, which is defined as an argument,
        // is permitted
        if (!check_tvar(tp.get(), tvars))
            return nullptr;

        ret->m_members.insert(memv(mem->m_id->m_id, tp));
    }

    return ret;
}

std::unique_ptr<typeenv> typeenv::make(const parser &ps) {
    auto ret = std::make_unique<typeenv>();
    for (auto &m : ps.get_modules()) {
        for (auto &s : m.second->get_struct()) {
            auto info = make_typeinfo(m.second.get(), s.second.get());
            info->m_is_struct = true;

            type_id id;
            id.m_id = s.second->m_id->m_id;
            id.m_path = m.second->get_filename();
            ret->m_types[id] = info;
        }

        // TODO: union
    }
    return ret;
}

type_infer::type_infer(defun &fun, classenv &cenv, funcenv &fenv)
    : m_defun(fun), m_classenv(cenv), m_funcenv(fenv), m_de_bruijn_idx(0) {
    // initialize a storage for variable names
    m_block_ids.push_back(std::vector<std::string>());
    m_ids = m_block_ids.rbegin();

    // initialize assumption
    for (auto &assump : fun.m_assump) {
        auto id = gensym_for(assump.first);
        m_assump[id].push_back(assump.second);
    }

    for (const qual *q = &fun; q != nullptr; q = q->m_parent) {
        // initialize predicates
        for (auto &p : q->m_preds) {
            auto np = std::make_unique<pred>(*p);
            m_preds.push_back(std::move(np));
        }

        // initialize constraints of kind
        for (auto &t : q->m_tvar_constraint) {
            assert(t->m_subtype == type::TYPE_VAR);
            auto tv = (type_var *)t.get();
            assert(!HASKEY(m_tvar_constraint, tv->get_id()));
            m_tvar_constraint[tv->get_id()] = tv->get_kind();
        }
    }

    // type of this function
    m_type = fun.m_type;

    // type of return value
    m_ret = fun.m_ret;

    // initialize substitution
    m_sbst = std::make_unique<substitution>();

    // AST of function definition
    m_ast = (ast_defun *)fun.m_ast;
}

std::string type_infer::gensym_for(const std::string &var) {
    std::string ret = boost::lexical_cast<std::string>(m_de_bruijn_idx++);

    m_ids->push_back(ret);
    m_idx2name[ret] = var;
    m_name2idx[var].push_back(ret);

    return ret;
}

std::string type_infer::name2idx(const std::string &var) {
    auto it = m_name2idx.find(var);
    if (it == m_name2idx.end()) {
        return "";
    }

    return *it->second.rbegin();
}

void type_infer::pop_variables() {
    assert(!m_block_ids.empty());

    for (auto it = (*m_ids).rbegin(); it != (*m_ids).rend(); ++it) {
        // remove bindings of de Bruijn index to variable name
        auto name = m_idx2name[*it];
        m_idx2name.erase(name);

        // remove bindings of variable name to de Bruijn index
        auto &v = m_name2idx[name];
        v.pop_back();
        if (v.empty()) {
            m_name2idx.erase(name);
        }
    }

    m_block_ids.pop_back();
    m_ids = m_block_ids.rbegin();
}

bool typing(classenv &cenv, funcenv &fenv) {
    for (auto &fun : fenv.m_defuns) {
        type_infer ti(*(fun.second), cenv, fenv);
        if (!ti.typing())
            return false;
    }

    return true;
}

bool type_infer::typing() {
    shared_type t;
    ast_expr *e;
    for (auto &expr : m_ast->m_exprs->m_exprs) {
        t = typing(expr.get());
        if (!t)
            return false;
        e = expr.get();
    }

    auto s = mgu(t, m_ret);
    if (!s) {
        std::string msg = "type of return value is incompatible. expected \"" +
                          m_ret->to_str() + "\"";
        TYPEERR(msg.c_str(), m_defun.m_module, m_ast);

        msg = "actually returned \"" + t->to_str() + "\"";
        TYPEINFO(msg.c_str(), m_defun.m_module, e);
        return false;
    }

    m_sbst = compose(*s, *m_sbst);
    m_type = m_sbst->apply(m_type);

    m_defun.m_type->print();
    std::cout << std::endl;

    m_type->print();
    std::cout << std::endl;

    return true;
}

shared_type type_infer::typing(ast_expr *expr) {
    switch (expr->m_exprtype) {
    case ast_expr::EXPR_ID:
        return typing_id((ast_expr_id *)expr);
    case ast_expr::EXPR_BINEXPR:
        return typing_dotexpr((ast_binexpr *)expr);
    default:
        assert(false); // not yet implemented
        break;
    }

    return nullptr;
}

shared_type type_infer::typing_id(ast_expr_id *expr) {
    auto id = name2idx(expr->m_id->m_id);
    auto ret = m_assump.find(id);
    if (ret == m_assump.end()) {
        TYPEERR("undefined variable is used", m_defun.m_module, expr);
        return nullptr;
    }

    return *(ret->second.rbegin());
}

shared_type type_infer::typing_dotexpr(ast_binexpr *expr) {
    std::list<std::string> ids;

    // the right hand side must be ID
    if (expr->m_right->m_exprtype != ast_expr::EXPR_ID) {
        TYPEERR("must be identifier", m_defun.m_module, expr->m_right);
        return nullptr;
    }

    auto rhs = (ast_expr_id *)expr->m_right.get();
    ids.push_back(rhs->m_id->m_id);

    return typing_dotexpr(expr->m_right.get(), ids);
}

shared_type type_infer::typing_dotexpr(ast_expr *expr,
                                       std::list<std::string> &ids) {
    switch (expr->m_exprtype) {
    case ast_expr::EXPR_ID: {
        auto ast = (ast_expr_id *)expr;
        auto id = name2idx(ast->m_id->m_id);
        auto it = m_assump.find(id);
        if (it == m_assump.end()) {
            // not variable name
            ids.push_front(ast->m_id->m_id);

            // TODO: function or data creation
            // function type

            // struct, union, or primitive types
        }
    }
    case ast_expr::EXPR_BINEXPR: {
        auto bin = (ast_binexpr *)expr;

        // the right hand side must be ID
        if (bin->m_right->m_exprtype != ast_expr::EXPR_ID) {
            TYPEERR("must be identifier", m_defun.m_module, bin->m_right);
            return nullptr;
        }

        auto rhs = (ast_expr_id *)bin->m_right.get();
        ids.push_front(rhs->m_id->m_id);

        return typing_dotexpr(bin->m_left.get(), ids);
    }
    default:
        break;
    }

    auto lhs = typing(expr);
    if (!lhs)
        return nullptr;

    // TODO: resolve type in struct or union

    return nullptr;
}

std::string type_const::to_str() { return m_id.m_id; }

std::string type_var::to_str() { return m_id; }

std::string type_app::to_str() { return to_str(true); }

std::string type_app::to_str(bool is_top) {
    std::string sl;
    auto sr = m_right->to_str();

    std::string ret;
    switch (m_left->m_subtype) {
    case type::TYPE_CONST:
    case type::TYPE_VAR:
        sl = m_left->to_str();
        ret = sl + "<" + sr;
        break;
    case type::TYPE_APP: {
        auto tapp = (type_app *)m_left.get();
        sl = tapp->to_str(false);
        ret = sl + ", " + sr;
        break;
    }
    }

    if (is_top)
        ret += ">";

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
    std::cout << ",\"arg\":";
    m_arg->print();
    std::cout << "}";
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

    std::cout << ",\"methods\":[";
    int i = 0;
    for (auto &func : m_funcs) {
        if (i > 0)
            std::cout << ",";
        std::cout << "{\"id\":";
        func.first.print();
        std::cout << ",\"type\":";
        func.second->m_type->print();
        std::cout << "}";
        i++;
    }

    std::cout << "],\"args\":\"" << m_arg << "\",\"constraints\":[";

    int n = 0;
    for (auto &tv : m_tvar_constraint) {
        if (n > 0)
            std::cout << ",";
        tv->print();
        n++;
    }

    std::cout << "]}";
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
    std::cout << "{\"head\":";
    m_pred.print();
    std::cout << ",\"predicates\":";
    print_preds();
    std::cout << ",\"methods\":[";

    int n = 0;
    for (auto &fun : m_funcs) {
        if (n > 0)
            std::cout << ",";
        std::cout << "{\"id\":\"" << fun.first << "\",\"type\":";
        fun.second->m_type->print();
        std::cout << "}";
        n++;
    }

    std::cout << "]}";
}

void defun::print() {
    std::cout << "{\"type\":";
    m_type->print();

    std::cout << ",\"return\":";
    m_ret->print();

    int n = 0;
    std::cout << ",\"args\":[";
    for (auto &arg : m_args) {
        if (n > 0)
            std::cout << ",";
        n++;
        std::cout << "{\"id\":\"" << arg.first << "\",\"type\":";
        arg.second->print();
        std::cout << "}";
    }
    std::cout << "],\"assumption\":[";

    n = 0;
    for (auto &assump : m_assump) {
        if (n > 0)
            std::cout << ",";
        n++;
        std::cout << "{\"id\":\"" << assump.first << "\",\"type\":";
        assump.second->print();
        std::cout << "}";
    }

    std::cout << "]}";
}

void funcenv::print() {
    int n = 0;
    std::cout << "[";
    for (auto &f : m_defuns) {
        if (n > 0)
            std::cout << ",";
        n++;
        std::cout << "{\"id\":";
        f.first.print();
        std::cout << ",\"defun\":";
        f.second->print();

        std::cout << ",\"predicates\":";
        f.second->print_preds();

        std::cout << "}";
    }

    std::cout << "]";
}

void typeenv::print() {
    int n = 0;
    std::cout << "[";
    for (auto &tp : m_types) {
        if (n > 0)
            std::cout << ",";

        std::cout << "{\"id\":";
        tp.first.print();

        std::cout << ",\"type\":";
        tp.second->m_type.m_type->print();

        std::cout << ",\"predicates\":";
        tp.second->m_type.print_preds();

        std::cout << ",\"members\":[";

        int m = 0;
        for (auto &mem : tp.second->m_members.get<seq>()) {
            if (m > 0)
                std::cout << ",";
            std::cout << "{\"id\":\"" << mem.m_id << "\",\"type\":";
            mem.m_type->print();
            std::cout << "}";
            m++;
        }

        std::cout << "]}";

        n++;
    }
    std::cout << "]";
}

} // namespace lunar
