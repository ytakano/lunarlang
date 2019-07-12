#include "lunar_type.hpp"
#include "lunar_print.hpp"

#include <assert.h>

#include <boost/lexical_cast.hpp>

#define TYPEERR(MSG, M, AST)                                                   \
    do {                                                                       \
        fprintf(stderr, "%s:%lu:%lu:(%d) error: " MSG "\n",                    \
                (M)->get_filename().c_str(), (AST)->m_line, (AST)->m_column,   \
                __LINE__);                                                     \
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

            if (t->m_args) {
                auto ret = type_const::make(id, t->m_args->m_types.size());
                APP_TYPES(ret, t->m_args->m_types);
                return ret;
            } else {
                return type_const::make(id, 0);
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

    if (occurs_check(*tv1, ty.get()))
        return nullptr;

    if (cmp_kind(var->get_kind().get(), ty->get_kind().get()) != 0)
        return nullptr;

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
        if (*((type_const *)lhs.get()) != *((type_const *)rhs.get())) {
            return nullptr;
        }
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

    return nullptr;
}

shared_subst mgu_pred(pred *lhs, pred *rhs) {
    if (lhs->m_id != rhs->m_id || lhs->m_args.size() != rhs->m_args.size())
        return nullptr;

    auto s1 = mgu(lhs->m_args[0], rhs->m_args[0]);
    if (!s1)
        return nullptr;

    for (int i = 1; i < lhs->m_args.size(); i++) {
        auto s2 = mgu(s1->apply(lhs->m_args[i]), s1->apply(rhs->m_args[i]));
        if (!s2)
            return nullptr;
        s1 = compose(*s2, *s1);
    }

    return s1;
}

shared_subst match_pred(pred *lhs, pred *rhs) {
    if (lhs->m_id != rhs->m_id || lhs->m_args.size() != rhs->m_args.size())
        return nullptr;

    auto s1 = std::make_shared<substitution>();
    for (int i = 0; i < lhs->m_args.size(); i++) {
        auto lapp = std::static_pointer_cast<type_app>(lhs->m_args[i]);
        auto rapp = std::static_pointer_cast<type_app>(rhs->m_args[i]);

        auto sl = match(lapp->m_left, rapp->m_left);
        if (!sl)
            return nullptr;

        auto sr = match(lapp->m_right, rapp->m_right);
        if (!sr)
            return nullptr;

        auto s2 = merge(*sl, *sr);
        if (!s2)
            return nullptr;

        s1 = merge(*s2, *s1);
    }

    return s1;
}

shared_pred pred::make(const module *ptr_mod, const ast_pred *ptr) {
    auto ret = std::make_shared<pred>();
    if (!ptr_mod->find_typeclass(ptr->m_id.get(), ret->m_id.m_path,
                                 ret->m_id.m_id)) {
        // error, no such type class
        TYPEERR("undefined type class", ptr_mod, ptr);
        return nullptr;
    }

    for (auto &arg : ptr->m_args->m_types) {
        auto ta = type::make(ptr_mod, arg.get());
        if (!ta)
            return nullptr;

        ret->m_args.push_back(ta);
    }

    return ret;
}

bool classenv::add_class(const module *ptr_mod, const ast_class *ptr) {
    auto cls = std::make_shared<typeclass>();

    cls->m_id.m_path = ptr_mod->get_filename();
    cls->m_id.m_id = ptr->m_id->m_id;

    if (ptr->m_preds) {
        for (auto &p : ptr->m_preds->m_preds) {
            auto pd = pred::make(ptr_mod, p.get());
            if (!pd)
                return false;

            cls->m_preds.push_back(pd);
        }
    }

    std::unordered_set<std::string> s;
    for (auto &tv : ptr->m_tvars->m_args) {
        if (HASKEY(s, tv->m_id->m_id)) {
            TYPEERR("argument name is multiply used", ptr_mod, tv->m_id);
            return false;
        }

        cls->m_args.push_back(tv->m_id->m_id);
        s.insert(tv->m_id->m_id);

        if (tv->m_kind) {
            int n = 0;
            for (auto k = tv->m_kind.get(); k->m_asttype != ast::AST_KSTAR;
                 k = ((ast_kfun *)k)->m_right.get()) {
                assert(k->m_asttype == ast::AST_KFUN);
                assert(((ast_kfun *)k)->m_right->m_asttype == ast::AST_KSTAR);
                n++;
            }
            auto v = type_var::make(tv->m_id->m_id, n);
            cls->m_tvar_constraint.push_back(v);
        }
    }

    // TODO: add arguments and functions

    if (HASKEY(m_env, cls->m_id)) {
        TYPEERR("multiply defined class", ptr_mod, ptr);
        return false;
    }

    cls->m_ast = ptr;
    cls->m_module = ptr_mod;

    auto e = std::make_unique<env>();
    e->m_class = cls;
    m_env[cls->m_id] = std::move(e);

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

bool classenv::add_instance(const module *ptr_mod, const ast_instance *ptr) {
    auto ret = std::make_shared<inst>();
    if (!ptr_mod->find_typeclass(ptr->m_arg->m_id.get(),
                                 ret->m_pred.m_id.m_path,
                                 ret->m_pred.m_id.m_id)) {
        // error, no such type class
        TYPEERR("undefined type class", ptr_mod, ptr->m_arg->m_id);
        return false;
    }

    auto cls = m_env.find(ret->m_pred.m_id);
    assert(cls != m_env.end());

    for (auto &arg : ptr->m_arg->m_args->m_types) {
        auto ta = type::make(ptr_mod, arg.get());
        if (!ta)
            return false;
        ret->m_pred.m_args.push_back(ta);
    }

    auto is = overlap(ret->m_pred);
    if (is) {
        TYPEERR2("instance declarations are overlapped", ptr_mod, is->m_module,
                 ptr->m_arg, ((ast_instance *)is->m_ast)->m_arg);
        return false;
    }

    if (ptr->m_req) {
        for (auto &p : ptr->m_req->m_preds) {
            auto pd = pred::make(ptr_mod, p.get());
            if (!pd)
                return false;
            ret->m_preds.push_back(pd);
        }
    }

    ret->m_ast = ptr;
    ret->m_module = ptr_mod;

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

    if (!ret->is_asyclic())
        return nullptr;

    ret->de_bruijn();

    return ret;
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

std::string classenv::gensym() {
    return "T" + boost::lexical_cast<std::string>(m_de_bruijn_idx++);
}

void classenv::de_bruijn() {
    for (auto &it : m_env) {
        de_bruijn(it.second->m_class.get());
        for (auto &p : it.second->m_insts) {
            de_bruijn(p.get());
        }
    }
}

void classenv::de_bruijn(typeclass *ptr) {
    for (auto &arg : ptr->m_args) {
        std::string tmp = arg;
        arg = gensym();
        ptr->m_idx_tvar.insert(bimap_ss::value_type(arg, tmp));
    }

    for (auto &tv : ptr->m_tvar_constraint) {
        assert(tv->m_subtype == type::TYPE_VAR);
        auto t = (type_var *)tv.get();
        auto it = ptr->m_idx_tvar.right.find(t->get_id());
        if (it != ptr->m_idx_tvar.right.end())
            t->set_id(it->second);
    }

    for (auto &p : ptr->m_preds) {
        for (auto &t : p->m_args) {
            de_bruijn(ptr, t.get());
        }
    }
}

void classenv::de_bruijn(inst *ptr) {
    for (auto &t : ptr->m_pred.m_args) {
        de_bruijn(ptr, t.get());
    }

    for (auto &p : ptr->m_preds) {
        for (auto &t : p->m_args) {
            de_bruijn(ptr, t.get());
        }
    }
}

void classenv::de_bruijn(qual *ptr, type *ptr_type) {
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
        auto it = ptr->m_idx_tvar.right.find(id);
        if (it == ptr->m_idx_tvar.right.end()) {
            var->set_id(gensym());
            ptr->m_idx_tvar.insert(bimap_ss::value_type(var->get_id(), id));
        } else {
            var->set_id(it->second);
        }
        break;
    }
    case type::TYPE_CONST:
        break;
    }
}

bool classenv::by_super(pred *pd, std::vector<std::unique_ptr<pred>> &ret) {
    auto p = std::make_unique<pred>(*pd);
    ret.push_back(std::move(p));

    auto it = m_env.find(pd->m_id);
    assert(it != m_env.end());

    if (pd->m_args.size() != it->second->m_class->m_args.size()) {
        // TODO: print error
        return false;
    }

    // get variable bindings
    substitution sbst;
    for (int i = 0; i < pd->m_args.size(); i++) {
        auto const &id = it->second->m_class->m_args[i];
        auto const k = pd->m_args[i]->get_kind();
        // check wheter the kind of the argument satsfies the constraints
        for (auto &tv : it->second->m_class->m_tvar_constraint) {
            if (id == ((type_var *)tv.get())->get_id() &&
                cmp_kind(k.get(), tv->get_kind().get()) != 0) {
                // TODO: print error
                return false;
            }
        }

        type_var tv(id, k);
        sbst.m_subst[tv] = pd->m_args[i];
    }

    for (auto &s : it->second->m_class->m_preds) { // get super classes
        // apply the substitution to the super classes
        pred sup;
        sup.m_id = s->m_id;
        for (auto &t : s->m_args) {
            sup.m_args.push_back(sbst.apply(t));
        }

        by_super(&sup, ret);
    }

    return true;
}

// find the instance of a predicate, and return predicates required
// by the instance
void classenv::by_inst(pred *pd, std::vector<std::unique_ptr<pred>> &ret) {
    auto it = m_env.find(pd->m_id);
    assert(it != m_env.end());

    for (auto &is : it->second->m_insts) {
        auto sbst = match_pred(&is->m_pred, pd);
        if (sbst != nullptr) {
            for (auto &sp : is->m_preds) {
                auto p = std::make_unique<pred>();
                p->m_id = pd->m_id;
                for (auto &arg : sp->m_args) {
                    p->m_args.push_back(sbst->apply(arg));
                }
                ret.push_back(std::move(p));
            }
        }
        return;
    }

    assert(false); // never reach here
}

TRIVAL classenv::entail(std::vector<std::unique_ptr<pred>> &ps, pred *p) {
    {
        for (auto &p0 : ps) {
            std::vector<std::unique_ptr<pred>> super;
            if (p0 && !by_super(p0.get, super))
                return TRI_FAIL;

            for (auto &s : super) {
                if (*s == *p)
                    return TRI_TRUE;
            }
        }
    }

    std::vector<std::unique_ptr<pred>> qs;
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

bool classenv::to_hnfs(std::vector<std::unique_ptr<pred>> &ps,
                       std::vector<std::unique_ptr<pred>> &ret) {
    for (auto &p : ps) {
        if (!to_hnf(std::move(p), ret))
            return false;
    }
    return true;
}

bool classenv::to_hnf(std::unique_ptr<pred> pd,
                      std::vector<std::unique_ptr<pred>> &ret) {
    if (pd->in_hnf()) {
        ret.push_back(std::move(pd));
        return true;
    }

    std::vector<std::unique_ptr<pred>> ps;
    by_inst(pd.get(), ps);
    if (ps.empty()) {
        // TODO: print error, "context reduction"
        return false;
    }

    return to_hnfs(ps, ret);
}

bool classenv::simplify(std::vector<std::unique_ptr<pred>> &ps) {

    for (int i = 0; i < ps.size(); i++) {
        assert(ps[i]);
        auto tmp = std::move(ps[i]);
        switch (entail(ps, tmp.get())) {
        case TRI_TRUE:
            continue;
        case TRI_FALSE:
            ps[i] = std::move(tmp);
            break;
        case TRI_FAIL:
            return false;
        }
    }

    return true;
}

bool classenv::reduce(std::vector<std::unique_ptr<pred>> &ps,
                      std::vector<std::unique_ptr<pred>> &ret) {
    if (!to_hnfs(ps, ret))
        return false;

    return simplify(ret);
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

    std::cout << ",\"args\":[";
    int n = 0;
    for (auto &arg : m_args) {
        if (n > 0)
            std::cout << ",";
        std::cout << "\"" << arg << "\"";
        n++;
    }

    std::cout << "],\"constraints\":[";
    n = 0;
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
    std::cout << "}";
}

void typing(ptr_ast_type &ast) {}

} // namespace lunar
