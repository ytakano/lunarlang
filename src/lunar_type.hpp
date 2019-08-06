#ifndef LUNAR_TYPE_HPP
#define LUNAR_TYPE_HPP

#include "lunar_common.hpp"
#include "lunar_parser.hpp"

#include <assert.h>

#include <map>
#include <memory>
#include <string>
#include <unordered_map>
#include <vector>

#include <boost/bimap/bimap.hpp>
#include <boost/bimap/unordered_set_of.hpp>

#include <boost/multi_index/hashed_index.hpp>
#include <boost/multi_index/member.hpp>
#include <boost/multi_index/random_access_index.hpp>
#include <boost/multi_index_container.hpp>

namespace lunar {

struct type_id {
    std::string m_path;
    std::string m_id;

    void print() const {
        std::cout << "{\"path\":\"" << m_path << "\",\"id\":\"" << m_id
                  << "\"}";
    }

    bool operator==(const type_id &rhs) const {
        return m_path == rhs.m_path && m_id == rhs.m_id;
    }

    bool operator!=(const type_id &rhs) const { return !(*this == rhs); }
};

} // namespace lunar

namespace std {

template <> struct std::hash<lunar::type_id> {
  public:
    size_t operator()(const lunar::type_id &data) const {
        std::size_t seed = 0;
        boost::hash_combine(seed, data.m_path);
        boost::hash_combine(seed, data.m_id);
        return seed;
    }
};

} // namespace std

namespace lunar {

struct ast;
struct ast_kind;
struct ast_type;
struct ast_class;
struct ast_instance;
struct ast_pred;
struct ast_defun;
struct ast_interface;
struct ast_expr;
struct ast_expr_id;
struct ast_binexpr;
struct ast_userdef;
class module;
class parser;
class pred;
class defun;

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

    static uint16_t num_args(const ast_kind *ptr);

    virtual void print() = 0;
    virtual std::string to_str() = 0;

    bool m_is_star;
};

typedef std::shared_ptr<kind> shared_kind;

// normal type
class star : public kind {
  public:
    star() { m_is_star = true; }
    virtual ~star() {}
    virtual void print() { std::cout << "*"; }
    virtual std::string to_str() { return "*"; }
};

typedef std::shared_ptr<star> shared_star;

// higher order type
class kfun : public kind {
  public:
    kfun() { m_is_star = false; }
    virtual ~kfun() {}

    virtual void print() {
        m_left->print();
        std::cout << " -> ";
        m_right->print();
    }

    virtual std::string to_str() {
        auto left = m_left->to_str();
        auto right = m_right->to_str();
        return left + " -> " + right;
    }

    shared_kind m_left;
    shared_kind m_right;
};

typedef std::shared_ptr<kfun> shared_kfun;

int cmp_kind(const kind *lhs, const kind *rhs);

// type
class type {
  public:
    type() {}
    virtual ~type(){};

    virtual void print() = 0;
    virtual std::string to_str() = 0;

    enum subtype {
        TYPE_CONST, // constant type
        TYPE_VAR,   // type variable
        TYPE_APP,   // higher order type application
    };

    subtype m_subtype;

    static std::shared_ptr<type> make(const module *ptr_mod,
                                      const ast_type *ptr);

    virtual shared_kind get_kind() const = 0;
};

typedef std::shared_ptr<type> shared_type;

bool eq_type(type *lhs, type *rhs);

// constant type
// e.g.
//   num : *
//   vec : * -> *
class type_const : public type {
  public:
    virtual ~type_const() {}
    virtual void print();
    virtual std::string to_str();

    bool operator==(const type_const &lhs) const {
        return m_id == lhs.m_id &&
               cmp_kind(m_kind.get(), lhs.m_kind.get()) == 0;
    }

    bool operator!=(const type_const &lhs) const { return !(*this == lhs); }

    virtual shared_kind get_kind() const { return m_kind; }
    const type_id &get_id() { return m_id; }

    // id: type name
    // numtargs: the number of type arguments
    static shared_type make(const type_id &id, unsigned int numtargs);

  protected:
    type_const() { m_subtype = TYPE_CONST; }
    type_id m_id; // identifier of type (e.g. num, bool)
    shared_kind m_kind;
};

typedef std::shared_ptr<type_const> shared_type_const;

// type variable
class type_var : public type {
  public:
    type_var(const std::string &id, shared_kind k) : m_id(id), m_kind(k) {
        m_subtype = TYPE_VAR;
    }
    virtual ~type_var() {}

    bool operator==(const type_var &lhs) const {
        return m_id == lhs.m_id &&
               cmp_kind(m_kind.get(), lhs.m_kind.get()) == 0;
    }

    bool operator!=(const type_var &lhs) const { return !(*this == lhs); }

    bool operator<(const type_var &lhs) const {
        int ret = m_id.compare(lhs.m_id);
        if (ret == 0)
            ret = cmp_kind(m_kind.get(), lhs.m_kind.get());
        return ret > 0 ? true : false;
    }

    virtual shared_kind get_kind() const { return m_kind; }
    const std::string &get_id() const { return m_id; }
    void set_id(const std::string &s) { m_id = s; }

    // id: type variable name
    static shared_type make(const std::string &id, unsigned int numtargs);

    // id: type variable name, k: kind
    static shared_type make(const std::string &id, shared_kind k) {
        auto ret = std::shared_ptr<type_var>(new type_var);
        ret->m_id = id;
        ret->m_kind = k;
        return ret;
    }

    virtual void print();
    virtual std::string to_str();

  private:
    type_var() { m_subtype = TYPE_VAR; }
    std::string m_id; // type variable name
    shared_kind m_kind;
};

typedef std::shared_ptr<type_var> shared_type_var;

// application of higher order type
// e.g. (1)
//   struct dict<T1, T2> {...} is denoted as * -> (* -> *)
//   dict<int> is denoted as
//     tapp1.left  = * -> (* -> *)
//     tapp1.right = *
//   dict<int, bool> is denoted as
//     tapp2.left  = tapp1
//     type2.right = *
//
// e.g. (2)
//   struct somedata<T1, T2, T3> {...}
//   somedata<int>
//     tapp1.left  = * -> (* -> (* -> *))
//     tapp1.right = *
//   somedata<int, int>
//     tapp2.left  = tapp1
//     tapp2.right = *
//   somedata<int, int, int>
//     tapp3.left  = tapp2
//     tapp3.right = *
class type_app : public type {
  public:
    virtual ~type_app() {}

    bool operator==(const type_app &lhs) const {
        return eq_type(m_left.get(), m_right.get()) &&
               eq_type(lhs.m_left.get(), lhs.m_right.get());
    }

    // e.g.
    // if
    //   left:  * -> (* -> *)
    //   right: *
    // then output
    //   * -> *
    virtual shared_kind get_kind() const {
        auto k = m_left->get_kind();
        assert(!k->m_is_star);

        auto kf = std::static_pointer_cast<kfun>(k);

        return kf->m_right;
    }
    static shared_type make(shared_type lhs, shared_type rhs);

    // require
    //   m_left->get_kind()->m_left == m_right->get_kind()
    //   m_right->get_kind() == *
    shared_type m_left;
    shared_type m_right;

    virtual void print();
    virtual std::string to_str();
    std::string to_str(bool is_top);

  private:
    type_app() { m_subtype = TYPE_APP; }
};

typedef std::unique_ptr<pred> uniq_pred;

// substitution from (id, kind) to type
// type variable -> type
// e.g.
//   substitution:
//     {(`a : *) -> int}
//   apply:
//     {(`a : *) -> int} `a : * -> int
//   composition:
//     {`b : * -> int} {(`a : *) -> vec (`b : *)} -> {(`a : *) -> vec int}
class substitution {
  public:
    substitution() {}
    virtual ~substitution() {}

    // substitute type variables in the argument
    shared_type apply(shared_type type);
    uniq_pred apply(pred *p);

    std::map<type_var, shared_type> m_subst;
};

typedef std::unique_ptr<substitution> uniq_subst;

// a predicate asserts that m_types are members of a class named m_id
// e.g.
//   num<`a>: class name = num, m_args = [`a]
//   ClassA<`a, `b>: class name = ClassA, m_args = [`a, `b]
class pred {
  public:
    pred() {}
    virtual ~pred() {}

    void print();
    std::string to_str() { return m_id.m_id + "<" + m_arg->to_str() + ">"; }

    static uniq_pred make(const module *ptr_mod, const ast_pred *ptr);

    bool operator==(const pred &rhs) const {
        if (m_id != rhs.m_id)
            return false;

        return eq_type(m_arg.get(), rhs.m_arg.get());
    }

    bool in_hnf() { return hnf(m_arg.get()); }

    bool is_head_var(const std::string &arg) {
        return head_var(m_arg.get(), arg);
    }

    type_id m_id; // class name_id
    shared_type m_arg;

  private:
    static bool hnf(type *p) {
        switch (p->m_subtype) {
        case type::TYPE_VAR:
            return true;
        case type::TYPE_APP: {
            auto ap = (type_app *)p;
            return hnf(ap->m_left.get());
        }
        case type::TYPE_CONST:
            return false;
        }
    }

    static bool head_var(type *p, const std::string &arg) {
        switch (p->m_subtype) {
        case type::TYPE_VAR: {
            auto tv = (type_var *)p;
            if (arg == tv->get_id())
                return true;
            else
                return false;
        }
        case type::TYPE_APP: {
            auto ap = (type_app *)p;
            return head_var(ap->m_left.get(), arg);
        }
        case type::TYPE_CONST:
            return false;
        }
    }
};

typedef boost::bimaps::bimap<boost::bimaps::unordered_set_of<std::string>,
                             boost::bimaps::unordered_set_of<std::string>>
    bimap_ss;

// qualified type
// e.g.
//   qualified class declaration:
//     class ord<`a> require eq<`a>
//   qualified class instance:
//     inst ord<either `a> require ord<'a>
//   qualified type declaration:
//     fn myfun (x : `a, y : `b) : `a require num<`a>, bool<`b>
//     struct data<`a, `b> require num<`a>, bool<`b>
class qual {
  public:
    qual() : m_parent(nullptr) {}
    virtual ~qual() {}

    void print_preds();

    std::vector<uniq_pred> m_preds; // require

    // de Bruijn index <-> original type variable
    bimap_ss m_idx_tvar;

    const ast *m_ast;
    const module *m_module;
    const qual *m_parent;

    std::vector<shared_type> m_tvar_constraint; // type variable constraint

    bool check_kind_constraint(const std::string &id, kind *k, bool &found);
    bool add_constraints(pred *p);
    bool add_constraints(type *p);
    std::string

    // find oritinal type variable from de Bruijn index
    // id: de Bruijn index
    // if found this returns original type variable name,
    // otherwise returns ""
    find_tvar_idx(const std::string &id) const;
};

// qualified type
// function, struct, or union types are denoted by this class
class qual_type : public qual {
  public:
    qual_type() {}
    virtual ~qual_type() {}

    shared_type m_type;
};

typedef std::unique_ptr<qual_type> ptr_qual_type;

// class declaration
class typeclass : public qual {
  public:
    enum ASYCLIC {
        ASYCLIC_NONE,
        ASYCLIC_YES,
        ASYCLIC_NO,
    };
    typeclass() : m_is_asyclic(ASYCLIC_NONE) {}
    virtual ~typeclass() {}

    void print();

    type_id m_id; // class name

    std::string m_arg;                                  // arguments
    std::unordered_map<type_id, ptr_qual_type> m_funcs; // interfaces

    ASYCLIC m_is_asyclic;

    bool apply_super(shared_type arg, std::vector<uniq_pred> &ret,
                     const module *ptr_mod, const ast *ptr_ast);
    bool add_interface(const module *ptr_mod, const std::string &id,
                       ast_interface *ptr_ast);

  private:
    shared_type make_funtype(const module *ptr_mod, ast_interface *ptr_ast);
};

typedef std::unique_ptr<typeclass> uniq_typeclass;

class defun : public qual {
  public:
    defun() {}
    virtual ~defun() {}

    void print();

    static std::unique_ptr<defun>
    make(const module *ptr_mod, const qual *parent, const ast_defun *ast);

    shared_type m_type;
    std::vector<std::pair<std::string, shared_type>> m_args;
    shared_type m_ret;
    std::unordered_map<std::string, shared_type> m_assump;
};

typedef std::unique_ptr<defun> ptr_defun;

// class instance declaration
class inst : public qual {
  public:
    inst() {}
    virtual ~inst() {}

    void print();

    pred m_pred;
    std::unordered_map<std::string, ptr_defun> m_funcs; // interfaces
};

typedef std::unique_ptr<inst> uniq_inst;

class classenv {
  public:
    classenv() {}
    virtual ~classenv() {}

    static std::unique_ptr<classenv> make(const parser &ps);

    void print();

  private:
    struct env {
        uniq_typeclass m_class; // class declaration

        // multiply defined instances are prohibited
        // 0 <= i, j < m_insts.size()
        // s: substitution (mgu)
        // ∀i ∀j ∃s (s m_insts[i].m_args = s m_insts[j].m_args) -> error
        std::vector<uniq_inst> m_insts; // instance declarations
    };

    typedef std::unique_ptr<env> ptr_env;

    // class name -> (class declarations, class instance declarations)
    std::unordered_map<type_id, ptr_env> m_env;

    // function name -> class name
    std::unordered_map<type_id, std::string> m_func2class;

    bool add_class(const module *ptr_mod, const ast_class *ptr);
    bool add_instance(const module *ptr_mod, const ast_instance *ptr_ast);
    bool is_inherit_instance(const pred &p, const module *ptr_mod,
                             const ast *ptr_ast);
    inst *overlap(pred &ptr);
    bool is_asyclic();
    bool is_asyclic(const module *ptr_mod, typeclass *ptr,
                    std::unordered_set<type_id> &visited);

    bool by_super(pred *pd, std::vector<uniq_pred> &ret);
    void by_inst(pred *pd, std::vector<uniq_pred> &ret);
    TRIVAL entail(std::vector<uniq_pred> &ps, pred *pd);
    bool to_hnfs(std::vector<uniq_pred> &ps, std::vector<uniq_pred> &ret,
                 int &idx);
    bool to_hnf(uniq_pred pd, std::vector<uniq_pred> &ret);
    bool simplify(std::vector<uniq_pred> &ps);
    bool reduce(std::vector<uniq_pred> &ps, int &idx);
    bool check_ifs_type(); // check type of interfaces
    uniq_subst mgu_if_type(typeclass *cls, inst *in, const std::string &id,
                           defun *qt);
};

class funcenv {
  public:
    funcenv() {}
    virtual ~funcenv() {}

    void print();

    static std::unique_ptr<funcenv> make(const parser &ps);

    friend class type_infer;
    friend bool typing(classenv &cenv, funcenv &fenv);

  private:
    std::unordered_map<type_id, ptr_defun> m_defuns;
};

namespace mi = boost::multi_index;

class typeenv {
  public:
    typeenv() {}
    virtual ~typeenv() {}

    void print();

    static std::unique_ptr<typeenv> make(const parser &ps);

    friend class type_infer;
    friend bool typing(classenv &cenv, funcenv &fenv);

    struct memv {
        std::string m_id;
        shared_type m_type;

        memv(const std::string &id, shared_type t) : m_id(id), m_type(t) {}
    };

  private:
    struct hashmap {};
    struct seq {};

    typedef mi::multi_index_container<
        memv, mi::indexed_by<
                  mi::hashed_unique<mi::tag<hashmap>,
                                    mi::member<memv, std::string, &memv::m_id>>,
                  mi::random_access<mi::tag<seq>>>>
        dict_mem;

    struct typeinfo {
        qual_type m_type;
        std::vector<shared_type> m_args;
        dict_mem m_members;
        bool m_is_struct;
        const ast_type *m_ast;
        const module *m_module;
    };

    static bool check_tvar(type *ptr,
                           const std::unordered_set<std::string> &tvars);
    static std::shared_ptr<typeinfo> make_typeinfo(const module *ptr_mod,
                                                   const ast_userdef *ptr_ast);

    std::unordered_map<type_id, std::shared_ptr<typeinfo>> m_types;
};

class type_infer {
  public:
    type_infer(defun &fun, classenv &cenv, funcenv &fenv);
    virtual ~type_infer() {}

    bool typing();

  private:
    defun &m_defun;
    classenv &m_classenv;
    funcenv &m_funcenv;
    shared_type m_type;
    shared_type m_ret;
    std::vector<uniq_pred> m_preds;
    uniq_subst m_sbst;
    ast_defun *m_ast;
    uint64_t m_de_bruijn_idx;

    // assumpsion: variable names to types
    std::unordered_map<std::string, std::vector<shared_type>> m_assump;

    // constraints of kind
    std::unordered_map<std::string, shared_kind> m_tvar_constraint;

    // variable names in function
    // storead variable name as de Bruijn index
    std::vector<std::vector<std::string>> m_block_ids;

    // variable names in current scope
    // storead variable name as de Bruijn index
    std::vector<std::vector<std::string>>::reverse_iterator m_ids;

    // de Bruijn index to original variable name
    std::unordered_map<std::string, std::string> m_idx2name;

    // original variable name to de Bruijn index
    std::unordered_map<std::string, std::vector<std::string>> m_name2idx;

    shared_type typing(ast_expr *expr);
    shared_type typing_id(ast_expr_id *expr);
    shared_type typing_dotexpr(ast_binexpr *expr);
    shared_type typing_dotexpr(ast_expr *expr, std::list<std::string> &ids);

    // generate de Bruijn index for var, and returned the generated index
    std::string gensym_for(const std::string &var);

    // return de Bruijn index of var
    // if no variable name is found, return ""
    std::string name2idx(const std::string &var);

    void pop_variables();
};

bool typing(classenv &cenv, funcenv &fenv);

} // namespace lunar

#endif // LUNAR_TYPE_HPP