#ifndef LUNAR_PARSEC_HPP
#define LUNAR_PARSEC_HPP

#include <string>
#include <unordered_set>

#define PMANY(P, RET, X)                                                       \
    for (;;) {                                                                 \
        P.checkpoint();                                                        \
        auto r = X;                                                            \
        if (P.is_fail()) {                                                     \
            P.revert();                                                        \
            break;                                                             \
        }                                                                      \
        RET.push_back(r);                                                      \
        P.set_fail(false);                                                     \
    }

#define PTRY(P, RET, X)                                                        \
    do {                                                                       \
        P.checkpoint();                                                        \
        RET = X;                                                               \
        if (P.is_fail())                                                       \
            P.revert();                                                        \
    } while (0);

namespace lunar {
class parsec {
  public:
    parsec(const std::string &str) : m_str(str), m_pos(0), m_fail(false) {
        m_spaces.insert(' ');
        m_spaces.insert('\t');
        m_spaces.insert('\r');
        m_spaces.insert('\n');
    }
    virtual ~parsec() {}

    std::string str(std::string match) {
        std::string s;

        auto t = m_str.size() - m_pos;
        if (match.size() > t) {
            m_fail = true;
            return "";
        }

        for (int i = 0; i < match.size(); i++) {
            if (m_str[m_pos] != match[i]) {
                m_fail = true;
                return "";
            }
            m_pos++;
        }

        m_fail = false;
        return match;
    }

    char character(char c) {
        if (m_pos >= m_str.size() || m_str[m_pos] != c) {
            m_fail = true;
            return 0;
        }

        m_pos++;
        m_fail = false;
        return c;
    }

    char oneof(const std::unordered_set<char> &c) {
        if (m_pos == m_str.size()) {
            m_fail = true;
            return 0;
        }

        if (c.find(m_str[m_pos]) == c.end()) {
            m_fail = true;
            return 0;
        }

        m_pos++;
        m_fail = false;
        return m_str[m_pos];
    }

    char oneof_not(const std::unordered_set<char> &c) {
        if (m_pos == m_str.size()) {
            m_fail = true;
            return 0;
        }

        if (c.find(m_str[m_pos]) != c.end()) {
            m_fail = true;
            return 0;
        }

        m_pos++;
        m_fail = false;
        return m_str[m_pos];
    }

    char space() { return oneof(m_spaces); }

    std::string spaces() {
        std::string s;
        PMANY((*this), s, space());
        return s;
    }

    void checkpoint() { m_checkpoint.m_pos = m_pos; }
    void revert() { m_pos = m_checkpoint.m_pos; }

    bool is_fail() { return m_fail; }
    void set_fail(bool is_fail) { m_fail = is_fail; }
    bool is_eof() { return m_pos == m_str.size(); }

  private:
    struct checkpoint_t {
        std::size_t m_pos;
    } m_checkpoint;

    const std::string &m_str;
    std::size_t m_pos;
    bool m_fail;

    std::unordered_set<char> m_spaces;
};

} // namespace lunar

#endif // LUNAR_PARSEC_HPP