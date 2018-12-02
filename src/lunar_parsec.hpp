#ifndef LUNAR_PARSEC_HPP
#define LUNAR_PARSEC_HPP

#include <string>
#include <unordered_set>

#define PMANY(P, RET, X)                                                       \
    for (;;) {                                                                 \
        P.checkpoint();                                                        \
        auto r = X;                                                            \
        RET.push_back(r);                                                      \
        if (P.is_fail()) {                                                     \
            P.revert();                                                        \
            break;                                                             \
        }                                                                      \
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
    parsec(const std::string &str) : m_str(str), m_pos(0), m_fail(false) {}
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
        return match;
    }

    char chara(std::unordered_set<char> c) {
        if (m_pos == m_str.size()) {
            m_fail = true;
            return 0;
        }

        if (c.find(m_str[m_pos]) == c.end()) {
            m_fail = true;
            return 0;
        } else {
            m_pos++;
            return m_str[m_pos];
        }
    }

    char not_char(std::unordered_set<char> c) {
        if (m_pos == m_str.size()) {
            m_fail = true;
            return 0;
        }

        if (c.find(m_str[m_pos]) != c.end()) {
            m_fail = true;
            return 0;
        } else {
            m_pos++;
            return m_str[m_pos];
        }
    }

    void checkpoint() { m_checkpoint = m_pos; }
    void revert() { m_pos = m_checkpoint; }

    bool is_fail() { return m_fail; }
    bool is_eof() { return m_pos == m_str.size(); }

  private:
    const std::string &m_str;
    std::size_t m_pos;
    bool m_fail;
    std::size_t m_checkpoint;
};

} // namespace lunar

#endif // LUNAR_PARSEC_HPP