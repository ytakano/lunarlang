#ifndef LUNAR_PARSEC_HPP
#define LUNAR_PARSEC_HPP

#include <iostream>
#include <string>
#include <unordered_set>

#define PMANY(P, RET, X)                                                       \
    for (;;) {                                                                 \
        P.checkpoint();                                                        \
        auto r = X;                                                            \
        if (P.is_fail()) {                                                     \
            P.revert();                                                        \
            P.set_fail(false);                                                 \
            break;                                                             \
        }                                                                      \
        RET.push_back(r);                                                      \
    }

#define PMANYMV(P, RET, X)                                                     \
    for (;;) {                                                                 \
        P.checkpoint();                                                        \
        auto r = X;                                                            \
        if (P.is_fail()) {                                                     \
            P.revert();                                                        \
            P.set_fail(false);                                                 \
            break;                                                             \
        }                                                                      \
        RET.push_back(std::move(r));                                           \
    }

#define PTRY(P, RET, X)                                                        \
    do {                                                                       \
        P.checkpoint();                                                        \
        RET = X;                                                               \
        if (P.is_fail())                                                       \
            P.revert();                                                        \
    } while (0);

#define PTRYMV(P, RET, X)                                                      \
    do {                                                                       \
        P.checkpoint();                                                        \
        RET = std::move(X);                                                    \
        if (P.is_fail())                                                       \
            P.revert();                                                        \
    } while (0);

namespace lunar {
class parsec {
  public:
    parsec(const std::string &str)
        : m_str(str), m_pos(0), m_line(0), m_column(0), m_fail(false) {
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
            increment();
        }

        m_fail = false;
        return match;
    }

    char character(char c) {
        if (m_pos >= m_str.size() || m_str[m_pos] != c) {
            m_fail = true;
            return 0;
        }

        increment();
        m_fail = false;
        return c;
    }

    char oneof(const std::unordered_set<char> &c) {
        if (m_pos >= m_str.size()) {
            m_fail = true;
            return 0;
        }

        char ret = m_str[m_pos];
        if (c.find(ret) == c.end()) {
            m_fail = true;
            return 0;
        }

        increment();
        m_fail = false;
        return ret;
    }

    char oneof_not(const std::unordered_set<char> &c) {
        if (m_pos >= m_str.size()) {
            m_fail = true;
            return 0;
        }

        char ret = m_str[m_pos];
        if (c.find(ret) != c.end()) {
            m_fail = true;
            return 0;
        }

        increment();
        m_fail = false;
        return ret;
    }

    char space() { return oneof(m_spaces); }

    std::string spaces() {
        std::string s;
        PMANY((*this), s, space());
        return s;
    }

    void increment() {
        if (m_str[m_pos] == '\n') {
            m_line++;
            m_column = 0;
        }
        m_column++;
        m_pos++;
    }

    void checkpoint() {
        m_checkpoint.m_pos = m_pos;
        m_checkpoint.m_line = m_line;
        m_checkpoint.m_column = m_column;
    }

    void revert() {
        m_pos = m_checkpoint.m_pos;
        m_line = m_checkpoint.m_line;
        m_column = m_checkpoint.m_column;
    }

    bool is_fail() { return m_fail; }
    void set_fail(bool is_fail) { m_fail = is_fail; }
    bool is_eof() { return m_pos == m_str.size(); }
    std::size_t get_line() { return m_line; }
    std::size_t get_column() { return m_column; }

  private:
    struct checkpoint_t {
        std::size_t m_pos;
        std::size_t m_line;
        std::size_t m_column;
    } m_checkpoint;

    std::string m_str;
    std::size_t m_pos;
    std::size_t m_line;
    std::size_t m_column;
    bool m_fail;

    std::unordered_set<char> m_spaces;
};

} // namespace lunar

#endif // LUNAR_PARSEC_HPP