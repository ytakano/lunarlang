#ifndef LUNAR_PARSEC_HPP
#define LUNAR_PARSEC_HPP

#include <functional>
#include <iostream>
#include <string>
#include <unordered_set>

#define PMANY(P, RET, X)                                                       \
    for (;;) {                                                                 \
        std::size_t pos, line, column;                                         \
        (P).checkpoint(pos, line, column);                                     \
        auto r = X;                                                            \
        if ((P).is_fail()) {                                                   \
            (P).revert(pos, line, column);                                     \
            (P).set_fail(false);                                               \
            break;                                                             \
        }                                                                      \
        (RET).push_back(r);                                                    \
    }

#define PMANYONE(P, RET, X)                                                    \
    auto r = X;                                                                \
    if (!(P).is_fail()) {                                                      \
        (RET).push_back(r);                                                    \
        for (;;) {                                                             \
            std::size_t pos, line, column;                                     \
            (P).checkpoint(pos, line, column);                                 \
            r = X;                                                             \
            if ((P).is_fail()) {                                               \
                (P).revert(pos, line, column);                                 \
                (P).set_fail(false);                                           \
                break;                                                         \
            }                                                                  \
            (RET).push_back(r);                                                \
        }                                                                      \
    }

#define PMANYMV(P, RET, X)                                                     \
    for (;;) {                                                                 \
        std::size_t pos, line, column;                                         \
        (P).checkpoint(pos, line, column);                                     \
        auto r = X;                                                            \
        if ((P).is_fail()) {                                                   \
            (P).revert(pos, line, column);                                     \
            (P).set_fail(false);                                               \
            break;                                                             \
        }                                                                      \
        (RET).push_back(std::move(r));                                         \
    }

#define PTRY(P, RET, X)                                                        \
    do {                                                                       \
        std::size_t pos, line, column;                                         \
        (P).checkpoint(pos, line, column);                                     \
        RET = X;                                                               \
        if ((P).is_fail())                                                     \
            (P).revert(pos, line, column);                                     \
    } while (0);

#define PTRYMV(P, RET, X)                                                      \
    do {                                                                       \
        std::size_t pos, line, column;                                         \
        (P).checkpoint(pos, line, column);                                     \
        RET = std::move(X);                                                    \
        if ((P).is_fail())                                                     \
            (P).revert(pos, line, column);                                     \
    } while (0);

namespace lunar {
class parsec {
  public:
    parsec(const std::string &str)
        : m_str(str), m_pos(0), m_line(1), m_column(1), m_fail(false) {
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

        for (size_t i = 0; i < match.size(); i++) {
            if (m_str[m_pos] != match[i]) {
                m_fail = true;
                return "";
            }
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

    char satisfy(std::function<bool(char)> fun) {
        if (m_pos >= m_str.size()) {
            m_fail = true;
            return 0;
        }

        char ret = m_str[m_pos];
        if (!fun(ret)) {
            m_fail = true;
            return 0;
        }

        increment();
        m_fail = false;
        return ret;
    }

    char any() {
        if (m_pos >= m_str.size()) {
            m_fail = true;
            return 0;
        }

        char ret = m_str[m_pos];
        increment();
        m_fail = false;
        return ret;
    }

    char hex() {
        return satisfy([&](char c) {
            return ('0' <= c && c <= '9') || ('a' <= c && c <= 'f') ||
                   ('A' <= c && c <= 'F');
        });
    }

    char space() { return oneof(m_spaces); }

    char peek() {
        if (m_pos >= m_str.size()) {
            m_fail = true;
            return '\0';
        }

        m_fail = false;
        return m_str[m_pos];
    }

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

    void checkpoint(std::size_t &pos, std::size_t &line, std::size_t &column) {
        pos = m_pos;
        line = m_line;
        column = m_column;
    }

    void revert(std::size_t pos, std::size_t line, std::size_t column) {
        m_pos = pos;
        m_line = line;
        m_column = column;
    }

    bool is_fail() { return m_fail; }
    void set_fail(bool is_fail) { m_fail = is_fail; }
    bool is_eof() { return m_pos == m_str.size(); }
    std::size_t get_line() const { return m_line; }
    std::size_t get_column() const { return m_column; }
    const std::string &get_str() const { return m_str; }

  private:
    std::string m_str;
    std::size_t m_pos;
    std::size_t m_line;
    std::size_t m_column;
    bool m_fail;

    std::unordered_set<char> m_spaces;
};

} // namespace lunar

#endif // LUNAR_PARSEC_HPP