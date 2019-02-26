#include "lunar_parsec.hpp"

namespace lunar {

parsec::parsec(const std::string &str)
    : m_str(str), m_pos(0), m_line(1), m_column(1), m_fail(false) {
    m_spaces.insert(' ');
    m_spaces.insert('\t');
    m_spaces.insert('\r');
    m_spaces.insert('\n');

    m_esc_char['a'] = '\a';
    m_esc_char['b'] = '\b';
    m_esc_char['f'] = '\f';
    m_esc_char['r'] = '\r';
    m_esc_char['n'] = '\n';
    m_esc_char['t'] = '\t';
    m_esc_char['v'] = '\v';
    m_esc_char['\\'] = '\\';
    m_esc_char['?'] = '\?';
    m_esc_char['\''] = '\'';
    m_esc_char['\"'] = '"';
    m_esc_char['\0'] = '\0';
    m_esc_char['U'] = 'U';
    m_esc_char['u'] = 'u';

    m_hex2num['0'] = 0;
    m_hex2num['1'] = 1;
    m_hex2num['2'] = 2;
    m_hex2num['3'] = 3;
    m_hex2num['4'] = 4;
    m_hex2num['5'] = 5;
    m_hex2num['6'] = 6;
    m_hex2num['7'] = 7;
    m_hex2num['8'] = 8;
    m_hex2num['9'] = 9;
    m_hex2num['a'] = 10;
    m_hex2num['A'] = 10;
    m_hex2num['b'] = 11;
    m_hex2num['B'] = 11;
    m_hex2num['c'] = 12;
    m_hex2num['C'] = 12;
    m_hex2num['d'] = 13;
    m_hex2num['D'] = 13;
    m_hex2num['e'] = 14;
    m_hex2num['e'] = 14;
    m_hex2num['f'] = 15;
    m_hex2num['F'] = 15;
}

bool parsec::str_literal(std::string &ret) {
    character('"');
    if (m_fail)
        return false;

    for (;;) {
        char c = any();
        if (m_fail)
            return false;

        if (c == '\"')
            break;

        if (c == '\\') {
            // parse escape character
            char esc = satisfy(
                [&](char c) { return m_esc_char.find(c) != m_esc_char.end(); });

            if (m_fail)
                return false;

            if (esc == 'u' || esc == 'U') {
                for (int i = 0; i < (esc == 'u') ? 2 : 4; i++) {
                    char h0 = hex();
                    if (m_fail)
                        return false;

                    char h1 = hex();
                    if (m_fail)
                        return false;

                    ret.push_back(m_hex2num[h0] << 4 | m_hex2num[h1]);
                }
            } else {
                ret.push_back(m_esc_char[esc]);
            }
        } else {
            ret.push_back(c);
        }
    }

    return true;
}

// $DECIMAL := [1-9][0-9]* | 0
// $FLOAT := $DECIMAL.[0-9]* $EXP? f?
// $EXP := e $PLUSMINUS [0-9]+
// $PLUSMINUS := + | -
parsec::numtype parsec::num_literal(std::string &ret) {
    // $DECIMAL
    char c = peek();
    if (m_fail)
        return NUM_FAIL;

    if (c == 0) {
        any();
        ret.push_back(c);
    } else {
        PMANY(*this, ret, decimal());
    }

    // .[0-9]* $EXP? f?
    c = peek();
    if (c != '.' || m_fail)
        return NUM_INT;

    PMANYONE(*this, ret, decimal());
    if (m_fail)
        return NUM_FAIL;

    c = peek();
    if (m_fail)
        return NUM_DOUBLE;

    // $EXP
    if (c == 'e') {
        any();
        ret.push_back(c);

        c = satisfy([](char a) { return a == '+' || a == '-'; });
        if (m_fail)
            return NUM_FAIL;

        ret.push_back(c);

        PMANYONE(*this, ret, decimal());
        if (m_fail)
            return NUM_FAIL;
    }

    // f?
    c = peek();
    if (m_fail)
        return NUM_DOUBLE;

    if (c == 'f') {
        any();
        ret.push_back(c);
        return NUM_FLOAT;
    }

    return NUM_DOUBLE;
}

} // namespace lunar