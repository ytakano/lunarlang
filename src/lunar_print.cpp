#include "lunar_print.hpp"

#include <stdio.h>
#include <unistd.h>

namespace lunar {

void print::unum(uint64_t var) {
    if (PRINTBUFSIZE - m_pos < 20)
        flush();

    m_pos += snprintf(m_buf + m_pos, PRINTBUFSIZE - m_pos, "%llu", var);
}

void print::snum(int64_t var) {
    if (PRINTBUFSIZE - m_pos < 20)
        flush();

    m_pos += snprintf(m_buf + m_pos, PRINTBUFSIZE - m_pos, "%lld", var);
}

void print::boolean(bool var) {
    if (PRINTBUFSIZE - m_pos < 5)
        flush();

    if (var) {
        m_pos += snprintf(m_buf + m_pos, PRINTBUFSIZE - m_pos, "true");
    } else {
        m_pos += snprintf(m_buf + m_pos, PRINTBUFSIZE - m_pos, "false");
    }
}

void print::endl() {
    if (PRINTBUFSIZE - m_pos < 1)
        flush();

    m_buf[m_pos] = '\n';
    m_pos++;

    flush();
}

void print::flush() {
    write(STDOUT_FILENO, m_buf, m_pos);
    m_pos = 0;
}

void print::utf8(const char *str) {
    while (*str != '\0') {
        uint8_t h = *str & 0xf0;
        switch (h) {
        case 0xc0:
        case 0xd0:
            // 2 bytes character
            if (PRINTBUFSIZE - m_pos < 2)
                flush();

            m_buf[m_pos] = str[0];
            m_buf[m_pos + 1] = str[1];

            m_pos += 2;
        case 0xe0:
            // 3 bytes character
            if (PRINTBUFSIZE - m_pos < 3)
                flush();

            m_buf[m_pos] = str[0];
            m_buf[m_pos + 1] = str[1];
            m_buf[m_pos + 3] = str[3];

            m_pos += 3;
        case 0xf0:
            // 4 bytes character
            if (PRINTBUFSIZE - m_pos < 4)
                flush();

            m_buf[m_pos] = str[0];
            m_buf[m_pos + 1] = str[1];
            m_buf[m_pos + 3] = str[3];
            m_buf[m_pos + 4] = str[4];

            m_pos += 4;
        default:
            if (PRINTBUFSIZE - m_pos < 1)
                flush();

            m_buf[m_pos] = *str;

            if (*str == '\n')
                flush();

            str++;
            m_pos++;
        }
    }
}

void print::ptr(const void *ptr) {
    if (PRINTBUFSIZE - m_pos < 10)
        flush();

    m_pos += snprintf(m_buf + m_pos, PRINTBUFSIZE - m_pos, "0x%p", ptr);
}

} // namespace lunar