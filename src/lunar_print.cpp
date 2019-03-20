#include "lunar_print.hpp"

#include <stdio.h>
#include <unistd.h>

#include <iostream>

namespace lunar {

void print::unum(uint64_t var) {
    if (PRINTBUFSIZE - m_pos < 20)
        flush();

    m_pos += snprintf(m_buf + m_pos, PRINTBUFSIZE - m_pos, "%llu", (unsigned long long)var);
}

void print::snum(int64_t var) {
    if (PRINTBUFSIZE - m_pos < 20)
        flush();

    m_pos += snprintf(m_buf + m_pos, PRINTBUFSIZE - m_pos, "%lld", (unsigned long long)var);
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

void print::fp32(float var) {
    snprintf(m_buf + m_pos, PRINTBUFSIZE - m_pos, "%f", var);
}

void print::fp64(double var) {
    snprintf(m_buf + m_pos, PRINTBUFSIZE - m_pos, "%lf", var);
}

void print_err(std::size_t line, std::size_t column, const std::string &str) {
    std::vector<std::string> lines;

    boost::split(lines, str, boost::is_any_of("\n"));

    std::cerr << lines[line - 1] << std::endl;

    for (size_t i = 1; i < column; i++) {
        std::cerr << " ";
    }
    std::cerr << "^" << std::endl;
}

} // namespace lunar
