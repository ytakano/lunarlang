#ifndef LUNAR_PRINT_HPP
#define LUNAR_PRINT_HPP

#include "lunar_common.hpp"

#define PRINTBUFSIZE 4096

#include <string>

#include <boost/algorithm/string.hpp>

namespace lunar {

class print {
  public:
    print() : m_pos(0) {}
    virtual ~print() {}

    void unum(uint64_t var);
    void snum(int64_t var);
    void boolean(bool var);
    void utf8(const char *str);
    void ptr(const void *ptr);
    void fp32(float var);
    void fp64(double var);
    void endl();
    void flush();

  private:
    char m_buf[PRINTBUFSIZE];
    int m_pos;
};

void print_err(std::size_t line, std::size_t column, const std::string &str);

} // namespace lunar

#endif // LUNAR_PRINT_HPP