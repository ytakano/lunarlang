#include "lunar_parser.hpp"

namespace lunar {

lang::lang(const std::string &filename, const std::string &str)
    : m_parsec(str), m_filename(filename) {}

} // namespace lunar