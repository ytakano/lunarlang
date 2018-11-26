#ifndef LUNAR_COMMON_HPP
#define LUNAR_COMMON_HPP

#include <stdint.h>
#include <errno.h>

#define HASKEY(CONTAINER, KEY) !(CONTAINER.find(KEY) == CONTAINER.end())
#define PRINTERR(M, ...) fprintf(stderr, "ERROR (%s:%d): " M "\n", __FILE__, __LINE__, ##__VA_ARGS__)

namespace lunar {

enum CH_RESULT {
    CH_SUCCESS      = 0x00,
    CH_EMPTY        = 0x01,
    CH_FULL         = 0x02,
    CH_READ_CLOSED  = 0x04,
    CH_WRITE_CLOSED = 0x08,
};

} // namespace lunar

#endif // LUNAR_COMMON_HPP