#ifndef LUNAR_COMMON_HPP
#define LUNAR_COMMON_HPP

#include <errno.h>
#include <stdint.h>

#define HASKEY(CONTAINER, KEY) !(CONTAINER.find(KEY) == CONTAINER.end())
#define PRINTERR(M, ...)                                                       \
    fprintf(stderr, "ERROR (%s:%d): " M "\n", __FILE__, __LINE__, ##__VA_ARGS__)

namespace lunar {

enum CH_RESULT {
    CH_SUCCESS = 0x00,
    CH_EMPTY = 0x01,
    CH_FULL = 0x02,
    CH_READ_CLOSED = 0x04,
    CH_WRITE_CLOSED = 0x08,
};

enum type_spec {
    TYPE_BOOL,
    TYPE_FP64, // double
    TYPE_FP32, // float
    TYPE_INT,
    TYPE_U64,
    TYPE_S64,
    TYPE_U32,
    TYPE_S32,
};

} // namespace lunar

#endif // LUNAR_COMMON_HPP