#ifndef LUNAR_COMMON_HPP
#define LUNAR_COMMON_HPP

namespace lunar {

enum CH_RESULT {
    CH_SUCCESS      = 0x00,
    CH_EMPTY        = 0x01,
    CH_FULL         = 0x02,
    CH_READ_CLOSED  = 0x04,
    CH_WRITE_CLOSED = 0x08,
};

}

#endif // LUNAR_COMMON_HPP