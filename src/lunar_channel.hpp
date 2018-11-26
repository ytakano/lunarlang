#ifndef LUNAR_CHANNEL_HPP
#define LUNAR_CHANNEL_HPP

// channel for green thread
// this is MT-UNSAFE and non-blocking

#include <stdint.h>
#include <string.h>
#include <stdio.h>

#include "lunar_common.hpp"

namespace lunar
{

class channel
{
  public:
    channel(int bucket_size, int len) : m_len(0),
                                        m_max_len(len),
                                        m_bucket_size(bucket_size),
                                        m_buf(new char[bucket_size * len]),
                                        m_end(&m_buf[bucket_size * len]),
                                        m_head(m_buf),
                                        m_tail(m_buf),
                                        m_flags(0) {}
    virtual ~channel()
    {
        delete[] m_buf;
    }

    CH_RESULT
    push(const char *val)
    {
        if (m_flags & CH_WRITE_CLOSED)
            return CH_WRITE_CLOSED;

        if (m_flags & CH_READ_CLOSED)
            return CH_READ_CLOSED;

        if (m_len == m_max_len)
            return CH_FULL;

        memcpy(m_tail, val, m_bucket_size);

        m_tail += m_bucket_size;
        if (m_tail >= m_end)
            m_tail = m_buf;

        m_len++;

        return CH_SUCCESS;
    }

    CH_RESULT
    pop(char *ret)
    {
        if (m_flags & CH_READ_CLOSED)
            return CH_READ_CLOSED;

        if (m_len == 0)
        {
            if (m_flags & CH_WRITE_CLOSED)
                return CH_WRITE_CLOSED;
            else
                return CH_EMPTY;
        }

        memcpy(ret, m_head, m_bucket_size);

        m_head += m_bucket_size;
        if (m_head >= m_end)
            m_head = m_buf;

        m_len--;

        return CH_SUCCESS;
    }

    void close_read() { m_flags |= CH_READ_CLOSED; };
    void close_write() { m_flags |= CH_WRITE_CLOSED; };

  private:
    volatile int m_len;
    int m_max_len;
    int m_bucket_size;

    char *m_buf;
    char *m_end;
    char *m_head;
    char *m_tail;

    int m_flags;
};

} // namespace lunar

#endif // LUNAR_CHANNEL_HPP