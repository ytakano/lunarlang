#ifndef LUNAR_XORSHIFT_HPP
#define LUNAR_XORSHIFT_HPP

#include <stdint.h>

namespace lunar {

class splitmix64 {
public:
    splitmix64(uint64_t n) : x(n) { }

    uint64_t next() {
        uint64_t z = (x += UINT64_C(0x9E3779B97F4A7C15));
        z = (z ^ (z >> 30)) * UINT64_C(0xBF58476D1CE4E5B9);
        z = (z ^ (z >> 27)) * UINT64_C(0x94D049BB133111EB);
        return z ^ (z >> 31);
    }

private:
    uint64_t x;
};

class xorshift32 {
public:
    xorshift32() : y(2463534242) { }
    xorshift32(uint32_t seed)
    {
        if (seed == 0)
            y = 2463534242;
        else
            y = seed;
    }

    uint32_t next()
    {
        y = y ^ (y << 13); y = y ^ (y >> 17);
        return y = y ^ (y << 5);
    }

private:
    uint32_t y;
};

class xorshift128 {
public:
    xorshift128() : x(123456789), y(362436069), z(521288629), w(88675123) { }
    xorshift128(uint32_t s)
    {
        if (s == 0) {
            x = 123456789;
            y = 362436069;
            z = 521288629;
            w = 88675123;
        } else {
            x = s = 1812433253U * (s ^ (s >> 30));
            y = s = 1812433253U * (s ^ (s >> 30)) + 1;
            z = s = 1812433253U * (s ^ (s >> 30)) + 2;
            w = s = 1812433253U * (s ^ (s >> 30)) + 3;
        }
    }

    uint32_t next()
    {
        uint32_t t = x ^ (x << 11);
        x = y; y = z; z = w;
        return (w = (w ^ (w >> 19)) ^ (t ^ (t >> 8)));
    }

private:
    uint32_t x, y, z, w;
};

class xorshift128_plus {
public:
    xorshift128_plus()
    {
        splitmix64 sp(0);
        s[0] = sp.next();
        s[1] = sp.next();
    }

    xorshift128_plus(uint64_t n)
    {
        splitmix64 sp(n);
        s[0] = sp.next();
        s[1] = sp.next();
    }

    uint64_t next()
    {
        uint64_t x = s[0];
        uint64_t const y = s[1];
        s[0] = y;
        x ^= x << 23; // a
        s[1] = x ^ y ^ (x >> 17) ^ (y >> 26); // b, c
        return s[1] + y;
    }

private:
    uint64_t s[2];
};

class xorshift1024_star {
public:
    xorshift1024_star() : p(0)
    {
        splitmix64 sp(0);
        for (int i = 0; i < 16; i++)
            s[i] = sp.next();
    }

    xorshift1024_star(uint64_t n) : p(0)
    {
        splitmix64 sp(n);
        for (int i = 0; i < 16; i++)
            s[i] = sp.next();
    }

    uint64_t next()
    {
        const uint64_t s0 = s[p];
        uint64_t s1 = s[p = (p + 1) & 15];
        s1 ^= s1 << 31; // a
        s[p] = s1 ^ s0 ^ (s1 >> 11) ^ (s0 >> 30); // b,c
        return s[p] * UINT64_C(1181783497276652981);
    }

private:
    uint64_t s[16];
    int p;
};

}

#endif // LUNAR_XORSHIFT_HPP