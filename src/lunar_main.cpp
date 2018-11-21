#include "lunar_channel.hpp"

#include <stdio.h>

int
main (int argc, char *argv[])
{
    lunar::channel ch(sizeof(int), 2);

    int a = 10;
    int b = 20;
    int c, d;

    printf("close read\n");
    ch.push((char*)&a);
    ch.push((char*)&b);
    printf("push = %x\n", ch.push((char*)&a));

    ch.pop((char*)&c);
    ch.pop((char*)&d);
    printf("c = %d\n", c);
    printf("d = %d\n", d);

    printf("pop = %x\n", ch.pop((char*)&d));

    printf("close read\n");
    ch.close_read();
    printf("push = %x\n", ch.push((char*)&d));
    printf("pop = %x\n", ch.pop((char*)&d));

    printf("close write\n");
    ch.close_write();
    printf("push = %x\n", ch.push((char*)&d));

    return 0;
}