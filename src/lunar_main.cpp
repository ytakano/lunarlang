#include "lunar_green_thread.hpp"

#include <stdio.h>

void world(void *arg) {
    printf("World!\n");
    yield_green_thread();
}

void hello(void *arg) {
    printf("Hello ");
    spawn_green_thread(world, nullptr);
    yield_green_thread();
}

int main(int argc, char *argv[]) {
    init_thread();
    spawn_green_thread(hello, nullptr);
    run_green_thread();

    return 0;
}