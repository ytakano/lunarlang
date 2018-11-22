#ifndef CHANNEL_TEST_HPP
#define CHANNEL_TEST_HPP

#include <gtest/gtest.h>
#include "lunar_channel.hpp"

namespace {

class ChannelTest : public ::testing::Test {
protected:
    ChannelTest() : ch(sizeof(int), 4) {

    }

    ~ChannelTest() override {

    }

    void SetUp() override {
        int a = 10, b = 20;
        ch.push((char*)&a);
        ch.push((char*)&b);
    }

    void TearDown() override {
    }

    lunar::channel ch;
};

TEST_F(ChannelTest, PushAndPop) {
    // push data
    int n = 30;
    ASSERT_EQ(ch.push((char *)&n), lunar::CH_SUCCESS);
    n = 40;
    ASSERT_EQ(ch.push((char *)&n), lunar::CH_SUCCESS);
    ASSERT_EQ(ch.push((char *)&n), lunar::CH_FULL);

    // pop data
    ASSERT_EQ(ch.pop((char *)&n), lunar::CH_SUCCESS);
    ASSERT_EQ(n, 10);
    ASSERT_EQ(ch.pop((char *)&n), lunar::CH_SUCCESS);
    ASSERT_EQ(n, 20);
    ASSERT_EQ(ch.pop((char *)&n), lunar::CH_SUCCESS);
    ASSERT_EQ(n, 30);
    ASSERT_EQ(ch.pop((char *)&n), lunar::CH_SUCCESS);
    ASSERT_EQ(n, 40);
    ASSERT_EQ(ch.pop((char *)&n), lunar::CH_EMPTY);
}

TEST_F(ChannelTest, CloseWriteAndPush) {
    ch.close_write();
    int n = 30;
    ASSERT_EQ(ch.push((char *)&n), lunar::CH_WRITE_CLOSED);
}

TEST_F(ChannelTest, CloseReadAndPop) {
    ch.close_read();
    int n;
    ASSERT_EQ(ch.pop((char *)&n), lunar::CH_READ_CLOSED);
}

TEST_F(ChannelTest, CloseReadAndPush) {
    ch.close_read();
    int n = 30;
    ASSERT_EQ(ch.push((char *)&n), lunar::CH_READ_CLOSED);
}

TEST_F(ChannelTest, CloseWriteAndPop) {
    ch.close_write();
    int n;
    ASSERT_EQ(ch.pop((char *)&n), lunar::CH_SUCCESS);
    ASSERT_EQ(n, 10);
    ASSERT_EQ(ch.pop((char *)&n), lunar::CH_SUCCESS);
    ASSERT_EQ(n, 20);
    ASSERT_EQ(ch.pop((char *)&n), lunar::CH_WRITE_CLOSED);
}

TEST_F(ChannelTest, RunMany) {
    int n;

    // pop all
    ASSERT_EQ(ch.pop((char *)&n), lunar::CH_SUCCESS);
    ASSERT_EQ(n, 10);
    ASSERT_EQ(ch.pop((char *)&n), lunar::CH_SUCCESS);
    ASSERT_EQ(n, 20);

    for (int i = 0; i < 10000; i++)
    {
        ASSERT_EQ(ch.push((char *)&i), lunar::CH_SUCCESS);
        ASSERT_EQ(ch.pop((char *)&n), lunar::CH_SUCCESS);
        ASSERT_EQ(i, n);
    }
}

}

#endif // CHANNEL_TEST_HPP