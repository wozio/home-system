// tv_test.cpp : Defines the entry point for the console application.
//

#include "pch.h"

#include "../src/session.h"

#define BOOST_TEST_MODULE test tv session
#include <boost/test/unit_test.hpp>

INITIALIZE_EASYLOGGINGPP

#define BUFFER_SIZE 20971520

void test(size_t buf_size, int buf_num)
{
  size_t buf_size_total = buf_size * buf_num;

  std::vector<char> recv_buf;
  recv_buf.resize(buf_size_total);
  std::atomic<int> recv_len{ 0 };

  std::condition_variable cv;
  std::mutex cv_m;

  home_system::media::session s(0, [&](int id, void* buf, size_t len, size_t buf_size) {
    BOOST_TEST(id == 0);
    BOOST_TEST(recv_len + len <= buf_size_total);
    if (recv_len + len <= buf_size_total)
    {
      memcpy(&recv_buf[recv_len], buf, len);
    }
    recv_len += len;
    cv.notify_all();
  });
  std::vector<char> buf;
  buf.resize(buf_size);
  /*for (int i = 0; i < buf_size; i++)
  {
    buf[i] = i;
  }*/
  std::vector<char> sent_buf;
  for (int i = 0; i < buf_num; ++i)
  {
    s.stream_part(&buf[0], buf_size);
    sent_buf.insert(sent_buf.end(), buf.begin(), buf.end());
  }

  using namespace std::chrono_literals;
  std::unique_lock<std::mutex> lk(cv_m);
  BOOST_TEST((cv.wait_for(lk, 100s, [&]() {return recv_len >= buf_size_total; }) == true), "Timed out waiting for all data to receive");

  BOOST_TEST(recv_len == buf_size_total);
  BOOST_TEST(recv_buf == sent_buf, "Buffers not equal");
}

BOOST_AUTO_TEST_CASE(session_1_part_filling_exactly_whole_buffer)
{
  test(20971520, 1);
}

BOOST_AUTO_TEST_CASE(session_4_parts_filling_more_than_buffer)
{
  test(6990506, 4);
}

BOOST_AUTO_TEST_CASE(session_11_parts_not_filling_buffer)
{
  test(18801, 11);
}

BOOST_AUTO_TEST_CASE(session_lot_of_small_parts_filling_buffer)
{
  test(1880, 20000);
}