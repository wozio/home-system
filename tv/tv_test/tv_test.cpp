// tv_test.cpp : Defines the entry point for the console application.
//

#include "pch.h"

#include "../src/session.h"

#define BOOST_TEST_MODULE test tv session
#include <boost/test/unit_test.hpp>

INITIALIZE_EASYLOGGINGPP

#define BUFFER_SIZE 20971520

BOOST_AUTO_TEST_CASE(session_stream_part)
{
  std::vector<char> recv_buf;
  recv_buf.resize(BUFFER_SIZE);
  std::atomic<int> recv_len{ 0 };

  std::condition_variable cv;
  std::mutex cv_m;

  home_system::media::session s(0, [&](int id, void* buf, size_t len, size_t buf_size) {
    BOOST_TEST(id == 0);
    BOOST_TEST(buf_size == BUFFER_SIZE);
    BOOST_TEST(recv_len + len <= BUFFER_SIZE);
    if (recv_len + len <= BUFFER_SIZE)
    {
      char* cbuf = (char*)buf;
      for (int i = 0; i < len; ++i)
      {
        recv_buf[recv_len + i] = cbuf[i];
      }
    }
    recv_len += len;
    cv.notify_all();
  });
  std::vector<char> buf;
  buf.resize(BUFFER_SIZE);
  for (int i = 0; i < BUFFER_SIZE; i++)
  {
    buf[i] = i;
  }
  s.stream_part(&buf[0], BUFFER_SIZE);

  using namespace std::chrono_literals;
  std::unique_lock<std::mutex> lk(cv_m);
  BOOST_TEST((cv.wait_for(lk, 100s, [&]() {return recv_len >= BUFFER_SIZE; }) == true), "Timed out waiting for all data to receive");

  BOOST_TEST(recv_len == BUFFER_SIZE, "received size not correct " << recv_len << "!=" << BUFFER_SIZE);
  BOOST_TEST(recv_buf == buf, "Buffers not equal");
}

#define BUFFER_SIZE3 6990506
#define BUFFER_SIZE3_FULL 6990506 * 4

BOOST_AUTO_TEST_CASE(session_stream_part_4_parts)
{
  std::vector<char> recv_buf;
  recv_buf.resize(BUFFER_SIZE3_FULL);
  std::atomic<int> recv_len{ 0 };

  std::condition_variable cv;
  std::mutex cv_m;

  home_system::media::session s(0, [&](int id, void* buf, size_t len, size_t buf_size) {
    BOOST_TEST(id == 0);
    BOOST_TEST(recv_len + len <= BUFFER_SIZE3_FULL);
    if (recv_len + len <= BUFFER_SIZE3_FULL)
    {
      char* cbuf = (char*)buf;
      for (int i = 0; i < len; ++i)
      {
        recv_buf[recv_len + i] = cbuf[i];
      }
    }
    recv_len += len;
    cv.notify_all();
  });
  std::vector<char> buf;
  buf.resize(BUFFER_SIZE3);
  for (int i = 0; i < BUFFER_SIZE3; i++)
  {
    buf[i] = i;
  }
  s.stream_part(&buf[0], BUFFER_SIZE3);
  s.stream_part(&buf[0], BUFFER_SIZE3);
  s.stream_part(&buf[0], BUFFER_SIZE3);
  s.stream_part(&buf[0], BUFFER_SIZE3);

  std::vector<char> sent_buf;
  sent_buf.resize(BUFFER_SIZE3_FULL);
  sent_buf.insert(buf);

  using namespace std::chrono_literals;
  std::unique_lock<std::mutex> lk(cv_m);
  BOOST_TEST((cv.wait_for(lk, 100s, [&]() {return recv_len >= BUFFER_SIZE3_FULL; }) == true), "Timed out waiting for all data to receive");

  BOOST_TEST(recv_len == BUFFER_SIZE3_FULL, "received size not correct " << recv_len << "!=" << BUFFER_SIZE3_FULL);
  BOOST_TEST(recv_buf == buf, "Buffers not equal");
}