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

  home_system::media::session s(0, [&](int id, void* buf, size_t len, size_t buf_size, size_t buf_pos) {
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
  for (int i = 0; i < buf_size; i++)
  {
    buf[i] = i + 1;
  }
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
  test(BUFFER_SIZE, 1);
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

BOOST_AUTO_TEST_CASE(session_1_byte_parts)
{
  test(1, 20000);
}


BOOST_AUTO_TEST_CASE(session_seeking)
{
  std::vector<char> recv_buf;
  recv_buf.resize(50);
  std::atomic<int> recv_len{ 0 };

  std::condition_variable cv;
  std::mutex cv_m;

  home_system::media::session s(0, [&](int id, void* buf, size_t len, size_t buf_size, size_t buf_pos) {
    BOOST_TEST(id == 0);
    BOOST_TEST(len == 50);
    BOOST_TEST(buf_size == 100);
    BOOST_TEST(buf_pos == 100);
    if (len <= 50)
    {
      memcpy(&recv_buf[0], buf, len);
    }
    recv_len += len;
    cv.notify_all();
  });

  // fill the buffer while paused
  s.pause();

  std::vector<char> buf;
  buf.resize(100);
  for (int i = 0; i < 100; i++)
  {
    buf[i] = i;
  }

  s.stream_part(&buf[0], 100);

  // now seek to 50 position and run playing
  // in callback we should receive 49 bytes with values from 50 to 99
  BOOST_TEST(s.seek(50) == 50);
  BOOST_TEST(s.play() == 50);

  // wait for result in abother thread
  using namespace std::chrono_literals;
  std::unique_lock<std::mutex> lk(cv_m);
  BOOST_TEST((cv.wait_for(lk, 1s, [&]() {return recv_len == 50; }) == true), "Timed out waiting for all data to receive");

  std::vector<char> cmp_buf;
  cmp_buf.insert(cmp_buf.begin(), buf.begin() + 50, buf.end());
  
  BOOST_TEST(recv_len == 50);
  BOOST_TEST(recv_buf == cmp_buf, "Buffers not equal");
}

BOOST_AUTO_TEST_CASE(session_seeking_full_buffer)
{
  std::vector<char> recv_buf;
  recv_buf.resize(150);
  std::atomic<int> recv_len{ 0 };

  std::condition_variable cv;
  std::mutex cv_m;

  home_system::media::session s(0, [&](int id, void* buf, size_t len, size_t buf_size, size_t buf_pos) {
    BOOST_TEST(id == 0);
    BOOST_TEST(buf_size == BUFFER_SIZE);
    if (recv_len + len <= 150)
    {
      memcpy(&recv_buf[recv_len], buf, len);
    }
    recv_len += len;
    cv.notify_all();
  });

  // fill the buffer while paused
  s.pause();

  std::vector<char> buf;
  buf.resize(BUFFER_SIZE);
  for (int i = 0; i < BUFFER_SIZE; i++)
  {
    buf[i] = i;
  }
  s.stream_part(&buf[0], BUFFER_SIZE);
  s.stream_part(&buf[0], 100);
  
  // now seek to buffer size - 150 position and run playing
  // in callback we should receive 50 bytes (from position to the end of buffer)
  // and then 100 bytes (remaining from beginning of buffer till last written data)
  BOOST_TEST(s.seek(BUFFER_SIZE - 150) == BUFFER_SIZE - 150);
  BOOST_TEST(s.play() == BUFFER_SIZE - 150);

  // wait for result in abother thread
  using namespace std::chrono_literals;
  std::unique_lock<std::mutex> lk(cv_m);
  BOOST_TEST((cv.wait_for(lk, 1s, [&]() {return recv_len == 150; }) == true), "Timed out waiting for all data to receive");

  std::vector<char> cmp_buf;
  cmp_buf.insert(cmp_buf.end(), buf.end() - 50, buf.end());
  cmp_buf.insert(cmp_buf.end(), buf.begin(), buf.begin() + 100);

  BOOST_TEST(recv_len == 150);
  BOOST_TEST(recv_buf == cmp_buf, "Buffers not equal");
}