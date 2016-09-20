// tv_test.cpp : Defines the entry point for the console application.
//

#include "pch.h"

#include "../src/session.h"

INITIALIZE_EASYLOGGINGPP

int main()
{
  home_system::media::session s(0, [](int id, void* buf, size_t len, size_t buf_size) {
    LOG(DEBUG) << id << " " << len << " " << buf_size;

  });
  char buf[256];
  for (int i = 0; i < 256; i++)
  {
    buf[i] = i;
  }
  s.stream_part(buf, 256);
  return 0;
}

