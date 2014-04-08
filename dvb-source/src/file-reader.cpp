#include "file-reader.h"
#include "logger.h"
#include <libdvbapi/dvbdemux.h>
#include <unistd.h>
//#include <fstream>
#include <iostream>

using namespace std;

namespace home_system
{
namespace media
{

file_reader::file_reader(int adapter, int frontend, dvb::session_callback_t callback)
: adapter_(adapter),
  frontend_(frontend),
  continue_(true),
  callback_(callback)
{
  thread_ = thread([this] () { thread_exec(); });
}

file_reader::~file_reader()
{
  continue_ = false;
  if (thread_.joinable())
  {
    thread_.join();
  }
}

void file_reader::thread_exec()
{
  //ofstream f("/storage/stream.ts", ofstream::binary);
  LOG("File reader thread started");
  int fd = dvbdemux_open_dvr(adapter_, frontend_, 1, 0);
  if (fd == -1)
  {
    LOGWARN("Failed to open DVR device");
    return;
  }
  
  // Initialise file descriptor sets
  fd_set read_fds;
  FD_ZERO(&read_fds);
  FD_SET(fd, &read_fds);
  
  struct timeval timeout;
  
  while (continue_)
  {
    // Set timeout to 1.0 seconds
    timeout.tv_sec = 1;
    timeout.tv_usec = 0;

    // Wait for input to become ready or until the time out; the first parameter is
    // 1 more than the largest file descriptor in any of the sets
    switch (select(fd + 1, &read_fds, NULL, NULL, &timeout))
    {
    case 1: // fd is ready for reading
    {
      char buffer[18800];
      size_t n = ::read(fd, buffer, 18800);
      if (callback_ != nullptr && n > 0)
      {
        callback_(n, buffer);
      }

      //f.write(buffer, n);
      break;
    }
    case 0: // timeout
      break;
    case -1: // error
      FD_ZERO(&read_fds);
      FD_SET(fd, &read_fds);
      break;
    }
  }

  //f.close();
  close(fd);

  LOG("File reader thread stopped");
}

}
}
