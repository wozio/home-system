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

file_reader::file_reader(int adapter, int frontend, const std::string& endpoint)
: adapter_(adapter),
  frontend_(frontend),
  continue_(true),
  endpoint_(endpoint)
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

bool file_reader::is_running()
{
  return running_;
}

void file_reader::thread_exec()
{
  running_ = true;
  
  //ofstream f("/storage/stream.ts", ofstream::binary);
  LOG(DEBUG) << "File reader thread started";
  int fd = dvbdemux_open_dvr(adapter_, frontend_, 1, 0);
  if (fd == -1)
  {
    LOG(WARNING) << "Failed to open DVR device";
    return;
  }
  
  // Initialise file descriptor sets
  fd_set read_fds;
  FD_ZERO(&read_fds);
  FD_SET(fd, &read_fds);
  
  struct timeval timeout;
  
  session_.reset(new server_binary_session(endpoint_));
  
  try
  {  
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
        if (n > 0)
        {
          session_->send(buffer, n);
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
  }
  catch (const dvb::session_error&)
  {
    LOG(DEBUG) << "Session error, quiting file reader thread";
  }

  //f.close();
  close(fd);
  
  running_ = false;

  LOG(DEBUG) << "File reader thread stopped";
}

}
}
