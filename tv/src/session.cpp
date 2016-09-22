#include "pch.h"
#include "session.h"
#include "logger.h"
#include "yamicontainer.h"

#define BUFSIZE 18800

using namespace std;

namespace home_system
{
namespace media
{

//#define TOTAL_BUFFER_SIZE 2147483648 // 2GB
#define TOTAL_BUFFER_SIZE 20971520 // 20MB

session::session(int id, stream_callback_t stream_callback)
: id_(id),
  stream_callback_(stream_callback),
  playing_(true),
  read_write_diff_(0),
  readpos_(0),
  writepos_(0),
  full_(false)
{
  ostringstream str;
  str << "timeshift_buffer_" << id_ << ".ts";
  buffer_.open(str.str(), ios::trunc | ios::binary | ios::out | ios::in);
  LOG(DEBUG) << "Create session id=" << id;
}

session::~session()
{
  LOG(DEBUG) << "Delete client session id=" << id_;
  playing_ = false;
  ios_.stop_ios();
}

void session::stream_part(const void* buf, size_t length)
{
  lock_guard<mutex> lock(m_mutex);
  
  char* mybuf = (char*)buf;
  
  size_t to_write = length;
  
  //LOG(TRACE)  << "RECEIVED: " << length << " writepos=" << writepos_ << " readpos=" << readpos_ << " diff=" << dec << read_write_diff_;

  if (to_write > TOTAL_BUFFER_SIZE)
  {
    // well...
    mybuf += to_write - TOTAL_BUFFER_SIZE;
  }

  // first ensure that writepos will never pass readpos
  while (to_write > TOTAL_BUFFER_SIZE - read_write_diff_)
  {
    send();
  }
  
  if (writepos_ + to_write >= TOTAL_BUFFER_SIZE)
  {
    // writing is going across end of buffer
    // here write from current pos to end of buffer
    size_t to_write_here = TOTAL_BUFFER_SIZE - writepos_;
    buffer_.seekp(writepos_);
    buffer_.write(mybuf, to_write_here);
    // decrement number of bytes to write by already written amount
    to_write -= to_write_here;
    // move pointer in the buffer
    mybuf += to_write_here;
    // increment read_write_diff
    read_write_diff_ += to_write_here;
    full_ = true;
    writepos_ = 0;
  }
  if (to_write > 0)
  {
    buffer_.seekp(writepos_);
    buffer_.write(mybuf, to_write);
    writepos_ += to_write;
    read_write_diff_ += to_write;
  }

  //LOG(TRACE) << "RECEIVED: writepos=" << writepos_ << " readpos=" << readpos_ << " diff=" << dec << read_write_diff_;

  trigger_send_some();
}

void session::play()
{
  LOG(DEBUG) << "Play for session " << id_;
  playing_ = true;
  trigger_send_some();
}

void session::pause()
{
  LOG(DEBUG) << "Pause for session " << id_;
  playing_ = false;
}

void session::seek(long long pos)
{
  LOG(DEBUG) << "Seek for session " << id_ << " to position " << pos;
}

void session::trigger_send_some()
{
  if (playing_)
  {
    ios_.io_service().post([this] () {
      send_some();
    });
  }
}

void session::send_some()
{
  lock_guard<mutex> lock(m_mutex);
  if (playing_)
  {
    send();
    
    if (read_write_diff_ != 0)
    {
      trigger_send_some();
    }
  }
}

void session::send()
{
  try
  {
    //LOG(TRACE) << "SENDING: writepos=" << dec << writepos_ << " readpos=" << dec << readpos_ << " diff=" << dec << read_write_diff_;

    if (read_write_diff_ == 0)
    {
      return;
    }
    
    if (readpos_ == TOTAL_BUFFER_SIZE)
    {
      readpos_ = 0;
    }

    char buf[BUFSIZE];
    size_t len, end;

    if (readpos_ + read_write_diff_ > TOTAL_BUFFER_SIZE)
    {
      end = TOTAL_BUFFER_SIZE;
    }
    else
    {
      end = readpos_ + read_write_diff_;
    }

    if (end - readpos_ < BUFSIZE)
    {
      len = end - readpos_;
    }
    else
    {
      len = BUFSIZE;
    }

    buffer_.seekg(readpos_);
    buffer_.read(buf, len);
    
    size_t size;
    full_ ? size = TOTAL_BUFFER_SIZE : size = writepos_;

    stream_callback_(id_, buf, len, size);

    readpos_ += len;
    read_write_diff_ -= len;

    //LOG(TRACE) << "SENT: len=" << len << " writepos=" << dec << writepos_ << " readpos=" << dec << readpos_ << " diff=" << dec << read_write_diff_;
  }
  catch (const std::exception& e)
  {
    LOG(WARNING) << "Error sending stream part to client: " << e.what();
    trigger_send_some();
  }
}

}
}
