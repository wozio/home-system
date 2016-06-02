#include "session.h"
#include "logger.h"
#include "yamicontainer.h"
#include <memory>
#include <sstream>

#define BUFSIZE 18800

using namespace std;

namespace home_system
{
namespace media
{

//#define MAX_BUFFER_SIZE 2147483648 // 2GB
#define MAX_BUFFER_SIZE 20971520 // 20MB

session::session(int id, std::string endpoint, std::string destination)
: id_(id),
  endpoint_(endpoint),
  destination_(destination),
  playing_(true),
  readpos_(0),
  writepos_(0),
  full_(false)
{
  ostringstream str;
  str << "timeshift_buffer_" << id_ << ".ts";
  buffer_.open(str.str(), ios::trunc | ios::binary | ios::out | ios::in);
  LOG(DEBUG) << "Create session id=" << id << " endpoint=" << endpoint << " destination=" << destination;
}

session::~session()
{
  LOG(DEBUG) << "Delete client session id=" << id_;

  try
  {
    yami::parameters params;
    params.set_integer("session", id_);
    YC.agent().send(endpoint_, destination_, "session_deleted", params);
  }
  catch (const std::exception& e)
  {
    LOG(WARNING) << "EXCEPTION: " << e.what();
  }
}

void session::stream_part(const void* buf, size_t length)
{
  lock_guard<mutex> lock(m_mutex);
  
  char* mybuf = (char*)buf;
  
  streampos to_write = length;
  
  //LOG(TRACE)  << "RECEIVED: " << length << " writepos=" << writepos_ << " readpos=" << readpos_;
  
  buffer_.seekp(writepos_);

  if (writepos_ + to_write > MAX_BUFFER_SIZE)
  {
    // when buffer is full write position must never pass read position
    // so enough bytes is sent to free space
    if (full_)
    {
      if (writepos_ < readpos_)
      {
        // it must not be equal since in such case there is no way to distinguish
        // if read pos should be behind or in front of write pos
        while (writepos_ + to_write >= readpos_)
        {
          send();
        }
      }
    }
    // from current write pos to end of the buffer
    streampos to_write_here = MAX_BUFFER_SIZE - writepos_;
    buffer_.write(mybuf, to_write_here);
    // decrement number of bytes to write by already written amount
    to_write -= to_write_here;
    // move pointer in the buffer
    mybuf += to_write_here;
    // set write pointer to beggining of the file
    buffer_.seekp(writepos_);
    writepos_ = 0;
    full_ = true;
  }
  // when buffer is full write position must never pass read position
  // so enough bytes is sent to free space
  if (full_)
  {
    if (writepos_ < readpos_)
    {
      // it must not be equal since in such case there is no way to distinguish
      // if read pos should be behind or in front of write pos
      while (writepos_ + to_write >= readpos_)
      {
        send();
      }
    }
  }
  
  buffer_.write(mybuf, to_write);
  writepos_ += to_write;

  //LOG(TRACE) << "RECV: writepos=" << writepos_ << " readpos=" << readpos_;

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
  
  lock_guard<mutex> lock(m_mutex);
  
  yami::parameters params;
  params.set_integer("session", id_);
  YC.agent().send(endpoint_, destination_, "clear_stream", params);
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
    
    streampos end;

    if (readpos_ > writepos_)
    {
      end = MAX_BUFFER_SIZE;
    }
    else
    {
      end = writepos_;
    }
    
    if (readpos_ != end)
    {
      trigger_send_some();
    }
  }
}

void session::send()
{
  try
  {
    //LOG(TRACE) << "SENDING: writepos=" << dec << writepos_ << " readpos=" << dec << readpos_;
    
    if (readpos_ == writepos_)
    {
      return;
    }

    char buf[BUFSIZE];
    streamsize len, end;

    if (readpos_ > writepos_)
    {
      end = MAX_BUFFER_SIZE;
    }
    else
    {
      end = writepos_;
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

    if (len > 0)
    {
      yami::parameters params;
      params.set_binary("payload", buf, len);
      params.set_integer("session", id_);
      streampos size;
      full_ ? size = MAX_BUFFER_SIZE : size = writepos_;
      params.set_integer("buffer_size", size);

      YC.agent().send(endpoint_, destination_, "stream_part", params);

      readpos_ = buffer_.tellg();

      if (readpos_ == (streampos)MAX_BUFFER_SIZE)
      {
        readpos_ = 0;
      }
      //LOG(TRACE) << "SENT: len=" << len << " writepos=" << dec << writepos_ << " readpos=" << dec << readpos_;
    }
  }
  catch (const std::exception& e)
  {
    LOG(WARNING) << "Error sending stream part to client: " << e.what();
    trigger_send_some();
  }
}

}
}
