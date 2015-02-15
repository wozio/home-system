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
  LOG("Create session id=" << id << " endpoint=" << endpoint << " destination=" << destination);
}

session::~session()
{
  LOG("Delete client session id=" << id_);

  try
  {
    yami::parameters params;
    params.set_integer("session", id_);
    YC.agent().send(endpoint_, destination_, "session_deleted", params);
  }
  catch (const std::exception& e)
  {
    LOGWARN("EXCEPTION: " << e.what());
  }
}

void session::stream_part(const void* buf, size_t length)
{
  lock_guard<mutex> lock(m_mutex);
  
  streampos len = length;
  
  LOG("RECEIVED: " << len << " writepos=" << writepos_ << " readpos=" << dec << readpos_);
  
  if (readpos_ > writepos_)
  {
    while (writepos_ + len > readpos_)
    {
      send();
    }
  }
  
  buffer_.seekp(writepos_);

  if (writepos_ + len > MAX_BUFFER_SIZE)
  {
    streampos to_write = MAX_BUFFER_SIZE - writepos_;
    buffer_.write((const char*) buf, to_write);
    buffer_.seekp(0);
    writepos_ = 0;
    while (writepos_ + len - to_write > readpos_)
    {
      send();
    }
    buffer_.write(((const char*) buf + to_write), len - to_write);
    writepos_ = len - to_write;
    full_ = true;
  }
  else
  {
    buffer_.write((const char*) buf, len);
    writepos_ += len;
  }

  LOG("RECV: writepos=" << dec << writepos_ << " readpos=" << dec << readpos_);

  trigger_send_some();
}

void session::play()
{
  playing_ = true;
  trigger_send_some();
}

void session::pause()
{
  playing_ = false;
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
  if (playing_)
  {
    lock_guard<mutex> lock(m_mutex);
    
    send();
    
    streamsize end;

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
    LOG("SENDING: writepos=" << dec << writepos_ << " readpos=" << dec << readpos_);
    
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

    len = buffer_.readsome(buf, len);

    if (len > 0)
    {
      yami::parameters params;
      params.set_binary("payload", buf, len);
      params.set_integer("session", id_);

      YC.agent().send(endpoint_, destination_, "stream_part", params);

      readpos_ = buffer_.tellg();

      if (readpos_ == MAX_BUFFER_SIZE)
      {
        readpos_ = 0;
      }
    }
    LOG("SEND: writepos=" << dec << writepos_ << " readpos=" << dec << readpos_);
  }
  catch (const std::exception& e)
  {
    LOGWARN("Error sending stream poart to client: " << e.what());
    trigger_send_some();
  }
}

}
}
