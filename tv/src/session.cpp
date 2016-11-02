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

#define TOTAL_BUFFER_SIZE 2147483648 // 2GB
//#define TOTAL_BUFFER_SIZE 20971520 // 20MB

session::session(int id, const std::string& endpoint)
: id_(id),
  playing_(true),
  read_write_diff_(0),
  readpos_(0),
  writepos_(0),
  full_(false),
  abs_pos_(0),
  abs_len_(0)
{
  ostringstream str;
  str << "timeshift_buffer_" << id_ << ".ts";
  buffer_.open(str.str(), ios::trunc | ios::binary | ios::out | ios::in);
  LOG(DEBUG) << "Create session id=" << id << " buffer size=" << dec << TOTAL_BUFFER_SIZE;
  binary_session_.reset(new server_binary_session(endpoint));
}

session::~session()
{
  LOG(DEBUG) << "Delete client session id=" << id_;
  playing_ = false;
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
    if (playing_)
    {
      send();
    }
    else
    {
      // overwrite
      read_write_diff_ = TOTAL_BUFFER_SIZE - to_write;
      if (writepos_ + to_write > TOTAL_BUFFER_SIZE)
      {
        readpos_ = to_write - (TOTAL_BUFFER_SIZE - writepos_);
      }
      else
      {
        readpos_ = writepos_ + to_write;
      }
    }
  }

  pos_to_time_[abs_len_] = std::time(nullptr);
  
  if (writepos_ + to_write >= TOTAL_BUFFER_SIZE)
  {
    LOG(DEBUG) << "Buffer full";
    log_data();
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
    abs_len_ += to_write_here;
  }
  if (to_write > 0)
  {
    buffer_.seekp(writepos_);
    buffer_.write(mybuf, to_write);
    writepos_ += to_write;
    read_write_diff_ += to_write;
    abs_len_ += to_write;
  }

  trigger_send();
}

void session::play()
{
  lock_guard<mutex> lock(m_mutex);
  //LOG(DEBUG) << "Play";
  //log_data();
  playing_ = true;

  trigger_send();
}

void session::pause()
{
  lock_guard<mutex> lock(m_mutex);
  //LOG(DEBUG) << "Pause";
  //log_data();
  playing_ = false;

  trigger_send();
}

void session::seek(long long apos, std::function<void(long long pos, long long time)> callback)
{
  LOG(DEBUG) << "Seek for session " << id_ << " to position " << dec << apos;
  
  lock_guard<mutex> lock(m_mutex);

  size_t size;
  full_ ? size = TOTAL_BUFFER_SIZE : size = writepos_;

  // checking bonduaries of physical buffer
  if (apos > abs_len_)
  {
    apos = abs_len_;
  }
  else if (apos < abs_len_ - size)
  {
    apos = abs_len_ - size;
  }

  abs_pos_ = apos;

  // position in the physical buffer corresponding to absolute position
  long long pos = size - (abs_len_ - apos);

  read_write_diff_ = size - pos;

  if (writepos_ >= read_write_diff_)
  {
    readpos_ = writepos_ - read_write_diff_;
  }
  else
  {
    readpos_ = writepos_ + size - read_write_diff_;
  }

  trigger_send();

  long long ct;
  if (pos_to_time_.size() == 1)
  {
    ct = pos_to_time_.begin()->second;
  }
  else
  {
    auto i = pos_to_time_.lower_bound(abs_pos_);
    if (i == pos_to_time_.end())
    {
      i = --pos_to_time_.end();
      ct = i->second;
    }
    else
    {
      ct = i->second;
    }
  }

  log_data();

  callback(apos, ct);
}

void session::trigger_send()
{
  ios_.io_service().post([this]() {
    lock_guard<mutex> lock(m_mutex);
    send();
  });
}

void session::send()
{
  try
  {
    char buf[BUFSIZE];
    size_t len = 0, end;

    // when nothing to send or we are not playing, send only current times
    if (read_write_diff_ > 0 && playing_)
    {
      if (readpos_ == TOTAL_BUFFER_SIZE)
      {
        readpos_ = 0;
      }

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
    }

    long long bt, ct, et;
    get_times(bt, ct, et);
    
    if (len > 0)
      binary_session_->send(buf, len);

    abs_pos_ += len;
    readpos_ += len;
    read_write_diff_ -= len;

    long long buf_size, long long cur_pos,
      long long beg_time, long long cur_time, long long end_time
    size_t size;
    full_ ? size = TOTAL_BUFFER_SIZE : size = writepos_;
    stream_info_(size, )

    abs_pos_ += len;
    readpos_ += len;
    read_write_diff_ -= len;

    if (playing_ && read_write_diff_ > 0)
    {
      trigger_send();
    }
  }
  catch (const std::exception& e)
  {
    LOG(WARNING) << "Error sending stream part to client: " << e.what();
    trigger_send();
  }
  catch (...)
  {
    LOG(WARNING) << "Unknown exception while sending stream part to client";
  }
}

void session::get_times(long long& bt, long long& ct, long long& et)
{
  if (pos_to_time_.size() == 1)
  {
    bt = ct = et = pos_to_time_.begin()->second;
  }
  else if (pos_to_time_.size() > 1)
  {
    bt = pos_to_time_.begin()->second;
    auto i = --pos_to_time_.end();
    et = i->second;
    i = pos_to_time_.lower_bound(abs_pos_);
    if (i == pos_to_time_.end())
    {
      ct = et;
    }
    else
    {
      ct = i->second;
    }
  }
  else
  {
    bt = ct = et = 0;
  }
}

void session::log_data()
{
  long long bt, ct, et;
  get_times(bt, ct, et);
  char btstr[10], ctstr[10], etstr[10];
  std::strftime(btstr, sizeof(btstr), "%H.%M.%S", std::localtime(&bt));
  std::strftime(ctstr, sizeof(ctstr), "%H.%M.%S", std::localtime(&ct));
  std::strftime(etstr, sizeof(etstr), "%H.%M.%S", std::localtime(&et));
  LOG(TRACE) << "Buffer times " << btstr << " " << ctstr << " " << etstr;
  LOG(TRACE) << "abs size=" << dec << abs_len_ << " abs pos=" << abs_pos_;
  size_t size;
  full_ ? size = TOTAL_BUFFER_SIZE : size = writepos_;
  LOG(TRACE) << "size=" << dec << size << " writepos=" << writepos_ << " readpos=" << readpos_ << " diff=" << read_write_diff_;
}

}
}
