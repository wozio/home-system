#ifndef SESSION_H
#define	SESSION_H

#include "ios_wrapper.h"

namespace home_system
{
namespace media
{

typedef std::function<void(int id, void* buf, size_t len, size_t buf_size, size_t buf_pos)> stream_callback_t;

class session_error
  : public std::runtime_error
{
public:
  session_error(const std::string& what_arg, int session)
  : runtime_error(what_arg),
    session_(session)
  {
  }
  
  int session() const
  {
    return session_;
  }
  
private:
  int session_;
};

class session
{
public:
  session(int id, stream_callback_t stream_callback);
  session(const session& orig) = delete;
  ~session();
  
  void stream_part(const void* buf, size_t length);
  // absolute position where 0 oldest position in buffer and current size is
  // latest position in buffer
  // returns current position in buffer
  size_t play();
  void pause();
  // returns current position in buffer after seek operation
  size_t seek(size_t pos);
  
private:
  int id_;
  stream_callback_t stream_callback_;
  
  bool playing_;
  size_t read_write_diff_; // how much writing is ahead of reading, should never be negative...
  size_t readpos_, writepos_;
  bool full_;
  std::fstream buffer_;
  
  ios_wrapper ios_;
  
  void trigger_send_some();
  void send_some();
  void send();
  
  std::mutex m_mutex;
};

}
}

#endif	/* SESSION_H */
