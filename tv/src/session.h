#ifndef SESSION_H
#define	SESSION_H

#include "ios_wrapper.h"

namespace home_system
{
namespace media
{

typedef std::function<void(int id, void* buf, size_t len, size_t buf_size)> stream_callback_t;

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
  void play();
  void pause();
  void seek(long long pos);
  
private:
  int id_;
  stream_callback_t stream_callback_;
  
  bool playing_;
  std::streampos readpos_, writepos_;
  bool full_;
  std::fstream buffer_;
  
  ios_wrapper ios_;
  
  void trigger_send_some();
  void send_some();
  void send();
  
  std::streampos size();
  std::streampos read_pos();
  std::streampos write_pos();
  
  std::mutex m_mutex;
};

}
}

#endif	/* SESSION_H */
