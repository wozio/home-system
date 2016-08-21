#ifndef SESSION_H
#define	SESSION_H

#include "ios_wrapper.h"
#include <string>
#include <stdexcept>
#include <fstream>

namespace home_system
{
namespace media
{

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
  session(int id, std::string destination);
  session(const session& orig) = delete;
  ~session();
  
  void stream_part(const void* buf, size_t length);
  void play();
  void pause();
  void seek(long long pos);
  
private:
  int id_;
  int channel_;
  std::string destination_;
  
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
