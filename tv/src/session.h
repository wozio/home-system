#ifndef SESSION_H
#define	SESSION_H

#include "sources.h"
#include <string>

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
  session(sources& sources, int id, int channel, std::string endpoint, std::string destination);
  session(const session& orig) = delete;
  ~session();
  
private:
  sources& sources_;
  int id_;
  int channel_;
  std::string endpoint_;
  std::string destination_;
  source_t source_;
  
  void handle_stream_part(const void* buf, size_t length);
};

}
}

#endif	/* SESSION_H */

