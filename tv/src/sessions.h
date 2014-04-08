#ifndef SESSIONS_H
#define	SESSIONS_H

#include "sources.h"
#include "session.h"
#include <map>
#include <memory>

namespace home_system
{
namespace media
{

class sessions
{
public:
  sessions(sources& sources);
  sessions(const sessions& orig) = delete;
  ~sessions();
  
  int handle_create_client_session(int channel, const std::string& endpoint, const std::string& destination);
  void handle_delete_client_session(int session);
  
private:
  sources& sources_;
  
  std::map<int, std::shared_ptr<session>> sessions_;
};

}
}

#endif	/* SESSIONS_H */

