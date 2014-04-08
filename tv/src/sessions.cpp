#include "sessions.h"

using namespace std;

namespace home_system
{
namespace media
{

sessions::sessions(sources& sources)
: sources_(sources)
{
}

sessions::~sessions()
{
}

int sessions::handle_create_client_session(int channel, const std::string& endpoint, const std::string& destination)
{
  // finding free session id
  int id = 0;
  while (true)
  {
    if (sessions_.count(id) == 0)
    {
      break;
    }
    ++id;
  }
  
  // creating session
  
  sessions_[id] = make_shared<session>(sources_, id, channel, endpoint, destination);
  
  return id;
}

void sessions::handle_delete_client_session(int session)
{
  sessions_.erase(session);
}

}
}

