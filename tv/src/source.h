#ifndef SOURCE_H
#define	SOURCE_H

#include "db.h"
#include "session.h"
#include <string>
#include <map>
#include <memory>

namespace home_system
{
namespace media
{

class source;

typedef std::shared_ptr<source> source_t;
typedef std::shared_ptr<session> session_t;

class source
: public std::enable_shared_from_this<source>
{
public:
  source(db& db, const std::string& name, const std::string& ye);
  source(const source& orig) = delete;
  ~source();
  
  void not_available();
  
  int create_session(int channel, const std::string& client);
  void delete_session(int client_session);

  void stream_part(int source_session, const void* buf, size_t len);
  
  void session_deleted(int source_session);
  
  static source_t source_for_session(int client_session);
  
  session_t get_session(int s);
  
  std::string endpoint();
  
private:
  db& db_;
  std::string name_, ye_;
  int source_session_id_;
  int client_session_id_;
  session_t client_session_;
  
  static std::map<int, source_t> _client_session_ids;
  
  void delete_source_session();
};

}
}

#endif	/* SOURCE_H */

