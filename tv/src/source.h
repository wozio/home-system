#ifndef SOURCE_H
#define	SOURCE_H

#include "db.h"
#include <string>
#include <functional>

namespace home_system
{
namespace media
{

class source
{
public:
  source(db& db, const std::string& name, const std::string& ye);
  source(const source& orig) = delete;
  ~source();
  
  typedef std::function<void (const void*, size_t)> stream_part_callback_t;
  void create_session(int channel, stream_part_callback_t stream_part_callback);
  void delete_session();
  
  void handle_stream_part(int server_session, const void* buf, size_t length);
  void handle_session_deleted(int server_session);
  
  std::string endpoint();
  
private:
  db& db_;
  std::string name_, ye_;
  int source_session_id_;
  
  stream_part_callback_t stream_part_callback_;
};

}
}

#endif	/* SOURCE_H */

