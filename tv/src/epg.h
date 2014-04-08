#ifndef EPG_H
#define	EPG_H

#include "event_info.h"
#include "db.h"
#include <yami4-cpp/incoming_message.h>
#include <map>

namespace home_system
{
namespace media
{

class epg
{
public:
  epg(db& db);
  epg(const epg& orig) = delete;
  ~epg();
  
  void handle_epg_data(const yami::parameters& params);
  void handle_get_epg_info(yami::incoming_message& im);
  void handle_get_epg_data(yami::incoming_message& im);
  
private:
  db& db_;
  
  struct channel
  {
    std::string name_;
    int id_;
    std::map<int, event_info> events_; // key is event_id unique within channel
  };
  // key is channel id
  std::map<int, channel> channels_;
};

}
}

#endif	/* EPG_H */

