#ifndef TV_SERVICE_H
#define	TV_SERVICE_H

#include "service.h"
#include "recordings.h"
#include "db.h"
#include "epg.h"
#include "sources.h"
#include <boost/shared_ptr.hpp>
#include <vector>
#include <set>
#include <string>
#include <fstream>

namespace home_system
{
namespace media
{

class tv_service
: public home_system::service
{
public:
  tv_service(db& db);
  ~tv_service();

  void on_msg(yami::incoming_message& im);
  

private:
  
  void handle_source_available(yami::incoming_message& im);
  void handle_new_channel(yami::incoming_message& im);
  void handle_get_epg_data(yami::incoming_message& im);
  void handle_get_channels(yami::incoming_message& im);
  void handle_schedule_event_record(yami::incoming_message& im);
  
  db& db_;
  sources sources_;
  recordings recordings_;
  epg epg_;

//  boost::mutex session_mutex_;
//  std::map<int, boost::shared_ptr<streaming_session> > sessions_;
};

}
}


#endif	/* TV_SERVICE_H */

