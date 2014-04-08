#ifndef DVBSERVICE_H
#define	DVBSERVICE_H

#include "service.h"
#include "session_t.h"
#include "dvb.h"
#include <string>
#include <deque>
#include <map>
#include <set>

struct section;

namespace home_system
{
namespace media
{

class dvb_service
: public home_system::service
{
public:
  dvb_service(const std::string& name,
    int adapter, int frontend,
    const std::string& transponder_file, const std::string& channel_file);
  ~dvb_service();
  
  void print_channels();
  void print_transponders();
  void print_current_transponder();
  
  
  
private:
  void on_msg(yami::incoming_message & im);
  void on_remote_service_availability(const std::string& name, bool available);
  
  void on_ei(const demux::event_info& ei);
  void on_channel_change(channel_event event, channel_t);
  void send_channel(channel_t c);
  bool tv_service_;
  
  dvb::dvb dvb_;
  
  std::deque<demux::event_info> bundle_;
  timer bundle_timer_;
  void on_bundle_timer();
  
  dvb::session_t session_;
  std::string endpoint_, destination_;
  bool exception_handled_;
  void on_stream_part(size_t size, char* buffer);
  
  void delete_session();
};

}
}

#endif	/* DVBSERVICE_H */

