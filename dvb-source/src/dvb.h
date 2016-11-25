#ifndef DVB_H
#define	DVB_H

#include "transponders.h"
#include "channels.h"
#include "frontend.h"
#include "demux.h"
#include "timer.h"
#include "channel_t.h"
#include "session_callback_t.h"
#include "session.h"
#include "session_t.h"
#include <functional>
#include <set>

namespace dvb
{

class dvb
{
public:
  dvb(int adapter, int frontend,
    const std::string& transponder_file, const std::string& channel_file);
  ~dvb();
  
  typedef std::function<void (const home_system::media::demux::event_info& ei)> ei_callback_t;
  void set_ei_callback(ei_callback_t callback);
  
  home_system::media::channels& channels();
  home_system::media::transponders& transponders();
  
  size_t create_session(home_system::media::channel_t c,
    session_callback_t session_callback, const std::string& endpoint);
  void delete_session(size_t s);
  
private:
  
  home_system::media::transponders transponders_;
  home_system::media::channels channels_;
  home_system::media::frontend frontend_;
  home_system::media::demux demux_;
  
  enum class state
  {
    idle,
    scan,
    busy
  } state_;
  
  std::mutex state_mutex_;
  
  std::map<size_t, session_t> sessions_;
  
  home_system::timer timer_;
  void start_idle_scan();
  void idle_scan_timeout();
  void stop_idle_scan();
  
  void on_frontend_state_change(home_system::media::frontend_state newstate);
  void on_demux_event(home_system::media::demux_event event);
  
  ei_callback_t ei_callback_;
  void on_ei(const home_system::media::demux::event_info& ei);
  std::mutex ei_callback_mutex_;
};

}

#endif	/* DVB_H */

