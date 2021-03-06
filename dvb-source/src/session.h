#ifndef SESSION_H
#define	SESSION_H

#include "demux.h"
#include "frontend.h"
#include "transponders.h"
#include <memory>

namespace dvb
{

class session
{
public:
  session(home_system::media::channel_t c, dvb::session_callback_t callback,
    dvb::session_stream_part_callback_t stream_part_callback,
    home_system::media::frontend& frontend, home_system::media::demux& demux,
    home_system::media::transponders& transponders);
  session(const session& orig) =  delete;
  ~session();
  
  void on_frontend_state_change(home_system::media::frontend_state newstate);
  void on_demux_state_change(home_system::media::demux_event newstate);
  
private:
  home_system::media::frontend& frontend_;
  int frontend_callback_id_;
  home_system::media::demux& demux_;
  int demux_callback_id_;
  home_system::media::transponders& transponders_;
  
  home_system::media::channel_t channel_;
  
  dvb::session_callback_t callback_;
  dvb::session_stream_part_callback_t stream_part_callback_;
};

}

#endif	/* SESSION_H */

