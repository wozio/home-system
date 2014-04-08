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
    home_system::media::frontend& frontend, home_system::media::demux& demux,
    home_system::media::transponders& transponders);
  session(const session& orig) =  delete;
  ~session();
  
  void on_frontent_state_change(home_system::media::frontend_state newstate);
  void on_demux_state_change(home_system::media::demux_state newstate);
  
private:
  home_system::media::frontend& frontend_;
  home_system::media::demux& demux_;
  home_system::media::transponders& transponders_;
  
  home_system::media::channel_t channel_;
  
  dvb::session_callback_t callback_;
};

}

#endif	/* SESSION_H */

