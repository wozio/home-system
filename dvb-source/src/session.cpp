#include "session.h"
#include "logger.h"

using namespace home_system::media;

namespace dvb
{

session::session(home_system::media::channel_t c, dvb::session_callback_t callback,
    dvb::session_stream_part_callback_t stream_part_callback,
    home_system::media::frontend& frontend, home_system::media::demux& demux,
    home_system::media::transponders& transponders)
: frontend_(frontend),
  demux_(demux),
  transponders_(transponders),
  channel_(c),
  callback_(callback),
  stream_part_callback_(stream_part_callback)
{
  LOG(DEBUG) << "Creating session for channel " << c->get_name();
  demux_.reset_mux();
  if (channel_->get_transponder() == frontend_.get_transponder() && frontend_.get_state() == frontend_state::tuned)
  {
    LOG(DEBUG) << "Tuned to: " << *(frontend_.get_transponder());
    demux_.set_channel(channel_, stream_part_callback_);
  }
  else
  {
    LOG(DEBUG) << "Tuning to: " << *(channel_->get_transponder());
    transponders_.set(channel_->get_transponder());
    frontend_.set_transponder(transponders_.current());
    // now wait for frontend to tune to new transponder
  }
}

session::~session()
{
  LOG(DEBUG) << "Destroying session " << channel_->get_name();
  demux_.reset_mux();
  if (callback_)
  {
    callback_(session_event_t::ended);
  }
}

void session::on_frontent_state_change(home_system::media::frontend_state newstate)
{
  if (newstate == frontend_state::tuned)
  {
    demux_.set_channel(channel_, stream_part_callback_);
  }
}

void session::on_demux_state_change(home_system::media::demux_state newstate)
{
}

}
