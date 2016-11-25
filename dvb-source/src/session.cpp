#include "session.h"
#include "logger.h"
#include "session_callback_t.h"
#include "demux.h"
#include "frontend.h"

using namespace home_system::media;

namespace dvb
{

session::session(home_system::media::channel_t c, dvb::session_callback_t callback,
    const std::string& endpoint,
    home_system::media::frontend& frontend, home_system::media::demux& demux,
    home_system::media::transponders& transponders)
: frontend_(frontend),
  demux_(demux),
  transponders_(transponders),
  channel_(c),
  callback_(callback),
  endpoint_(endpoint)
{
  LOG(DEBUG) << "Creating session for channel " << c->get_name();
  demux_callback_id_ = demux_.register_event_callback([this](demux_event state){
    this->on_demux_state_change(state);
  });
  frontend_callback_id_ = frontend_.register_state_callback([this](frontend_state state){
    this->on_frontend_state_change(state);
  });
  demux_.reset_mux();
  LOG(DEBUG) << "Tuning to: " << *(channel_->get_transponder());
  transponders_.set(channel_->get_transponder());
  frontend_.set_transponder(transponders_.current());
  // now wait for frontend to tune to new transponder
  callback_(session_event_t::starting);
}

session::~session()
{
  LOG(DEBUG) << "Destroying session " << channel_->get_name();
  demux_.unregister_event_callback(demux_callback_id_);
  demux_.reset_mux();
  frontend_.unregister_state_callback(frontend_callback_id_);
  callback_(session_event_t::ended);
}

void session::on_frontend_state_change(home_system::media::frontend_state newstate)
{
  LOG(DEBUG) << "Frontend state change to: " << newstate;
  if (newstate == frontend_state::tuned)
  {
    demux_.set_channel(channel_, endpoint_);
  }
}

void session::on_demux_state_change(home_system::media::demux_event newstate)
{
  LOG(DEBUG) << "Demux state change to: " << newstate;
}

}
