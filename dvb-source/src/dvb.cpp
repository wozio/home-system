#include "dvb.h"

#include "logger.h"
#include <mutex>
#include <iostream>

using namespace home_system::media;
using namespace std;

namespace dvb
{

dvb::dvb(int adapter, int frontend,
  const std::string& transponder_file, const std::string& channel_file)
: transponders_(transponder_file),
  channels_(channel_file, transponders_),
  frontend_(adapter, frontend),
  demux_(adapter, frontend, channels_, transponders_),
  state_(state::idle)
{
  demux_.register_event_callback([this](demux_event event){ on_demux_event(event); });
  demux_.set_ei_callback([this](const demux::event_info& ei){ on_ei(ei); });
  frontend_.register_state_callback([this](frontend_state state){ on_frontend_state_change(state); });
    
  start_idle_scan();
}

dvb::~dvb()
{
  stop_idle_scan();
  
  {
    //lock_guard<mutex> lock(ei_callback_mutex_);
    ei_callback_ = nullptr;
  }
  
  //lock_guard<mutex> lock(state_mutex_);
  
  state_ = state::idle;
  sessions_.clear();
  timer_.cancel();
}

void dvb::set_ei_callback(ei_callback_t callback)
{
  lock_guard<mutex> lock(ei_callback_mutex_);
  ei_callback_ = callback;
}

home_system::media::channels& dvb::channels()
{
  return channels_;
}

home_system::media::transponders& dvb::transponders()
{
  return transponders_;
}

size_t dvb::create_session(home_system::media::channel_t c,
    session_callback_t session_callback,
    session_stream_part_callback_t stream_part_callback)
{
  LOG(DEBUG) << "Creating session";
  
  stop_idle_scan();
  
  //lock_guard<mutex> lock(state_mutex_);

  state_ = state::busy;
  session_t s(new session(c, session_callback, stream_part_callback,
    frontend_, demux_, transponders_));
  // find free id
  size_t sid = 0;
  while (sessions_.find(sid) != sessions_.end())
  {
    sid++;
  }
  sessions_.emplace(sid, std::move(s));
  return sid;
}

void dvb::delete_session(size_t s)
{
  LOG(DEBUG) << "Deleting session";
  {
    //lock_guard<mutex> lock(state_mutex_);

    sessions_.erase(s);
  }
  if (sessions_.size() == 0 && state_ != state::scan)
  {
    state_ = state::idle;
    start_idle_scan();
  }
}

void dvb::start_idle_scan()
{
  //lock_guard<mutex> lock(state_mutex_);
  
  if (state_ == state::idle)
  {
    if (transponders_.size() > 0)
    {
      LOG(DEBUG) << "Starting idle scan";
      
      demux_.reset_mux();
      state_ = state::scan;
      frontend_.set_transponder(transponders_.current());
      timer_.set_from_now(25000, [this] (){idle_scan_timeout();});
    }
  }
}

void dvb::idle_scan_timeout()
{
  //lock_guard<mutex> lock(state_mutex_);
  
  if (state_ == state::scan)
  {
    LOG(DEBUG) << "Continuing idle scan, setting next transponder";
    demux_.reset_mux();
    frontend_.set_transponder(transponders_.next());
    timer_.set_from_now(25000, [this] (){idle_scan_timeout();});
  }
}

void dvb::stop_idle_scan()
{
  //lock_guard<mutex> lock(state_mutex_);
  
  if (state_ == state::scan)
  {
    LOG(DEBUG) << "Stopping idle scan";
    
    state_ = state::idle;
    timer_.cancel();
    demux_.reset_mux();
  }
}

void dvb::on_frontend_state_change(frontend_state newstate)
{
  switch (state_)
  {
  case state::scan:
    if (newstate == frontend_state::tuned)
    {
      demux_.set_mux();
    }
    break;
  default:
    break;
  }
}

void dvb::on_demux_event(demux_event event)
{
}

void dvb::on_ei(const demux::event_info& ei)
{
  lock_guard<mutex> lock(ei_callback_mutex_);
  
  if (ei_callback_ != nullptr)
  {
    ei_callback_(ei);
  }
}

}
