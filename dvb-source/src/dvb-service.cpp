#include <map>

#include "dvb-service.h"
#include "logger.h"
#include "yamicontainer.h"
#include "discovery.h"
#include "transponders.h"
#include "channels.h"

#include <fstream>

using namespace std;

namespace home_system
{
namespace media
{

dvb_service::dvb_service(const std::string& name,
    int adapter, int frontend,
    const std::string& transponder_file, const std::string& channel_file)
: service(name),
  tv_service_(false),
  dvb_(adapter, frontend, transponder_file, channel_file)
{
  DISCOVERY.subscribe(this);
  
  dvb_.set_ei_callback([this](const demux::event_info& ei){ on_ei(ei); });
  dvb_.channels().set_channel_callback([this](channel_event event, channel_t channel)
    { on_channel_change(event, channel); });
}

dvb_service::~dvb_service()
{
  DISCOVERY.unsubscribe(this);
}

void dvb_service::on_channel_change(channel_event event, channel_t c)
{
  if (event == channel_event::added)
  {
    send_channel(c);
  }
}

void dvb_service::on_ei(const demux::event_info& ei)
{
  if (tv_service_)
  {
    lock_guard<mutex> lock(ei_mutex_);
    if (!bundle_timer_.is_set())
    {
      bundle_timer_.set_from_now(1000, [this](){ on_bundle_timer(); });
    }
    bundle_.push_back(ei);
  }
}

void dvb_service::on_bundle_timer()
{
  try
  {
    lock_guard<mutex> lock(ei_mutex_);
    
    yami::parameters params;
    params.set_string("source", name());
      
    size_t bs = bundle_.size();
    
    if (bs > 0)
    {
      // channels
      std::vector<long long> c;
      c.reserve(bs);
      // events
      std::vector<int> eid;
      eid.reserve(bs);
      // titles and descriptions
      params.create_string_array("title", bs);
      params.create_string_array("description", bs);
      params.create_string_array("plot", bs);
      // start times
      std::vector<long long> st;
      st.reserve(bs);
      // durations
      std::vector<int> dur;
      dur.reserve(bs);

      for (size_t i = 0; i < bs; ++i)
      {
        c.push_back(bundle_[i].channel_);
        eid.push_back(bundle_[i].event_);
        params.set_string_in_array("title", i, bundle_[i].title_);
        params.set_string_in_array("description", i, bundle_[i].description_);
        params.set_string_in_array("plot", i, bundle_[i].plot_);
        st.push_back(bundle_[i].start_time_);
        dur.push_back(bundle_[i].duration_);
      }

      params.set_long_long_array_shallow("channel", &c[0], bs);
      params.set_long_long_array_shallow("start_time", &st[0], bs);
      params.set_integer_array_shallow("duration", &dur[0], bs);
      params.set_integer_array_shallow("event", &eid[0], bs);

      AGENT.send(DISCOVERY.get("tv"), "tv", "epg_data", params);
    }
    
    if (!bundle_timer_.is_set())
    {
      bundle_timer_.set_from_now(1000, [this](){ on_bundle_timer(); });
    }
    bundle_.clear();
  }
  catch (const service_not_found& e)
  {
    tv_service_ = false;
  }
  catch (const yami::yami_runtime_error& e)
  {
    tv_service_ = false;
  }
  catch (const std::exception& e)
  {
    LOG(WARNING) << "EXCEPTION: " << e.what();
  }
}

void dvb_service::on_remote_service_availability(const std::string& name, bool available)
{
  if (available)
  {
    if (name == "tv")
    {
      LOG(TRACE) << "TV service available";
      
      try
      {
        yami::parameters params;
        params.set_string("name", service::name());
        
        vector<long long> ids;
        dvb_.channels().get_channel_ids(ids);
        params.set_long_long_array("channel_ids", &ids[0], ids.size());
        
        params.create_string_array("channel_names", ids.size());
        
        for (size_t i = 0; i < ids.size(); ++i)
        {
          params.set_string_in_array("channel_names", i, dvb_.channels().get(ids[i])->get_name());
        }
        
        AGENT.send(DISCOVERY.get(name), name, "source_available", params);
        
        tv_service_ = true;
      }
      catch (const std::exception& e)
      {
        LOG(WARNING) << "EXCEPTION: " << e.what();
      }
    }
  }
  else
  {
    if (name == "tv")
    {
      tv_service_ = false;
      
      LOG(DEBUG) << "TV service not available";
    }
  }
}

void dvb_service::on_create_streaming_session(long long channel)
{
  
}

void dvb_service::on_delete_streaming_session(int session)
{
  dvb_.delete_session(session_);
}

void dvb_service::on_msg(yami::incoming_message & im)
{
  if (im.get_message_name() == "get_state")
  {
    yami::parameters params;
    //params.set_integer("state", fe_state_);
    im.reply(params);
  }
  else if (im.get_message_name() == "get_current_channel")
  {
    yami::parameters params;
    //params.set_long_long("current_channel", current_channel_);
    im.reply(params);
  }
  else if (im.get_message_name() == "get_channel_name")
  {
    //uint64_t channel = im.get_parameters().get_long_long("channel");
    
//    if (channels_.find(channel) != channels_.end())
//    {
//      yami::parameters params;
//      params.set_string("channel_name", channels_[channel].name_);
//      im.reply(params);
//    }
//    else
//    {
//      LOGWARN("get_channel_name for not known channel id: " << hex << channel);
//      im.reject("Unknown channel id");
//    }
  }
  else if (im.get_message_name() == "get_channels")
  {
//    for (map<uint64_t, channel>::iterator i = channels_.begin(); i != channels_.end(); ++i)
//    {
//      send_new_channel(i->first);
//    }
  }
  else if (im.get_message_name() == "set_channel")
  {
    //uint64_t channel = im.get_parameters().get_long_long("channel");
    
//    if (channels_.find(channel) != channels_.end())
//    {
//      set_channel(channel);
//    }
//    else
//    {
//      LOGWARN("set_channel for not known channel id: " << hex << channel);
//      im.reject("Unknown channel id");
//    }
  }
  else if (im.get_message_name() == "create_session")
  {
    long long channel = im.get_parameters().get_long_long("channel");
    destination_ = im.get_parameters().get_string("destination");
    endpoint_ = im.get_parameters().get_string("endpoint");
    
    LOG(DEBUG) << "Create streaming session request for channel " << channel;
    session_ = dvb_.create_session(dvb_.channels().get(channel),
      [this] (dvb::session_event_t event)
      {
        switch (event)
        {
          case dvb::session_event_t::ended:
            session_deleted();
            break;
        }
      },
      endpoint_);

    LOG(DEBUG) << "Session created " << session_;

    yami::parameters reply;
    reply.set_integer("session", 0);
    im.reply(reply);
  }
  else if (im.get_message_name() == "delete_session")
  {
    on_delete_streaming_session(0);
  }
  else
  {
    service::on_msg(im);
  }
}

void dvb_service::session_deleted()
{
  LOG(TRACE) << "Session deleted";
  
  try
  {
    yami::parameters params;
    params.set_integer("session", 0);

    YC.agent().send(endpoint_, destination_, "session_deleted", params);
  }
  catch (const exception& e)
  {
    LOG(WARNING) << "EXCEPTION: " << e.what();
  }
}

void dvb_service::send_channel(channel_t c)
{
  if (tv_service_)
  {
    try
    {
      yami::parameters params;
      params.set_long_long("channel", c->get_id());
      params.set_string("name", c->get_name());
      params.set_string("source", name());
      AGENT.send(DISCOVERY.get("tv"), "tv", "new_channel", params);
    }
    catch (const service_not_found& e)
    {
    }
    catch (const yami::yami_runtime_error& e)
    {
    }
    catch (const std::exception& e)
    {
      LOG(WARNING) << "EXCEPTION: " << e.what();
    }
  }
}

void dvb_service::print_channels()
{
  cout << "Local channels (" << dvb_.channels().size() << "): " << endl;
  dvb_.channels().print();
}

void dvb_service::print_transponders()
{
  cout << "Transponders (" << dvb_.transponders().size() << "): " << endl;
  dvb_.transponders().print();
}

void dvb_service::print_current_transponder()
{
  cout << "Current transponder: " << dvb_.transponders().current() << endl;
}

}
}
