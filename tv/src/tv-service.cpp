#include "tv-service.h"
#include "logger.h"
#include "yamicontainer.h"
#include "epg.h"
#include "discovery.h"
#include <sstream>

using namespace std;
using namespace Poco::Data;
using namespace boost::asio;
using namespace boost::posix_time;
using namespace boost;

#define STATE_NOTSTARTED 0
#define STATE_WAKEUPSENT 1
#define STATE_ONGOING 2
#define STATE_FINISHED 3

#define NO_DURATION -1

namespace home_system
{
namespace media
{

tv_service::tv_service(db& db)
: service("tv", false),
  db_(db),
  sources_(db_),
  epg_(db_)
{
  init();
  
  DISCOVERY.subscribe([&] (const std::string& service, bool available)
  {
    if (!available)
    {
      if (sources_.check_source(service))
      {
        LOG("Source not available: " << service);
        sources_.source_not_available(service);
      }
    }
  });
}

tv_service::~tv_service()
{
  deinit();
}

void tv_service::handle_source_available(yami::incoming_message& im)
{
  string source = im.get_parameters().get_string("name");
  string ye = im.get_source();
  size_t s = 0;
  long long* ids = im.get_parameters().get_long_long_array("channel_ids", s);
  
  vector<string> names;
  names.reserve(s);
  
  for (size_t i = 0; i < s; ++i)
  {
    names.push_back(im.get_parameters().get_string_in_array("channel_names", i));
  }
  
  LOG("Source available: " << source << "("<< ye << ")");
  sources_.source_available(source, ye);
  db_.check_and_add_local_channels(source, s, ids, names);
  //RECORDINGS.check();
}

void tv_service::handle_new_channel(yami::incoming_message& im)
{
  long long local = im.get_parameters().get_long_long("channel");
  string source = im.get_parameters().get_string("source");
  string name = im.get_parameters().get_string("name");
  db_.check_and_add_local_channel(source, local, name);
}

void tv_service::handle_get_channels(yami::incoming_message& im)
{
  vector<int> gcs;
  vector<string> names;
  db_.get_channels(gcs, names);

  yami::parameters params;
  params.create_string_array("name", names.size());

  for (size_t i = 0; i < names.size(); ++i)
    params.set_string_in_array("name", i, names[i]);

  params.set_integer_array_shallow("channel", &gcs[0], gcs.size());
  im.reply(params);
}

void tv_service::handle_schedule_event_record(yami::incoming_message& im)
{
//  yami::parameters params;
//  // here we get global event id
//  int global_channel = im.get_parameters().get_integer("channel");
//  if (channels_.count(global_channel))
//  {
//    int event_id = im.get_parameters().get_integer("id");
//    if (channels_[global_channel].events_.count(event_id))
//    {
//      
//      
//      int recording_id = db_.create_recording(global_channel, channels_[global_channel].events_[event_id]);
//      
//      params.set_integer("recording_id", recording_id);
//
//      //check_recordings();
//    }
//  }
//  im.reply(params);
}

void tv_service::on_msg(yami::incoming_message& im)
{
  //LOG("message: " << im.get_message_name());
  try
  {
    if (im.get_message_name() == "stream_part")
    {
      int source_session = im.get_parameters().get_integer("session");
      string source = im.get_parameters().get_string("source");
      size_t length;
      const void* buf = im.get_parameters().get_binary("payload", length);
      sources_[source]->stream_part(source_session, buf, length);
    }
    else if (im.get_message_name() == "pause_session")
    {
      int s = im.get_parameters().get_integer("session");
      source::source_for_session(s)->get_session(s)->pause();
    }
    else if (im.get_message_name() == "play_session")
    {
      int s = im.get_parameters().get_integer("session");
      source::source_for_session(s)->get_session(s)->play();
    }
    else if (im.get_message_name() == "seek_session")
    {
      int s = im.get_parameters().get_integer("session");
      long long position = im.get_parameters().get_long_long("position");
      source::source_for_session(s)->get_session(s)->seek(position);
    }
    else if (im.get_message_name() == "hello")
    {
      LOG("Some client said hello, waking up sources");
      // sending Wake On Lan message
      // TODO make it on separate function, maybe move to control-server
      string strmac("000C7620C5E0");
      unsigned int mac[6];
      for (size_t j = 0; j < 6; j++)
      {
        std::stringstream s;
        s << hex << strmac.substr(j * 2, 2);
        s >> mac[j];
      }

      uint8_t mp[108];
      size_t j = 0;
      for (; j < 6; j++)
        mp[j] = 0xFF;
      while (j < 102)
      {
        for (size_t i = 0; i < 6; ++i)
        {
          mp[j++] = mac[i];
        }
      }
      
      io_service io_service;
      ip::udp::endpoint endpoint(ip::address::from_string("255.255.255.255"), 8);
      ip::udp::socket socket(io_service, endpoint.protocol());
      socket.set_option(ip::udp::socket::broadcast(true));
      socket.send_to(buffer(mp), endpoint);
    }
    else if (im.get_message_name() == "epg_data")
    {
      epg_.handle_epg_data(im.get_parameters());
    }
    else if (im.get_message_name() == "create_session")
    {
      try
      {
        int channel = im.get_parameters().get_integer("channel");
        string endpoint = im.get_parameters().get_string("endpoint");
        string destination = im.get_parameters().get_string("destination");
        
        LOG("Creating session for channel " << channel << "(" << db_.get_channel_name(channel) << ") to " << destination << "(" << endpoint << ")");

        int session = sources_.create_session(channel, endpoint, destination);

        yami::parameters reply;
        reply.set_integer("session", session);
        im.reply(reply);
      }
      catch (const source_not_found& e)
      {
        LOGWARN("Source not found");
        yami::parameters reply;
        reply.set_integer("session", -1);
        im.reply(reply);
      }
    }
    else if (im.get_message_name() == "delete_session")
    {
      int session = im.get_parameters().get_integer("session");
      sources_.delete_session(session);
    }
    else if (im.get_message_name() == "get_epg_info")
    {
      epg_.handle_get_epg_info(im);
    }
    else if (im.get_message_name() == "get_epg_data")
    {
      epg_.handle_get_epg_data(im);
    }
    else if (im.get_message_name() == "source_available")
    {
      handle_source_available(im);
    }
    else if (im.get_message_name() == "new_channel")
    {
      handle_new_channel(im);
    }
    else if (im.get_message_name() == "get_channels")
    {
      handle_get_channels(im);
    }
    else if (im.get_message_name() == "schedule_event_record")
    {
      handle_schedule_event_record(im);
    }
    else if (im.get_message_name() == "start_recording")
    {
//      int global_channel = im.get_parameters().get_integer("channel_id");
//      if (db_.check_channel_existence(global_channel))
//      {
//        int recording_id = db_.create_recording(global_channel, time(NULL), NO_DURATION);
//        yami::parameters params;
//        params.set_integer("recording_id", recording_id);
//        im.reply(params);
//
//        start_recording(recording_id);
//      }
//      else
//      {
//        im.reject("channel not found in the database");
//      }
    }
    else if (im.get_message_name() == "stop_recording")
    {
//      int recording_id = im.get_parameters().get_integer("recording_id");
//      if (db_.get_recording_state(recording_id) == STATE_ONGOING)
//      {
//        stop_recording(recording_id);
//      }
//      else
//      {
//        im.reject("recording not found in the database");
//      }
    }
    else
    {
      service::on_msg(im);
    }
  }
  catch (const std::exception& e)
  {
    LOGWARN("Exception: " << e.what());
    im.reject(e.what());
  }
}

}
}
