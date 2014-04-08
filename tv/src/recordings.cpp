#include "recordings.h"
#include "db.h"
#include "logger.h"
#include "yamicontainer.h"

#define RECMARGIN 300

using namespace std;

namespace home_system
{
namespace media
{

recordings::recordings()
{
  timer_.set_from_now(1000, [this] () {on_timeout();});
}

recordings::~recordings()
{
}

void recordings::on_timeout()
{
  check();
  timer_.set_from_now(1000, [this] () {on_timeout();});
}

void recordings::check()
{
  //LOG("Checking...");
  vector<int> recordings;
  //DB.get_recordings_to_start(recordings);

  for (size_t i = 0; i < recordings.size(); ++i)
  {
    start(recordings[i]);
  }
  
  recordings.clear();
  //DB.get_recordings_to_stop(recordings);

  for (size_t i = 0; i < recordings.size(); ++i)
  {
    start(recordings[i]);
  }
}

void recordings::start(int recording_id)
{
//  vector<string> sources;
//  vector<long long> local_channels;
//  int global_channel = db_.get_channel_for_recording(recording_id);
//  db_.get_sources_local_channels(global_channel, sources, local_channels);
//  if (sources.size() > 0)
//  {
//    // TODO: make it search for available sources and wake up first if none available
//    if (available_.find(sources[0]) != available_.end())
//    {
//      LOG("Starting recording: recording_id=" << recording_id);
//      try
//      {
//        // create streaming session
//        yami::parameters params;
//
//        params.set_long_long("local_channel", local_channels[0]);
//        params.set_string("destination", "tv");
//        params.set_string("endpoint", YC.endpoint());
//
//        LOG("Sending create_streaming_session to " << sources[0]);
//        auto_ptr<yami::outgoing_message> message(AGENT.send(DISCOVERY.get(sources[0]), sources[0], "create_streaming_session", params));
//
//        message->wait_for_completion(1000);
//
//        if (message->get_state() == yami::replied)
//        {
//          LOG("Got session=" << message->get_reply().get_integer("session"));
//          int session_id = message->get_reply().get_integer("session");
//          db_.update_recording(recording_id, STATE_ONGOING, sources[0], session_id);
//
//          // starting streaming session
//          mutex::scoped_lock lock(session_mutex_);
//          shared_ptr<streaming_session> ss(new streaming_session(time(NULL), channels_[global_channel].name_));
//          sessions_[session_id] = ss;
//        }
//      }
//      catch (const std::exception& e)
//      {
//        LOGWARN("EXCEPTION: " << e.what())
//      }
//    } else
//    {
//      LOG("Sending wakeup for recording: recording_id=" << recording_id);
//      // sending Wake On Lan message
//      // TODO make it on separate function, maybe move to control-server
//      string strmac("000C7620C5E0");
//      unsigned int mac[6];
//      for (size_t j = 0; j < 6; j++)
//      {
//        std::stringstream s;
//        s << hex << strmac.substr(j * 2, 2);
//        s >> mac[j];
//      }
//
//      uint8_t mp[108];
//      size_t j = 0;
//      for (; j < 6; j++)
//        mp[j] = 0xFF;
//      while (j < 102)
//      {
//        for (size_t i = 0; i < 6; ++i)
//        {
//          mp[j++] = mac[i];
//        }
//      }
//
//      ip::udp::endpoint endpoint(ip::address::from_string("255.255.255.255"), 8);
//      ip::udp::socket socket(io_service_, endpoint.protocol());
//      socket.set_option(ip::udp::socket::broadcast(true));
//      socket.send_to(buffer(mp), endpoint);
//    }
//  }
}

void recordings::stop(int recording_id)
{
//  try
//  {
//    string source;
//    int session_id = -1;
//
//    db_.session() << "SELECT source, session FROM recordings WHERE recording_id = :recording_id",
//      into(source), into(session_id), use(recording_id), now;
//
//    LOG("Stopping recording: recording_id=" << recording_id);
//
//    yami::parameters params;
//    params.set_integer("session", session_id);
//    AGENT.send(DISCOVERY.get(source), source, "delete_streaming_session", params);
//
//    sessions_.erase(session_id);
//  } catch (const std::exception& e)
//  {
//    LOGWARN("EXCEPTION: " << e.what())
//  }
//  db_.session() << "UPDATE recordings SET state = :state WHERE recording_id = :recording_id",
//    use(STATE_FINISHED), use(recording_id), now;
}

}
}

