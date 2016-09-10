#include "epg.h"
#include "logger.h"
#include <deque>

using namespace std;

namespace home_system
{
namespace media
{

epg::epg(db& db)
: db_(db)
{
}

epg::~epg()
{
}

void epg::handle_epg_data(const yami::parameters& params)
{
  string source = params.get_string("source");

  size_t cs, s, ds, sts;
  long long* channels = params.get_long_long_array("channel", cs);
  //LOG("cs=" << cs);
  int* events = params.get_integer_array("event", s);
  //LOG("s=" << s);
  int* durations = params.get_integer_array("duration", ds);
  //LOG("ds=" << ds);
  long long* start_times = params.get_long_long_array("start_time", sts);
  //LOG("sts=" << sts);

  if (!(cs == s && s == ds && ds == sts))
  {
    throw runtime_error("Incorrect data in epg_data message (wrong sizes)");
  }

  //LOG(TRACE) << "Received information for " << cs << " events";

  for (size_t i = 0; i < s; ++i)
  {
    int gc = db_.get_channel_from_source_local(source, channels[i]);
    if (gc != -1)
    {
      if (!channels_[gc].events_.count(events[i]))
      {
        event_info ei;
        ei.event_id_ = events[i];
        ei.title_ = params.get_string_in_array("title", i);
        ei.duration_ = durations[i];
        ei.start_time_ = start_times[i];
        ei.description_ = params.get_string_in_array("description", i);
        ei.plot_ = params.get_string_in_array("plot", i);

        channels_[gc].events_[ei.event_id_] = ei;
      }
    }
  }
}

void epg::handle_get_epg_info(yami::incoming_message& im)
{
  vector<int> channels_found;
  channels_found.reserve(channels_.size());
  time_t time_begin = im.get_parameters().get_long_long("time_begin");
  time_t time_end = im.get_parameters().get_long_long("time_end");
  for (const auto& ci : channels_)
  {
    for (const auto& ei : ci.second.events_)
    {
      if ((ei.second.start_time_ < time_end) && (ei.second.start_time_ + ei.second.duration_ > time_begin))
      {
        channels_found.push_back(ci.first);
        break;
      }
    }
  }
  yami::parameters params;
  params.set_integer_array_shallow("channels", &channels_found[0], channels_found.size());
  im.reply(params);
}

void epg::handle_get_epg_data(yami::incoming_message& im)
{
  int gc = im.get_parameters().get_integer("channel");
  LOG(TRACE) << "Get EPG data for " << gc;
  size_t s = channels_[gc].events_.size();
  vector<int> ids;
  ids.reserve(s);
  vector<long long> start_times;
  start_times.reserve(s);
  vector<int> durations;
  durations.reserve(s);

  LOG(TRACE) << "have something " << s;

  std::map<int, event_info>::iterator it = channels_[gc].events_.begin();

  for (size_t i = 0; i < s; ++i)
  {
    ids.push_back(it->second.event_id_);
    durations.push_back(it->second.duration_);
    start_times.push_back(it->second.start_time_);
    ++it;
  }

  yami::parameters params;
  params.set_integer("channel", gc);
  params.set_integer("event_num", ids.size());

  if (ids.size() > 0)
  {
    params.set_integer_array_shallow("id", &ids[0], ids.size());
    params.set_integer_array_shallow("duration", &durations[0], durations.size());
    params.set_long_long_array_shallow("start_time", &start_times[0], start_times.size());

    params.create_string_array("name", ids.size());
    params.create_string_array("description", ids.size());
    params.create_string_array("plot", ids.size());
    it = channels_[gc].events_.begin();
    for (size_t i = 0; i < ids.size(); ++i)
    {
      params.set_string_in_array("name", i, it->second.title_);
      params.set_string_in_array("description", i, it->second.description_);
      params.set_string_in_array("plot", i, it->second.plot_);
      ++it;
    }
  }
  
  im.reply(params);
}

}
}
