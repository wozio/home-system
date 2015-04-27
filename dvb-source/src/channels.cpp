#include "channels.h"
#include "transponder_configuration_exception.h"
#include <logger.h>
#include <boost/algorithm/string.hpp>
#include <boost/lexical_cast.hpp>
#include <fstream>
#include <stdexcept>

using namespace std;

namespace home_system
{
namespace media
{

channels::channels(const std::string& channels_file, transponders& to)
: transponders_(to),
  current_(channels_.begin()),
  channels_file_(channels_file)
{
  // parsing channels file for creating channel for each entry
  LOG("Channel definition file: " << channels_file);
  
  enum class state_t
  {
    transponder,
    name,
    id
  } state = state_t::transponder;
  
  ifstream f(channels_file);
  string line;
  shared_ptr<transponder> t;
  string cname;
  uint64_t cid;
  shared_ptr<channel> c;
  while (f.good())
  {
    getline(f, line);
    if (line.size())
    {
      switch (state)
      {
      case state_t::transponder:
      {
        try
        {
          t = transponders_.find_or_create(line);
        }
        catch (const transponder_configuration_exception& e)
        {
          LOGWARN("Creating transponder failed: " << e.what());
        }

        state = state_t::name;
        break;
      }
      case state_t::name:
        cname = line;
        
        state = state_t::id;
        break;
      case state_t::id:
        try
        {
          cid = boost::lexical_cast<uint64_t>(line);
          channels_[cid] = make_shared<channel>(cid, cname, t);
        }
        catch (const boost::bad_lexical_cast& e)
        {
          LOGWARN("Creating channel failed: " << e.what());
        }
        
        state = state_t::transponder;
        break;
      }
    }
  }
  LOGINFO("Number of defined channels: " << channels_.size());
}

channels::~channels()
{
  LOG("Saving channels list");
  
  ofstream f(channels_file_, ios_base::trunc);
  for (auto c : channels_)
  {
    c.second->save(f);
  }
}

void channels::set_channel_callback(channel_callback_t callback)
{
  channel_callback_ = callback;
}

channel_t channels::get(uint64_t id)
{
  auto c = channels_.find(id);
  if (c == channels_.end())
    throw runtime_error("Channel not found");
  return c->second;
}

void channels::add(uint64_t id, const std::string& name, uint16_t service_id)
{
  channel_t nc = make_shared<channel>(id, name, transponders_.current(), service_id);
  channels_[id] = nc;
  transponders_.current()->add_channel(nc);
  if (channel_callback_ != nullptr)
  {
    channel_callback_(channel_event::added, nc);
  }
}

size_t channels::size()
{
  return channels_.size();
}

channel_t channels::next()
{
  if (!channels_.empty())
  {
    current_++;
    
    if (current_ == channels_.end())
      current_ = channels_.begin();
    
    return current();
  }
  throw runtime_error("Next channel requested while channel list is empty");
}

channel_t channels::current()
{
  if (!channels_.empty())
  {
    return current_->second;
  }
  throw runtime_error("Current channel requested while channel list is empty");
}

void channels::get_channel_ids(std::vector<long long>& ids)
{
  ids.reserve(channels_.size());
  
  for (auto c : channels_)
  {
    ids.push_back(static_cast<long long>(c.second->get_id()));
  }
}

void channels::print()
{
  for (auto c : channels_)
  {
    c.second->print();
  }
}

}
}
