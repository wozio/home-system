#ifndef CHANNELS_H
#define	CHANNELS_H

#include "channel.h"
#include "transponders.h"
#include <map>
#include <vector>
#include <functional>

namespace home_system
{
namespace media
{

enum class channel_event
{
  added,
  removed,
  changed
};

class channels
{
public:
  channels(const std::string& channels_file, transponders& to);
  ~channels();
  
  size_t size();
  channel_t next();
  channel_t current();
  
  channel_t get(uint64_t id);
  
  void get_channel_ids(std::vector<long long>& ids);
  void print();
  
  void add(uint64_t id, const std::string& name, uint16_t service_id);
  
  typedef std::function<void (channel_event, channel_t)> channel_callback_t;
  void set_channel_callback(channel_callback_t callback = nullptr);
  
private:
  channels(const channels& o)
  : transponders_(o.transponders_)
  {};
  channel_callback_t channel_callback_;
    
  transponders& transponders_;
  typedef std::map<uint64_t, channel_t> channels_t;
  channels_t channels_;
  
  channels_t::iterator current_;
};

}
}

#endif	/* CHANNELS_H */

