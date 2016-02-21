#include "transponder.h"
#include <logger.h>
#include <sstream>
#include <memory>

namespace home_system
{
namespace media
{

transponder::transponder()
{
}

transponder::~transponder()
{
}

void transponder::add_channel(channel_t c)
{
  LOG(DEBUG) << "Channel added: " << c->get_name();
  channels_[c->get_id()] = c;
}

void transponder::remove_channel(uint64_t cid)
{
  channels_.erase(cid);
}

void transponder::get_channels(std::vector<channel_t>& channel_list)
{
  channel_list.clear();
  channel_list.reserve(channels_.size());
  for (auto c : channels_)
  {
    channel_list.push_back(c.second);
  }
}

std::ostream& operator<< (std::ostream& out, const transponder& t)
{
  t.print(out);
  return out;
}

std::ostream& operator<< (std::ostream& out, const std::shared_ptr<transponder> t)
{
  out << *(t.get());
  return out;
}

}
}
