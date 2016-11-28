#ifndef EVENT_INFO_H
#define	EVENT_INFO_H

namespace home_system
{
namespace media
{
  
struct event_info
{
  int event_id_; // unique within channel
  time_t start_time_;
  int duration_;
  std::string title_;
  std::string description_;
  std::string plot_;
  bool operator<(const event_info& right) const
  {
    return start_time_ < right.start_time_;
  }
};
  
}
}
#endif	/* EVENT_INFO_H */

