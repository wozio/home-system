#ifndef DEMUX_H
#define	DEMUX_H

#include "timer.h"
#include "channels.h"
#include "file-reader.h"
#include "session_callback_t.h"
#include <map>
#include <mutex>
#include <deque>

struct section;

namespace home_system
{
namespace media
{

enum class demux_state
{
  idle,
  mux_set
};

class demux
{
public:
  
  
  demux(int adapter, int frontend, channels& channels, transponders& transponders);
  ~demux();
  
  void set_mux();
  void reset_mux();
  
  typedef std::function<void (demux_state)> state_callback_t;
  void set_state_callback(state_callback_t callback = nullptr);
  
  struct event_info
  {
    uint64_t channel_;
    int event_;
    time_t start_time_;
    int duration_;
    std::string title_;
    std::string description_;
    std::string plot_;
  };
  
  typedef std::function<void (const event_info&)> ei_callback_t;
  void set_ei_callback(ei_callback_t callback = nullptr);
  
  
  int set_channel(channel_t c, dvb::session_stream_part_callback_t callback);
  
private:
  demux(const demux& o)
  : channels_(o.channels_),
    transponders_(o.transponders_)
  {
  }
  
  int adapter_, demux_;
  
  channels& channels_;
  transponders& transponders_;
  
  typedef std::function<void (section*)> section_callback_t;
  std::map<int, section_callback_t> section_callbacks_;
  std::vector<pollfd> pollfds_;
  
  uint8_t sdt_version_number_;
  uint8_t nit_version_number_;
  uint8_t pat_version_number_;
  uint8_t pmt_version_number_;
  bool send_event_info_;
  
  demux_state state_;
  void change_state(demux_state new_state);
  
  state_callback_t state_callback_;
  ei_callback_t ei_callback_;
  
  dvb::session_stream_part_callback_t session_callback_;
  channel_t channel_;

  std::deque<int> pid_fds_;
  uint16_t pmt_pid_;
  
  std::unique_ptr<home_system::media::file_reader> file_reader_;
  
  timer timer_;
  void set_timer(int duration);
  
  int create_section_filter(uint16_t pid, uint8_t table, uint8_t bitmask,
    section_callback_t section_callback);
  void remove_section_filter(int fd);
  int create_pid_filter(uint16_t pid);
  
  std::mutex state_mutex_;
  
  void check();
  void poll_filters();
  void check_pat(section* s);
  void check_pmt(section* s);
  void check_sdt(section* s);
  void check_eit(section* s);
  void check_nit(section* s);
};

}
}

#endif	/* DEMUX_H */
