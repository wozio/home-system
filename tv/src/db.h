#ifndef DB_H
#define	DB_H

#include "event_info.h"
#include <Poco/Data/Common.h>
#include <stdint.h>
#include <mutex>
#include <map>
#include <stdexcept>

namespace home_system
{
namespace media
{
  
class db
{
public:
  db();
  ~db();
  
  void check_and_add_local_channels(const std::string& source, size_t size, long long* local, const std::vector<std::string>& names);
  void check_and_add_local_channel(const std::string& source, long long local, const std::string& name);
  void add_channel(const std::string& source, long long local, const std::string& name);
  bool check_channel_existence(int channel);
  void get_channels(std::vector<int>& channels, std::vector<std::string>& names);
  int get_channel_from_source_local(const std::string& source, long long local) throw(std::out_of_range);
  void get_sources_local_channels(int channel, std::vector<std::string>& sources,
    std::vector<long long>& local_channels);
  void get_sources_for_channel(int channel, std::vector<std::string>& sources);
  long long get_local_channel(int channel, const std::string& source);
  
  int get_channel_for_recording(int id);
  void get_recordings_to_start(std::vector<int>& recordings);
  void get_recordings_to_stop(std::vector<int>& recordings);
  int create_recording(int channel, time_t start_time, int duration);
  
  std::string get_channel_name(int channel);

private:
  Poco::Data::Session& session();
  std::mutex db_mutex_;
  
  
  std::map<std::string, std::map<long long, int>> source_local_to_global_;
  
};

}
}

#endif	/* DB_H */

