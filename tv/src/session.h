#ifndef SESSION_H
#define	SESSION_H

#include "server_binary_session.h"

namespace home_system
{
namespace media
{

class session_error
  : public std::runtime_error
{
public:
  session_error(const std::string& what_arg, int session)
  : runtime_error(what_arg),
    session_(session)
  {
  }
  
  int session() const
  {
    return session_;
  }
  
private:
  int session_;
};

class session
{
public:
  session(int id, const std::string& endpoint);
  session(const session& orig) = delete;
  ~session();
  
  void stream_part(const void* buf, size_t length);
  void play();
  void pause();
  // returns current position in buffer after seek operation
  void seek(long long apos, std::function<void(long long pos, long long time)> callback);

  boost::signals2::signal<void(long long buf_size, long long cur_pos,
    long long beg_time, long long cur_time, long long end_time)> stream_info_;

private:
  int id_;
  std::unique_ptr<server_binary_session> binary_session_;
  bool playing_;
  size_t read_write_diff_; // how much writing is ahead of reading, should never be negative...
  size_t readpos_, writepos_;
  bool full_;
  std::fstream buffer_;

  ios_wrapper ios_;
  
  void trigger_send();
  void send();
  
  std::mutex m_mutex;

  long long abs_pos_, abs_len_;

  std::map<long long, long long> pos_to_time_;

  void get_times(long long& bt, long long& ct, long long& et);

  void log_data();
};

}
}

#endif	/* SESSION_H */
