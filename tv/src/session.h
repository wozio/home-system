#ifndef SESSION_H
#define	SESSION_H

#include "ios_wrapper.h"
#include "timer.h"

namespace home_system
{
namespace media
{

typedef std::function<void(int id, void* buf, size_t len, long long buf_size,
  long long beg_time, long long cur_time, long long end_time)> stream_callback_t;

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
  session(int id, stream_callback_t stream_callback);
  session(const session& orig) = delete;
  ~session();
  
  void stream_part(const void* buf, size_t length);
  long long get_data(char* buf, long long len);
  // absolute position where 0 oldest position in buffer and current size is
  // latest position in buffer
  // returns current position in buffer
  void play();
  void pause();
  // returns current position in buffer after seek operation
  void seek(long long apos, std::function<void(long long pos, long long time)> callback);
  
private:
  int id_;
  stream_callback_t stream_callback_;
  
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
