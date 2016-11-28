#ifndef FILE_READER_H
#define	FILE_READER_H

#include "session_callback_t.h"
#include <thread>

namespace home_system
{
namespace media
{

class file_reader
{
public:
  file_reader(int adapter, int frontend, dvb::session_stream_part_callback_t callback);
  ~file_reader();
  
  bool is_running();
  
private:
  int adapter_, frontend_;
  bool continue_, running_;
  std::thread thread_;
  dvb::session_stream_part_callback_t callback_;
  
  
  void thread_exec();
};

}
}

#endif	/* FILE_READER_H */

