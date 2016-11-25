#ifndef FILE_READER_H
#define	FILE_READER_H

#include "session_callback_t.h"
#include "server_binary_session.h"
#include <thread>

namespace home_system
{
namespace media
{

class file_reader
{
public:
  file_reader(int adapter, int frontend, const std::string& endpoint);
  ~file_reader();
  
  bool is_running();
  
private:
  int adapter_, frontend_;
  bool continue_, running_;
  std::thread thread_;
  std::string endpoint_;
  std::unique_ptr<server_binary_session> session_;
  
  void thread_exec();
};

}
}

#endif	/* FILE_READER_H */

