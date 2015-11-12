#ifndef SYSTEM_HANDLER_H
#define	SYSTEM_HANDLER_H

#include <Poco/Net/WebSocket.h>
#include <memory>
#include <thread>

class ws_handler
{
public:
  typedef std::function<void (char* buf, size_t size)> on_read_t;
  ws_handler(std::shared_ptr<Poco::Net::WebSocket> ws, on_read_t on_read);
  ws_handler(const system_handler& orig) = delete;
  ~ws_handler();
  
  void write(char* buf, size_t size);
private:
  std::shared_ptr<Poco::Net::WebSocket> ws_;
  on_read_t on_read_;
  bool run_thread_;
  std::thread thr_;
  void thr_exec();

};

#endif	/* SYSTEM_HANDLER_H */

