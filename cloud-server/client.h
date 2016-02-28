#ifndef CLIENT_H
#define	CLIENT_H

#include "handler.h"
#include "system_t.h"
#include <mutex>

namespace home_system
{

class client
: public handler
{
public:
  client(ws_t ws);
  client(const client& orig) = delete;
  ~client();
  
  void on_read(data_t data, size_t data_size);
  void send(data_t data, size_t data_size);
  
  void shutdown();
  
private:
  enum {
    not_connected,
    wait_for_login,
    logged_in
  } client_state_;
  
  std::mutex client_state_mutex_;
  system_t system_;
  int seq_num_;
  std::string tmp_route_key_;
  std::string route_key_;
  
};

}

#endif	/* CLIENT_H */

