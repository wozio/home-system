#ifndef CLIENT_H
#define	CLIENT_H

#include "handler.h"
#include "system_t.h"

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
  
  void shutdown();
  
private:
  enum {
    not_connected,
    wait_for_login,
    logged_in
  } state_;
  
  system_t system_;
  
};

}

#endif	/* CLIENT_H */

