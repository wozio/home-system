#ifndef CLIENT_H
#define	CLIENT_H

#include "handler.h"

namespace home_system
{

class cloud_client
: public handler
{
public:
  cloud_client(ws_t ws, std::function<void()>shutdown_callback);
  cloud_client(const cloud_client& orig) = delete;
  ~cloud_client();
  
  void on_read(data_t data, size_t data_size);
  
  void shutdown();
  
private:
  std::function<void()>on_shutdown_;
};

}

#endif	/* CLIENT_H */

