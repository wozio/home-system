#ifndef CLOUD_CLIENT_H
#define	CLOUD_CLIENT_H

#include "client.h"

namespace home_system
{

class cloud_client
: public client
{
public:
  typedef std::function<void()> shutdown_callback_t;
  cloud_client(ws_t ws, shutdown_callback_t shutdown_callback);
  cloud_client(const cloud_client& orig) = delete;
  ~cloud_client();
  
  void shutdown();
  
private:
  shutdown_callback_t on_shutdown_;
};

}

#endif	/* CLOUD_CLIENT_H */

