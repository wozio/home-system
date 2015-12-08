#ifndef CLOUD_CLIENT_H
#define	CLOUD_CLIENT_H

#include "client.h"

namespace home_system
{

class cloud_client
: public client
{
public:
  cloud_client(ws_t ws, std::function<void()>shutdown_callback);
  cloud_client(const cloud_client& orig) = delete;
  ~cloud_client();
  
  void shutdown();
  
private:
  std::function<void()>on_shutdown_;
};

}

#endif	/* CLOUD_CLIENT_H */

