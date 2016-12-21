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

  bool is_logged_in(const std::string& client);
  void login(const std::string& client);
  void logout(const std::string& client);
  
private:
  shutdown_callback_t on_shutdown_;
  std::set<std::string> clients_ids_;
};

}

#endif	/* CLOUD_CLIENT_H */

