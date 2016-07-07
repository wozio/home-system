#ifndef SYSTEM_H
#define	SYSTEM_H

#include "handler.h"
#include "client_t.h"
#include "rapidjson/document.h"

namespace home_system
{

class system
: public handler
{
public:
  system(ws_t ws);
  system(const system& orig) = delete;
  ~system();
  
  void on_read(data_t data, size_t data_size);
  void shutdown();
  
  void set_route(const std::string& target, client_t client);
  void unset_route(const std::string& target);
  
  void send_to_system(const rapidjson::Document& d);
  
private:
  
  enum {
    wait_for_login,
    logged_in
  } sys_state_;
  
  std::map<std::string, client_t> route_;
};

}

#endif	/* SYSTEM_H */

