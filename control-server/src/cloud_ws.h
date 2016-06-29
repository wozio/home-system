#ifndef CLOUD_WS_H
#define	CLOUD_WS_H

#include "timer.h"
#include <thread>
#include <string>

namespace home_system
{

class cloud_ws
{
public:
  cloud_ws(const std::string& host, int port, const std::string& uri, bool no_ssl);
  cloud_ws(const cloud_ws& orig) = delete;
  ~cloud_ws();
private:
  std::string host_;
  int port_;
  std::string uri_;
  bool no_ssl_;
  
  timer timer_;
  void connect();
};

}

#endif	/* CLOUD_WS_H */
