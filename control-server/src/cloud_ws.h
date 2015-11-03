#ifndef CLOUD_WS_H
#define	CLOUD_WS_H

#include "Poco/Net/WebSocket.h"
#include <memory>

namespace home_system
{
  
class cloud_ws
{
public:
  cloud_ws();
  cloud_ws(const cloud_ws& orig) = delete;
  ~cloud_ws();
private:
  
  std::unique_ptr<Poco::Net::WebSocket> ws_;

};

}

#endif	/* CLOUD_WS_H */
