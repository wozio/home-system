#ifndef WS_COM_HANDLER_H
#define	WS_COM_HANDLER_H

#include "Poco/Net/WebSocket.h"

namespace home_system
{
void handle_ws_communication(Poco::Net::WebSocket& ws);

// exceptions

class bad_request : public std::exception
{
};

class service_unavailable : public std::exception
{
public:

  service_unavailable(const std::string& reason) throw ()
  : reason_(reason)
  {
  }

  ~service_unavailable() throw ()
  {
  }

  const char* what() const throw ()
  {
    return reason_.c_str();
  }
private:
  std::string reason_;
};
}

#endif	/* WS_COM_HANDLER_H */

