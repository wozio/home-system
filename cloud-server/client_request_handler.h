#ifndef CLIENT_REQUEST_HANDLER_H
#define	CLIENT_REQUEST_HANDLER_H

#include <Poco/Net/HTTPRequestHandler.h>

namespace home_system
{

class client_request_handler : public Poco::Net::HTTPRequestHandler
{
public:
  client_request_handler();
  ~client_request_handler();
  void handleRequest(Poco::Net::HTTPServerRequest& request, Poco::Net::HTTPServerResponse& response);
  
private:
};

}

#endif	/* CLIENT_REQUEST_HANDLER_H */

