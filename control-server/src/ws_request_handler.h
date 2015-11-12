#ifndef WS_REQUEST_HANDLER_H
#define	WS_REQUEST_HANDLER_H

#include <Poco/Net/HTTPRequestHandler.h>

namespace home_system
{

class ws_request_handler : public Poco::Net::HTTPRequestHandler
{
public:
  ws_request_handler();
  ~ws_request_handler();
  void handleRequest(Poco::Net::HTTPServerRequest& request, Poco::Net::HTTPServerResponse& response);

private:
  
};

}

#endif	/* WS_REQUEST_HANDLER_H */

