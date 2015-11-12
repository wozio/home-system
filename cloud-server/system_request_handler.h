#ifndef SYSTEM_REQUEST_HANDLER_H
#define	SYSTEM_REQUEST_HANDLER_H

#include <Poco/Net/HTTPRequestHandler.h>
#include <Poco/Net/WebSocket.h>
#include <memory>

namespace home_system
{

class system_request_handler : public Poco::Net::HTTPRequestHandler
{
public:
  system_request_handler();
  ~system_request_handler();
  void handleRequest(Poco::Net::HTTPServerRequest& request, Poco::Net::HTTPServerResponse& response);
  
  static std::unique_ptr<system_handler> system_h_;
  
private:
};

}

#endif	/* SYSTEM_REQUEST_HANDLER_H */

