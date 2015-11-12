#ifndef CLIENT_REQUEST_HANDLER_H
#define	CLIENT_REQUEST_HANDLER_H

#include <Poco/Net/HTTPRequestHandler.h>
#include <Poco/Net/WebSocket.h>
#include <memory>
#include <string>

namespace home_system
{

class client_request_handler : public Poco::Net::HTTPRequestHandler
{
public:
  client_request_handler();
  ~client_request_handler();
  void handleRequest(Poco::Net::HTTPServerRequest& request, Poco::Net::HTTPServerResponse& response);
  
  static void sendToClient(char* buf, size_t size);

private:
  
  static std::unique_ptr<Poco::Net::WebSocket> ws_;
};

}

#endif	/* CLIENT_REQUEST_HANDLER_H */
