#ifndef SYSTEM_REQUEST_HANDLER_H
#define	SYSTEM_REQUEST_HANDLER_H

#include <Poco/Net/HTTPRequestHandler.h>
#include <memory>
#include <string>

namespace home_system
{

class system_request_handler : public Poco::Net::HTTPRequestHandler
{
public:
  system_request_handler();
  ~system_request_handler();
  void handleRequest(Poco::Net::HTTPServerRequest& request, Poco::Net::HTTPServerResponse& response);
  
  static void sendToSystem(char* buf, size_t size);

private:
};

}

#endif	/* SYSTEM_REQUEST_HANDLER_H */

