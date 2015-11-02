#ifndef FILE_REQUEST_HANDLER_H
#define	FILE_REQUEST_HANDLER_H

#include <Poco/Net/HTTPRequestHandler.h>
#include <string>

namespace home_system
{
  
void fill_media_types();

class file_request_handler : public Poco::Net::HTTPRequestHandler
{
private:
  const std::string& root_;
public:
  file_request_handler(const std::string& root);
  void handleRequest(Poco::Net::HTTPServerRequest& request, Poco::Net::HTTPServerResponse& response);
};

}

#endif	/* FILE_REQUEST_HANDLER_H */

