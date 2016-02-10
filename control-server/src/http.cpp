#include "http.h"
#include "file_request_handler.h"
#include "ws_request_handler.h"
#include "logger.h"
#include <Poco/Net/HTTPServerResponse.h>
#include <Poco/URI.h>

using namespace Poco::Net;

namespace home_system
{
  
request_handler_factory::request_handler_factory(const std::string& root)
: root_(root)
{
  fill_media_types();
}

HTTPRequestHandler* request_handler_factory::createRequestHandler(const HTTPServerRequest& request)
{
  std::string uri = request.getURI();
  LOG(DEBUG) << "Request: " << request.clientAddress().toString() << " URI: " << uri;
  if (uri == "/access/client/" || uri == "/access/client")
  {
    return new ws_request_handler();
  }
  else
  {
    return new file_request_handler(root_);
  }
}

}
