#include "pch.h"
#include "http.h"
#include "file_request_handler.h"
#include "ws_request_handler.h"
#include "logger.h"

using namespace Poco::Net;
using namespace Poco;

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
  std::string path = URI(request.getURI()).getPath();
  static const boost::regex e("^/access/client(/+(\\w*))*$");
  boost::smatch  m;
  if (boost::regex_match(path, m, e))
  {
    LOG(DEBUG) << "WebSocket connection request";
    return new ws_request_handler(m[2]);
  }
  else
  {
    return new file_request_handler(root_);
  }
}

}
