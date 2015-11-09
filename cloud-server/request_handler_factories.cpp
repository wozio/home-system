#include "request_handler_factories.h"
#include "client_request_handler.h"
#include "system_request_handler.h"

using namespace Poco::Net;

namespace home_system
{
  
client_request_handler_factory::client_request_handler_factory()
{
}

HTTPRequestHandler* client_request_handler_factory::createRequestHandler(const HTTPServerRequest& request)
{
  return new client_request_handler();
}

system_request_handler_factory::system_request_handler_factory()
{
}

HTTPRequestHandler* system_request_handler_factory::createRequestHandler(const HTTPServerRequest& request)
{
  return new system_request_handler();
}

}
