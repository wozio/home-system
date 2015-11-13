#include "system_request_handler.h"
#include "logger.h"
#include "systems.h"
#include <Poco/Net/HTTPServerRequest.h>
#include <Poco/Net/HTTPServerResponse.h>
#include <Poco/Net/NetException.h>

using namespace Poco::Net;
using namespace Poco;
using namespace std;

namespace home_system
{

system_request_handler::system_request_handler()
{
}

system_request_handler::~system_request_handler()
{
}

void system_request_handler::handleRequest(HTTPServerRequest& request, HTTPServerResponse& response)
{
  try
  {
    ws_t ws(new WebSocket(request, response));
    system_t h(new system(ws));
    
    SYSTEMS.add(h);
  }
  catch (Poco::Net::WebSocketException& exc)
  {
    switch (exc.code())
    {
    case Poco::Net::WebSocket::WS_ERR_HANDSHAKE_UNSUPPORTED_VERSION:
      response.set("Sec-WebSocket-Version", Poco::Net::WebSocket::WEBSOCKET_VERSION);
      // fallthrough
    case Poco::Net::WebSocket::WS_ERR_NO_HANDSHAKE:
    case Poco::Net::WebSocket::WS_ERR_HANDSHAKE_NO_VERSION:
    case Poco::Net::WebSocket::WS_ERR_HANDSHAKE_NO_KEY:
      response.setStatusAndReason(HTTPResponse::HTTP_BAD_REQUEST);
      response.setContentLength(0);
      response.send();
      break;
    }
  }
}

}
