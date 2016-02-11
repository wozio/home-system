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
    LOG(DEBUG) << "Request for system connection";
    
    ws_t ws(new WebSocket(request, response));
    system_t h(new system(ws));
    h->init();
    
    SYSTEMS.add(h);
  }
  catch (const WebSocketException& exc)
  {
    LOG(ERROR) << exc.displayText();
    switch (exc.code())
    {
    case WebSocket::WS_ERR_HANDSHAKE_UNSUPPORTED_VERSION:
      response.set("Sec-WebSocket-Version", WebSocket::WEBSOCKET_VERSION);
      // fallthrough
    case WebSocket::WS_ERR_NO_HANDSHAKE:
    case WebSocket::WS_ERR_HANDSHAKE_NO_VERSION:
    case WebSocket::WS_ERR_HANDSHAKE_NO_KEY:
      response.setStatusAndReason(HTTPResponse::HTTP_BAD_REQUEST);
      response.setContentLength(0);
      response.send();
      break;
    }
  }
  catch (const Exception& e)
  {
    LOG(ERROR) << e.displayText();
  }
  catch (const exception& e)
  {
    LOG(ERROR) << e.what();
  }
}

}
