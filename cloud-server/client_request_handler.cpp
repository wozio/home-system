#include "client_request_handler.h"
#include "system_request_handler.h"
#include "logger.h"
#include <Poco/Net/HTTPServerRequest.h>
#include <Poco/Net/HTTPServerResponse.h>
#include <Poco/Net/NetException.h>

using namespace Poco::Net;
using namespace Poco;
using namespace std;

namespace home_system
{
  
std::unique_ptr<Poco::Net::WebSocket> client_request_handler::ws_;
  
client_request_handler::client_request_handler()
{
}

client_request_handler::~client_request_handler()
{
}

void client_request_handler::handleRequest(HTTPServerRequest& request, HTTPServerResponse& response)
{
  try
  {
    ws_.reset(new WebSocket(request, response));
    std::unique_ptr<char[]> data(new char[1025]);
    int flags;
    int n;
    do
    {
      try
      {
        n = ws_->receiveFrame(data.get(), 1024, flags);
        
        if ((flags & WebSocket::FRAME_OP_BITMASK) == WebSocket::FRAME_OP_CLOSE)
        {
          LOG("Closing connection");
          ws_->shutdown();
          return;
        }
        
        if ((flags & WebSocket::FRAME_OP_BITMASK) == WebSocket::FRAME_OP_TEXT && n > 0)
        {
          system_request_handler::sendToSystem(data.get(), n);
        }
      }
      catch (const runtime_error& e)
      {
        LOGWARN("EXCEPTION: runtime_error: " << e.what());
      }
      catch (const TimeoutException& e)
      {
        // do nothing with this one
      }
      catch (const Exception& e)
      {
        LOGWARN("EXCEPTION: " << e.displayText());
      }
      catch (const std::exception& e)
      {
        LOGWARN("EXCEPTION: " << e.what());
      }
    }
    while ((flags & WebSocket::FRAME_OP_BITMASK) != WebSocket::FRAME_OP_CLOSE);
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
  
  ws_.reset();
}

void client_request_handler::sendToClient(char* buf, size_t size)
{
  if (ws_)
  {
    ws_->sendFrame(buf, size, WebSocket::FRAME_OP_TEXT);
  }
}
  
}
