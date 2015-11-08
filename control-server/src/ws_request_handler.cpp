#include "ws_request_handler.h"
#include "json_converter.h"
#include "yamicontainer.h"
#include "logger.h"
#include "discovery.h"
#include <Poco/Net/HTTPServerRequest.h>
#include <Poco/Net/HTTPServerResponse.h>
#include <Poco/Net/NetException.h>
#include <Poco/URI.h>
#include <boost/algorithm/string.hpp>
#include <vector>
#include <memory>

using namespace Poco::Net;
using namespace Poco;
using namespace std;

namespace home_system
{

ws_request_handler::ws_request_handler()
{
}

ws_request_handler::~ws_request_handler()
{
}

void ws_request_handler::handleRequest(HTTPServerRequest& request, HTTPServerResponse& response)
{
  try
  {
    Poco::Net::WebSocket ws(request, response);
    std::unique_ptr<char[]> data(new char[1025]);
    int flags;
    int n;
    do
    {
      try
      {
        n = ws.receiveFrame(data.get(), 1024, flags);
        
        if ((flags & WebSocket::FRAME_OP_BITMASK) == WebSocket::FRAME_OP_CLOSE)
        {
          LOG("Closing connection");
          ws.shutdown();
          return;
        }
        
        if ((flags & WebSocket::FRAME_OP_BITMASK) == WebSocket::FRAME_OP_TEXT && n > 0)
        {
          data[n] = '\0';

          yami::parameters params;
          std::string service;
          std::string msg;
          bool expect_reply;
          
          process_json(data.get(), service, msg, expect_reply, params);
          
          //LOG("Service: " << service << ", message: " << msg);

          string ye = DISCOVERY.get(service);

          // sending message to service
          auto_ptr<yami::outgoing_message> message(
            AGENT.send(ye, service, msg, params));

          if (expect_reply)
          {
            message->wait_for_completion(1000);

            switch (message->get_state())
            {
              case yami::replied:
              {
                //LOG("Replied");
                response.setChunkedTransferEncoding(true);
                response.setContentType("application/json");
                response.add("Expires", "-1");
                response.add("Cache-control", "no-cache");

                // converting yami output to json
                // yami binary values are not supported

                string reply_json;
                yami::parameters* reply = message->extract_reply();
                process_parameters(reply, reply_json);
                delete reply;

                ws.sendFrame(reply_json.data(), reply_json.size(), flags);

                break;
              }

              case yami::posted:
              case yami::transmitted:
                //LOG("Posted/transmitted");
                break;

              case yami::abandoned:
                LOGWARN("Abandoned");
                throw service_unavailable("Message was abandoned");
                break;

              case yami::rejected:
                LOGWARN("Rejected: " + message->get_exception_msg());
                throw service_unavailable("Message was rejected: " + message->get_exception_msg());
                break;
            }
          }
        }
      }
      catch (const incorrect_message& e)
      {
        LOGWARN("EXCEPTION: incorrect_message");
      }
      catch (const service_not_found& e)
      {
        LOGWARN("EXCEPTION: service_not_found: " << e.what());
      }
      catch (const service_unavailable& e)
      {
        LOGWARN("EXCEPTION: service_unavailable: " << e.what());
      }
      catch (const yami::yami_runtime_error& e)
      {
        LOGWARN("EXCEPTION: yami_runtime_error: " << e.what());
      }
      catch (const runtime_error& e)
      {
        LOGWARN("EXCEPTION: runtime_error: " << e.what());
      }
      catch (const bad_request& e)
      {
        LOGWARN("EXCEPTION: bad_request: " << e.what());
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
}
  
}
