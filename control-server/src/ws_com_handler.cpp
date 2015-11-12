#include "ws_com_handler.h"

#include "json_converter.h"
#include "yamicontainer.h"
#include "logger.h"
#include "discovery.h"
#include <Poco/Net/NetException.h>
#include <Poco/URI.h>
#include <boost/algorithm/string.hpp>
#include <memory>
#include <string>

using namespace Poco::Net;
using namespace Poco;
using namespace std;

namespace home_system
{

void handle_ws_communication(Poco::Net::WebSocket& ws)
{
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

        LOG("Service: " << service << ", message: " << msg);

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
              LOG("Replied");
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
  }
  while ((flags & WebSocket::FRAME_OP_BITMASK) != WebSocket::FRAME_OP_CLOSE);
}

}
