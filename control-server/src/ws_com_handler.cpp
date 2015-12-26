#include "ws_com_handler.h"

#include "handlers.h"
#include "json_converter.h"
#include "yamicontainer.h"
#include "logger.h"
#include "discovery.h"
#include "ws_com_handler.h"
#include <boost/algorithm/string.hpp>
#include <memory>
#include <string>

using namespace Poco::Net;
using namespace Poco;
using namespace std;

namespace home_system
{

void handle_ws_data(data_t data, size_t data_size, handler_t handler)
{
  try
  {
    // it is guaranteed that data size is bigger than data_size
    (*data)[data_size] = '\0';

    yami::parameters params;
    std::string service;
    std::string msg;
    bool expect_reply;

    process_json(data, service, msg, expect_reply, params);

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
          //LOG("Replied");
          // converting yami output to json
          // yami binary values are not supported

          auto out = create_data();
          yami::parameters* reply = message->extract_reply();
          size_t out_size = 0;
          process_parameters(reply, out, out_size);
          delete reply;

          handler::on_send(handler, out, out_size);

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
}

}
