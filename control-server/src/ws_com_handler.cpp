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
    long long sequence_number;

    process_json(data, service, msg, expect_reply, sequence_number, params);

    LOG("Service: " << service << ", message: " << msg);

    try
    {
      string ye = DISCOVERY.get(service);

      // sending message to service
      if (!expect_reply)
      {
        AGENT.send_one_way(ye, service, msg, params);
      }
      else
      {
        try
        {
          auto_ptr <yami::outgoing_message> message(AGENT.send(ye, service, msg, params));

          message->wait_for_completion(1000);

          switch (message->get_state())
          {
          case yami::replied:
          {
            //LOG("Replied");
            // converting yami output to json
            // yami binary values are not supported
            yami::parameters* reply = message->extract_reply();
            size_t out_size = 0;
            auto out = create_data();
            yami_to_json(reply, sequence_number, out, out_size);
            delete reply;

            handler::on_send(handler, out, out_size);

            break;
          }

          case yami::abandoned:
            LOGWARN("Posted/Transmitted/Abandoned after timeout");
            throw service_unavailable("Message was abandoned");
            break;

          case yami::rejected:
            LOGWARN("Rejected: " + message->get_exception_msg());
            throw service_unavailable(
                "Message was rejected: " + message->get_exception_msg());
            break;
          }
        }
        catch (const exception& e)
        {
          LOGWARN("EXCEPTION: " << e.what());
          size_t out_size = 0;
          auto out = create_data();
          string result("failed");
          string reason(e.what());
          yami_to_json(result, reason, sequence_number, out, out_size);
          handler::on_send(handler, out, out_size);
        }
      }
    }
    catch (const exception& e)
    {
      LOGWARN("EXCEPTION: " << e.what());
      if (expect_reply)
      {
        LOGWARN("EXCEPTION: " << e.what());
        size_t out_size = 0;
        auto out = create_data();
        string result("failed");
        string reason(e.what());
        yami_to_json(result, reason, sequence_number, out, out_size);
        handler::on_send(handler, out, out_size);
      }
    }
  }
  catch (const exception& e)
  {
    LOGWARN("EXCEPTION: " << e.what());
  }
}

}
