#include "client.h"
#include "json_converter.h"
#include "yamicontainer.h"
#include "logger.h"
#include "discovery.h"
#include <thread>

using namespace std;

namespace home_system
{

client::client(ws_t ws)
: handler(ws)
{
  LOG("Client connected");
}

client::~client()
{
}

void client::on_read(data_t data, size_t data_size)
{
  //LOG("Read " << data_size << " bytes");
  try
  {
    thread t([this](data_t data, size_t data_size) {
      this->handle_data(data, data_size);
    }, data, data_size);
    t.detach();
  }
  catch (...)
  {
  }
}

void client::handle_data(data_t data, size_t data_size)
{
  try
  {
    // it is guaranteed that data size is bigger than data_size
    (*data)[data_size] = '\0';

    yami::parameters params;
    std::string service;
    std::string msg;
    std::string source;
    bool expect_reply;
    long long sequence_number;

    process_json(data, source, service, msg, expect_reply, sequence_number, params);

    LOG("Service: " << service << ", message: " << msg << ", from " << source);

    if (!this->is_logged_in(source))
    {
      if (msg != "login")
      {
        LOGWARN("Message from not logged source: " << source);
        reject(expect_reply, sequence_number, source, "Message from not logged source");
      }
    }

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
            yami_to_json(source, reply, sequence_number, out, out_size);
            delete reply;

            on_send(shared_from_this(), out, out_size);

            break;
          }

          case yami::abandoned:
            LOGWARN("Posted/Transmitted/Abandoned after timeout");
            throw runtime_error("Message was abandoned");
            break;

          case yami::rejected:
            LOGWARN("Rejected: " + message->get_exception_msg());
            throw runtime_error(
                "Message was rejected: " + message->get_exception_msg());
            break;
          }
        }
        catch (const exception& e)
        {
          LOGWARN("EXCEPTION: " << e.what());
          reject(expect_reply, sequence_number, source, e.what());
        }
      }
    }
    catch (const exception& e)
    {
      LOGWARN("EXCEPTION: " << e.what());
      reject(expect_reply, sequence_number, source, e.what());
    }
  }
  catch (const exception& e)
  {
    LOGWARN("EXCEPTION: " << e.what());
  }
}

void client::reject(bool expect_reply, long long sequence_number,
    const std::string& source, const char* reason)
{
  reject(expect_reply, sequence_number, source, string(reason));
}

void client::reject(bool expect_reply, long long sequence_number,
    const std::string& source, const std::string& reason)
{
  if (expect_reply)
  {
    size_t out_size = 0;
    auto out = create_data();
    string result("failed");
    yami_to_json(source, result, reason, sequence_number, out, out_size);
    client::on_send(shared_from_this(), out, out_size);
  }
}

bool client::is_logged_in(const std::string& source)
{
  return true;
}

}
