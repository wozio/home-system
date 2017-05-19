#include "pch.h"
#include "client_service.h"
#include "logger.h"
#include "discovery.h"
#include "json_converter.h"

using namespace std;

namespace home_system
{

client_service::client_service(const std::string& name, handler_t handler)
: service(name),
  name_(name),
  handler_(handler)
{
  LOG(DEBUG) << "Created client service: " << name;
  set_timer();
}

client_service::~client_service()
{
  LOG(DEBUG) << "Destroing client service: " << name_;
  DISCOVERY.unsubscribe(discovery_subscription_id_);
  timer_.cancel();
  std::lock_guard<std::mutex> guard(incoming_map_mutex_);
  incoming_.clear();
  incoming_timeouts_.clear();
}

void client_service::init()
{
  discovery_subscription_id_ = DISCOVERY.subscribe([this](const std::string& service, bool available){
    yami::parameters params;
    params.set_string("service", service);
    params.set_boolean("available", available);
    buffer_t buffer(new rapidjson::StringBuffer);
    msg_to_json(name_, "service_availability", params, buffer);
    handler_->on_send(buffer);
  });
}

void client_service::add_binary_connection(ws_t ws)
{
  // check if we have such session established
  if (!client_binary_session_)
  {
    LOG(ERROR) << "Request for not known binary session establishing";
    throw runtime_error("Unknown session id for this client_service");
  }
  binary_handler_.reset(new handler(ws, true));
  binary_handler_->init();
  // forward data received by binary session towards handler
  client_binary_session_->data_.connect([this](const std::vector<char>& indata)
  {
    if (indata.size() <= DATA_SIZE)
    {
      auto outdata = create_data();
      memcpy(outdata->data(), &indata[0], indata.size());
      binary_handler_->on_send(outdata, indata.size(), handler::BINARY);
    }
  });
}

void client_service::on_msg(yami::incoming_message & im)
{
  std::lock_guard<std::mutex> guard(incoming_map_mutex_);
  //LOG(TRACE) << "[" << name_ << "] Incoming message " << im.get_message_name();
  int sn = 0;
  while (incoming_.find(sn) != incoming_.end())
  {
    sn++;
  }
  //LOG(TRACE) << "Assigned sequence_number = " << sn;
  
  // convert to json
  buffer_t buffer(new rapidjson::StringBuffer);
  msg_to_json(name_, im.get_message_name(), sn, im.get_parameters(), buffer);
  
  // send to handler
  handler_->on_send(buffer);
  
  // move incoming message to map with assigned sequence number
  incoming_.emplace(sn, im);
  
  // start timer for assigned sequence number
  incoming_timeouts_[sn] = 5;
}

void client_service::set_timer()
{
  timer_.set_from_now(1000, [this]() {
    this->on_timeout();
  });
}

void client_service::on_timeout()
{
  set_timer();
  std::lock_guard<std::mutex> guard(incoming_map_mutex_);
  if (!incoming_timeouts_.empty())
  {
    std::list<int> timed_out;
    for (auto t : incoming_timeouts_)
    {
      t.second--;
      if (t.second == 0)
      {
        timed_out.push_back(t.first);
      }
    }
    for (auto sn : timed_out)
    {
      incoming_timeouts_.erase(sn);
      incoming_.erase(sn);
    }
  }
}

void client_service::on_remote_msg(const std::string& source, const std::string& target,
  msg_type_t msg_type, const std::string& msg,
  int sequence_number, yami::parameters& params)
{
  try
  {
    string ye = DISCOVERY.get(target);

    switch (msg_type)
    {
    case msg_type_t::one_way:
      //LOG(DEBUG) << "One way message: '" << msg << "', from '" << source << "' to '" << target << "'";
      AGENT.send_one_way(ye, target, msg, params);
      break;

    case msg_type_t::for_reply:
    {
      if (msg == "create_session")
      {
        // special handling of create_session message
        client_binary_session_.reset(new client_binary_session());

        // creating session in server
        params.set_string("endpoint", client_binary_session_->get_endpoint());
        auto msg = AGENT.send(ye, target, "create_session", params);

        msg->wait_for_completion(1000);

        if (msg->get_state() == yami::replied)
        {
          auto reply = msg->get_reply();
          buffer_t buffer(new rapidjson::StringBuffer);
          reply_to_json(source, "success", "", sequence_number, reply, buffer);
          handler_->on_send(buffer);
        }
        else if (msg->get_state() == yami::rejected)
        {
          client_binary_session_.reset();
          throw std::runtime_error("Unable to create session, " + msg->get_exception_msg());
        }
        else
        {
          client_binary_session_.reset();
          throw std::runtime_error("Unable to create session, timed out");
        }
      }
      else
      {
        //LOG(DEBUG) << "Message expecting reply: '" << msg << "', from '" << source << "' to '" << target << "'";
        auto_ptr <yami::outgoing_message> message(AGENT.send(ye, target, msg, params));

        message->wait_for_completion(1000);

        switch (message->get_state())
        {
        case yami::replied:
        {
          //LOG(DEBUG) << "Got reply";
          // converting yami output to json
          // yami binary values are not supported
          buffer_t buffer(new rapidjson::StringBuffer);
          reply_to_json(source, "success", "", sequence_number, message->get_reply(), buffer);

          handler_->on_send(buffer);

          break;
        }

        case yami::posted:
        case yami::transmitted:
        case yami::abandoned:
          LOG(WARNING) << "Posted/Transmitted/Abandoned after timeout";
          break;

        case yami::rejected:
        {
          LOG(WARNING) << "Rejected: " + message->get_exception_msg();
          buffer_t buffer(new rapidjson::StringBuffer);
          reply_to_json(source, "failed", message->get_exception_msg(), sequence_number, buffer);
          handler_->on_send(buffer);
          break;
        }
        }
      }
      break;
    }

    case msg_type_t::reply:
    {
      std::lock_guard<std::mutex> guard(incoming_map_mutex_);
      auto in = incoming_.find(sequence_number);
      if (in != incoming_.end())
      {
        incoming_timeouts_.erase(sequence_number);
        in->second.reply(params);
        incoming_.erase(in);
      }
      else
      {
        LOG(DEBUG) << "reply for message with not known sequence number " << sequence_number << ", it is incorrect or already timed out";
      }
      break;
    }
    }
  }
  catch (const std::exception& e)
  {
    LOG(WARNING) << "EXCEPTION: " << e.what();
    if (msg_type == msg_type_t::for_reply)
    {
      buffer_t buffer(new rapidjson::StringBuffer);
      reply_to_json(source, "failed", e.what(), sequence_number, buffer);
      handler_->on_send(buffer);
    }
  }
}

}
