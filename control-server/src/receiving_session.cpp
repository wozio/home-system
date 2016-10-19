#include "pch.h"

#include "receiving_session.h"

namespace home_system
{
  receiving_session::receiving_session(const std::string& target, const std::string& target_endpoint, yami::parameters& creation_params,
    data_callback_t data_callback, event_callback_t event_callback)
  : target_(target),
    target_endpoint_(target_endpoint),
    creation_params_(creation_params),
    data_callback_(data_callback),
    event_callback_(event_callback),
    id_(-1),
    agent_([]()->yami::parameters {
      yami::parameters p;
      p.set_boolean("deliver_as_raw_binary", true);
      return p;
    }())
  {
    // getting usable IP address
    Poco::Net::NetworkInterface::NetworkInterfaceList il = Poco::Net::NetworkInterface::list();
    std::string ip;
    for (size_t i = 0; i < il.size(); ++i)
    {
      if (!il[i].address().isLoopback())
        if (il[i].address().family() == Poco::Net::IPAddress::Family::IPv4)
          ip = il[i].address().toString();
    }

    // constructing enpoint for listener
    std::string ep("tcp://");
    ep.append(ip).append(":*");
    ep = agent_.add_listener(ep);

    agent_.register_object("*", *this);

    // creating session in server
    creation_params_.set_string("enpoint", ep);
    auto msg = agent_.send(target_, target_endpoint_, "create_session", creation_params);

    msg->wait_for_completion(1000);

    if (msg->get_state() == yami::replied)
    {
      auto reply = msg->get_reply();
      id_ = reply.get_long_long("id");
    }
    else if (msg->get_state() == yami::rejected)
    {
      throw std::runtime_error("Unable to create session, " + msg->get_exception_msg());
    }
    else
    {
      throw std::runtime_error("Unable to create session, timed out");
    }
  }

  int receiving_session::get_id()
  {
    return id_;
  }

  void receiving_session::operator()(yami::incoming_message & im)
  {
    try
    {
      auto indata = im.get_raw_content();
      data_callback_(id_, indata);
    }
    catch (const std::exception& e)
    {
    }
  }
}