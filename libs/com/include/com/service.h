#pragma once

#include "utils/ios_wrapper.h"
#include <boost/asio.hpp>
#include <yami4-cpp/yami.h>
#include <list>

namespace home_system
{
namespace com
{

class service
{
public:
  virtual ~service();

  // for yami message receive
  void operator()(yami::incoming_message &im);

  std::string name() const;
  std::string ye() const;

  virtual void init();
  virtual void deinit();

  // for discovery
  virtual void on_remote_service_availability(const std::string &name, bool availability){};

protected:
  service(const std::string &name, bool initialize = true);
  std::string name_;

  // yami message receive
  virtual void on_msg(yami::incoming_message &im);

private:
  home_system::utils::ios_wrapper ios_;
  boost::asio::deadline_timer notify_dt_;
  bool initialize_;

  void set_notify_timeout();
  void on_notify_timeout(const boost::system::error_code &error);

  // multicast send
  void send_hello();
  void send_notify();
  void send_bye();
};

}
}
