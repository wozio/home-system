#include "service.h"
#include "mcs.h"
#include "discovery.h"
#include "utils/logger.h"
#include "yamicontainer.h"
#include <sstream>
#include <fstream>
#include <string>

using namespace std;
using namespace boost;
using namespace boost::asio;

namespace home_system
{
namespace com
{

service::service(const std::string &name, bool initialize)
    : name_(name),
      notify_dt_(ios_.io_service()),
      initialize_(initialize)
{
  if (initialize_)
  {
    init();
  }
}

service::~service()
{
  if (initialize_)
  {
    deinit();
  }
}

void service::init()
{
  YC.register_handler(name_, [this](yami::incoming_message &im) {
    this->on_msg(im);
  });

  set_notify_timeout();

  LOG(DEBUG) << "Initialized service with name: " << name_ << " and YAMI endpoint: " << ye();

  send_hello();
}

void service::deinit()
{
  LOG(DEBUG) << "Deinitialized service with name: " << name_;
  YC.unregister_handler(name_);

  notify_dt_.cancel();

  send_bye();
}

std::string service::name() const
{
  return name_;
}

std::string service::ye() const
{
  return YC.endpoint();
}

void service::on_msg(yami::incoming_message &im)
{
  LOG(WARNING) << name_ << ": unknown message: " << im.get_message_name();
  im.reject("unknown message");
}

void service::operator()(yami::incoming_message &im)
{
//LOG(TRACE) << "message " << im.get_message_name() << " from " << im.get_source();
  try
  {
    on_msg(im);
  }
  catch (const std::exception &e)
  {
    LOG(WARNING) << name_ << ": EXCEPTION: " << e.what();
    im.reject(e.what());
  }
}

// multicast send

void service::send_hello()
{
  ostringstream str;
  str << "hello\n"
      << name_ << "\n"
      << ye();
  multicast_send(str.str());
}

void service::send_notify()
{
  ostringstream str;
  str << "notify\n"
      << name_ << "\n"
      << ye();
  multicast_send(str.str());

  set_notify_timeout();
}

void service::set_notify_timeout()
{
  notify_dt_.cancel();
  notify_dt_.expires_from_now(posix_time::seconds(rand() % 4 + 1));
  notify_dt_.async_wait([&](const boost::system::error_code &error) { on_notify_timeout(error); });
}

void service::send_bye()
{
  ostringstream str;
  str << "bye\n"
      << name_;
  multicast_send(str.str());
}

void service::on_notify_timeout(const boost::system::error_code &error)
{
  if (!error)
  {
    send_notify();
  }
}
}
}
