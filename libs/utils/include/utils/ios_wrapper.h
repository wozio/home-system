#pragma once

#include <boost/asio.hpp>
#include <thread>
#include <mutex>
#include <memory>

namespace home_system
{
namespace utils
{

class ios_wrapper
{
public:
  ios_wrapper();
  ~ios_wrapper();
  ios_wrapper(const ios_wrapper &) = delete;
  ios_wrapper &operator=(const ios_wrapper &) = delete;

  boost::asio::io_service &io_service();
  void stop_ios();

private:
  boost::asio::io_service io_service_;
  std::thread io_thread_;
  std::shared_ptr<boost::asio::io_service::work> work_;

  void start_ios();
  void thread_exec();
};
}
}
