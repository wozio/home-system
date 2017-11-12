#pragma once

#include <boost/asio.hpp>
#include <thread>
#include <vector>
#include <memory>

namespace home_system
{
namespace utils
{

class ios_wrapper
{
public:
  ios_wrapper();
  ios_wrapper(size_t num_of_threads);
  ~ios_wrapper();
  ios_wrapper(const ios_wrapper &) = delete;
  ios_wrapper &operator=(const ios_wrapper &) = delete;

  boost::asio::io_service &io_service();
  void stop_ios();

private:
  boost::asio::io_service io_service_;
  std::vector<std::thread> io_threads_;
  std::shared_ptr<boost::asio::io_service::work> work_;
};
}
}
