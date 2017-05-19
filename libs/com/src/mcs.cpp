#include "mcs.h"

#include <boost/asio.hpp>

using namespace boost;
using namespace boost::asio;

namespace home_system
{
namespace com
{

void multicast_send(const std::string& msg)
{
  static io_service io_service;
  static ip::udp::endpoint endpoint(ip::address::from_string("239.255.255.255"), 10001);
  static ip::udp::socket socket(io_service, endpoint.protocol());
  socket.send_to(buffer(msg), endpoint);
  //socket.shutdown(ip::udp::socket::shutdown_send);
  //socket.close();
}

}
}
