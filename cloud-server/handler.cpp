#include "handler.h"
#include "logger.h"
#include <exception>

using namespace Poco;
using namespace Poco::Net;
using namespace std;

namespace home_system
{

data_t create_data()
{
  return data_t(new std::array<char, 1024>());
}

handler::handler(ws_t ws)
: ws_(ws)
{
}

handler::~handler()
{
  ws_->shutdown();
}

Poco::Net::WebSocket& handler::ws()
{
  return *ws_;
}

size_t handler::read(data_t data)
{
  int flags;
  size_t n = ws_->receiveFrame((*data).data(), (*data).size(), flags);
  
  if ((flags & WebSocket::FRAME_OP_BITMASK) == WebSocket::FRAME_OP_CLOSE)
  {
    throw runtime_error("WebSocket close request received");
  }
  else if ((flags & WebSocket::FRAME_OP_BITMASK) != WebSocket::FRAME_OP_TEXT)
  {
    throw runtime_error("Only text frames are supported on WebSocket");
  }
  return n;
}

void handler::send(data_t data, size_t data_size)
{
  ws_->sendFrame((*data).data(), data_size);
}

}
