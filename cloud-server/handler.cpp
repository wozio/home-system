#include "handler.h"
#include <exception>

using namespace Poco::Net;
using namespace std;

namespace home_system
{

handler::handler(ws_t ws)
: ws_(ws)
{
}

handler::~handler()
{
  ws_->shutdown();
}

size_t handler::read(std::unique_ptr<char[]>& data, size_t data_size)
{
  int flags;
  size_t n = ws_->receiveFrame(data.get(), data_size, flags);
  
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

void handler::send(std::unique_ptr<char[]>& data, size_t data_size)
{
  ws_->sendFrame(data.get(), data_size, WebSocket::FRAME_OP_TEXT);
}

}
