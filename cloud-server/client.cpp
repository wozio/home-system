#include "client.h"
#include "logger.h"

using namespace Poco::Net;
using namespace Poco;
using namespace std;

client::client(ws_t ws)
: handler(ws)
{
  LOG("New client connected, performing handshake");
  
  unique_ptr<char[]> data(new char[1025]);
  int flags;
  int n;
  try
  {
    n = ws->receiveFrame(data.get(), 1024, flags);

    if ((flags & WebSocket::FRAME_OP_BITMASK) == WebSocket::FRAME_OP_CLOSE)
    {
      ws->shutdown();
    }
    else if ((flags & WebSocket::FRAME_OP_BITMASK) == WebSocket::FRAME_OP_TEXT && n > 0)
    {
      // for now just echo the message
      ws->sendFrame(data.get(), n, WebSocket::FRAME_OP_TEXT);
    }
  }
  catch (const TimeoutException& e)
  {
    ws->shutdown();
  }
  catch (const Exception& e)
  {
    LOGWARN("EXCEPTION: " << e.displayText());
  }
  catch (const std::exception& e)
  {
    LOGWARN("EXCEPTION: " << e.what());
  }
}

client::~client()
{
}
