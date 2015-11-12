#include "system_handler.h"

ws_handler::ws_handler(std::shared_ptr<Poco::Net::WebSocket> ws, on_read_t on_read)
: ws_(ws),
  on_read_(on_read),
  run_thread_(true),
  thr_([this] () {this->thr_exec();})
{
}

ws_handler::~ws_handler()
{
  run_thread_ = false;
  thr_.join();
}

void ws_handler::thr_exec()
{
  std::unique_ptr<char[]> data(new char[1025]);
  int flags;
  int n;
  do
  {
    try
    {
      n = ws_->receiveFrame(data.get(), 1024, flags);

      if ((flags & WebSocket::FRAME_OP_BITMASK) == WebSocket::FRAME_OP_CLOSE)
      {
        break;
      }

      if ((flags & WebSocket::FRAME_OP_BITMASK) == WebSocket::FRAME_OP_TEXT && n > 0)
      {
        on_read_(data.get(), n);
      }
    }
    catch (const runtime_error& e)
    {
      LOGWARN("EXCEPTION: runtime_error: " << e.what());
    }
    catch (const TimeoutException& e)
    {
      // do nothing with this one
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
  while ((flags & WebSocket::FRAME_OP_BITMASK) != WebSocket::FRAME_OP_CLOSE);

  LOG("Closing connection");
  ws_->shutdown();
  ws_.reset();
}

void ws_handler::send(char* buf, size_t size)
{
  if (ws_)
  {
    LOG("Sending " << size << " bytes to system");
    ws_->sendFrame(buf, size, WebSocket::FRAME_OP_TEXT);
  }
}
