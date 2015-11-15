#include "handlers.h"
#include "logger.h"
#include <utility>

using namespace std;

namespace home_system
{

handlers::handlers()
{
}

handlers::~handlers()
{
  
}

void handlers::add(handler_t handler)
{
  // start reading from assiociated websocket
  ios_.io_service().post([this, handler] ()
    {
      this->read(handler);
    }
  );
}

void handlers::read(handler_t handler)
{
  try
  {
    auto data = create_data();
    int n = handler->read(data);
    if (n > 0)
    {
      handler->on_read(data, n);
    }
    ios_.io_service().post([this, handler] ()
      {
        this->read(handler);
      }
    );
  }
  catch (...)
  {
    // anything thrown from handler's read leads to deleting handler
    handler->shutdown();
  }
}

void handlers::post_send(handler_t handler, data_t data, size_t data_size)
{
  ios_.io_service().post([this, handler, data, data_size] ()
    {
      this->send(handler, data, data_size);
    }
  );
}

void handlers::send(handler_t handler, data_t data, int data_size)
{
  try
  {
    handler->send(data, data_size);
  }
  catch (...)
  {
    // anything thrown from handler's send leads to deleting handler
    handler->shutdown();
  }
}

}
