#include "system.h"
#include "logger.h"

using namespace std;

namespace home_system
{

system::system(ws_t ws)
: handler(ws)
{
  LOG("New system connected, performing system logging in");
  
  // any exception thrown from logging in will lead to deleting system
  auto data = create_data();
  int n = read(data);
  // for now just echo the message
  send(data, n);
}

system::~system()
{
}

}