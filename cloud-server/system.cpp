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
  unique_ptr<char[]> data(new char[1025]);
  int n = read(data, 1024);
  // for now just echo the message
  send(data, n);
}

system::~system()
{
}

}