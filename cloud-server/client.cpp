#include "client.h"
#include "logger.h"


using namespace Poco;
using namespace std;

namespace home_system
{

client::client(ws_t ws)
: handler(ws)
{
  LOG("New client connected, performing client logging in");
  
  // any exception thrown from logging in will lead to deleting client
  unique_ptr<char[]> data(new char[1025]);
  int n = read(data, 1024);
  // for now just echo the message
  send(data, n);
}

client::~client()
{
}

}