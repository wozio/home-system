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
  auto data = create_data();
  int n = read(data);
  // for now just echo the message
  LOG(n);
  send(data, n);
}

client::~client()
{
  LOG("Client disconnected");
}

}