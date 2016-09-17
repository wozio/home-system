#include "pch.h"
#include "client_sessions.h"

namespace home_system
{
namespace media
{

client_sessions& client_sessions::instance()
{
  static client_sessions cs;
  return cs;
}

size_t client_sessions::create(uint64_t)
{
  return 0;
}

}
}
