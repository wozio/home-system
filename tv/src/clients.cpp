#include "pch.h"
#include "clients.h"

namespace home_system
{
namespace media
{

clients& clients::instance()
{
  static clients i;
  return i;
}

clients::clients()
{
  
}

clients::~clients()
{
  
}

size_t clients::create(uint64_t)
{
  return 0;
}

}
}
