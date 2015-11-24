#include "systems.h"
#include "logger.h"

namespace home_system
{

systems::systems()
{
}

systems::~systems()
{
}

void systems::add(system_t system)
{
  LOG("System add");
  system_ = system;
}

system_t systems::get()
{
  return system_;
}

void systems::remove(system_t system)
{
  LOG("System remove");
  system_.reset();
}

}
