#include "systems.h"

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
  system_ = system;
}

system_t systems::get()
{
  return system_;
}

void systems::remove(system_t system)
{
  system_.reset();
}

}
