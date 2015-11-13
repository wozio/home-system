#include "systems.h"
#include "handlers.h"

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
  HANDLERS.add(system);
}

system_t systems::get()
{
  return system_;
}

}
