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
  HANDLERS.add(system);
}

}
