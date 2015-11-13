#include "systems.h"
#include "handlers.h"

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