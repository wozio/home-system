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

system_t systems::get(const std::string& user)
{
  return user_to_system_.at(user);
}

void systems::remove(system_t system)
{
  LOG(DEBUG) << "System remove";
  for (auto it = user_to_system_.begin(); it != user_to_system_.end();)
  {
    if (it->second == system)
    {
      user_to_system_.erase(it++);
    }
    else
    {
      ++it;
    }
  }
}

void systems::add(const std::string& user, system_t system)
{
  LOG(DEBUG) << "Adding system for user " << user;
  user_to_system_[user] = system;
}

}
