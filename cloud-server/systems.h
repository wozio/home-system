#ifndef SYSTEMS_H
#define	SYSTEMS_H

#include "system.h"
#include <memory>

namespace home_system
{

class systems;

typedef std::unique_ptr<systems> systems_t;

class systems
{
public:
  
  systems(const systems& orig) = delete;
  ~systems();
  
  static systems_t create()
  {
    return systems_t(new systems());
  };
  
  system_t get(const std::string& user);
  
  void add(const std::string& user, system_t system);
  void remove(system_t system);
  
private:
  systems();
  
  std::map<std::string, system_t> user_to_system_;

};

}

extern home_system::systems_t _systems;

#define SYSTEMS (*::_systems)


#endif	/* SYSTEMS_H */

