#ifndef SYSTEMS_H
#define	SYSTEMS_H

#include "system.h"
#include <memory>

class systems;

typedef std::unique_ptr<systems> systems_t;
typedef std::shared_ptr<home_system::system> system_t;

class systems
{
public:
  
  systems(const systems& orig) = delete;
  ~systems();
  
  static systems_t create()
  {
    return systems_t(new systems());
  };
  
  void add(system_t system);
private:
  systems();

};

extern systems_t _systems;

#define SYSTEMS (*::_systems)


#endif	/* SYSTEMS_H */

