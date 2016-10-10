#ifndef CLIENTS_H
#define	CLIENTS_H

#define CLIENTS home_system::media::clients::instance()

namespace home_system
{
namespace media
{

class clients
{
public:
  clients& instance();
  
  size_t create(uint64_t);
  
private:
  clients();
  ~clients();
};

}
}

#endif	/* CLIENTS_H */

