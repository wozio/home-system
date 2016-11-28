#ifndef CHANNEL_H
#define	CHANNEL_H

#include "transponder.h"
#include "channel_t.h"
#include <memory>
#include <string>
#include <vector>

namespace home_system
{
namespace media
{

class transponder;
typedef std::shared_ptr<transponder> transponder_t;

class channel
{
public:
  channel(uint64_t id, const std::string& name, transponder_t t, uint16_t service_id = 0);
  ~channel();
  
  void tune(int fd);
  
  uint64_t get_id();
  
  std::string get_name();
  std::shared_ptr<transponder> get_transponder();
  uint16_t service_id();
  
  void print();
  void save(std::ostream& str);
private:
  uint64_t id_;
  std::string name_;
  std::shared_ptr<transponder> transponder_;
  uint16_t service_id_;
};

}
}

#endif	/* CHANNEL_H */

