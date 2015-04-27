#ifndef TRANSPONDER_H
#define	TRANSPONDER_H

#include "channel.h"
#include "channel_t.h"
#include <string>
#include <memory>
#include <map>

namespace home_system
{
namespace media
{

class transponder;
typedef std::shared_ptr<transponder> transponder_t;

class transponder
{
public:
  transponder();
  virtual ~transponder();
  
  virtual void tune(int fd) = 0;
  virtual void print(std::ostream& str) const = 0;
  virtual void save(std::ostream& str) const = 0;
  virtual bool isless(const transponder* right) = 0;
  
  void add_channel(channel_t c);  
private:
  // key is channel id
  std::map<uint64_t, channel_t> channels_;
};

std::ostream& operator<< (std::ostream& out, const transponder& t);
std::ostream& operator<< (std::ostream& out, const transponder_t t);

}
}

#endif	/* TRANSPONDER_H */

