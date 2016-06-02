#ifndef TRANSPONDER_H
#define	TRANSPONDER_H

#include "channel.h"
#include "channel_t.h"
#include <vector>
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
  virtual bool isless(const transponder_t right) = 0;
  
  void add_channel(channel_t c);
  void remove_channel(uint64_t cid);
  void get_channels(std::vector<channel_t>& channel_list);
private:
  // key is channel id
  std::map<uint64_t, channel_t> channels_;
};

std::ostream& operator<< (std::ostream& out, const transponder& t);
std::ostream& operator<< (std::ostream& out, const transponder_t t);

struct transponder_comp {
  bool operator() (const transponder_t& lt, const transponder_t& rt) const;
};

}
}

#endif	/* TRANSPONDER_H */

