#ifndef TRANSPONDERS_H
#define	TRANSPONDERS_H

#include "transponder.h"
#include <set>
#include <vector>
#include <memory>

namespace home_system
{
namespace media
{

class transponders
{
public:
  transponders(const std::string& transponder_file);
  ~transponders();
  
  size_t size();
  
  typedef std::shared_ptr<transponder> transponder_t;
  
  transponder_t next();
  transponder_t current();
  transponder_t find_or_create(std::string& definition);
  
  transponder_t set(transponder_t t);
  
  void print();
private:
  transponders(const transponders&){};
  // tried unique_ptr but it fails on GCC 4.7.3 with Internal Compiler Error when push_back into vector
  std::set<transponder_t> transponders_;
  
  std::set<transponder_t>::iterator current_;
};

}
}

#endif	/* TRANSPONDERS_H */

