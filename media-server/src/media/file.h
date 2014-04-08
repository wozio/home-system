#ifndef FILE_H_
#define FILE_H_

#include "media.h"

namespace home_system
{
namespace media
{

class file : public item
{
public:
  file(id_t id, const std::string& ps);
  ~file();

private:
  void build_uri();
};

}
}

#endif /* FILE_H_ */
