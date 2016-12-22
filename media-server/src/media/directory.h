#ifndef DIRECTORY_H_
#define DIRECTORY_H_

#include "media.h"

namespace home_system
{
namespace media
{

class directory : public container
{
public:
  directory(id_t id, const std::string& ps);
  ~directory();

protected:
};

}
}

#endif /* DIRECTORY_H_ */
