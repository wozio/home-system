#ifndef SESSION_H
#define	SESSION_H

#include <cstdint>

namespace home_system
{
namespace media
{

class client_sessions
{
public:
  static client_sessions& instance();
  size_t create(uint64_t);
};

}
}

#endif /* SESSION_H */
