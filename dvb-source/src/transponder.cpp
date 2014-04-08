#include "transponder.h"
#include <sstream>
#include <memory>

namespace home_system
{
namespace media
{

transponder::transponder()
{
}

transponder::~transponder()
{
}

void transponder::add_channel(channel_t c)
{
  channels_[c->get_id()] = c;
}

std::ostream& operator<< (std::ostream& out, const transponder& t)
{
  t.print(out);
  return out;
}

std::ostream& operator<< (std::ostream& out, const std::shared_ptr<transponder> t)
{
  out << *(t.get());
  return out;
}

}
}
