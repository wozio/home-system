#include "file.h"

#include <boost/filesystem.hpp>

using namespace Poco::Data;
using namespace boost::filesystem;
using namespace std;

namespace home_system
{
namespace media
{

file::file(id_t id, const std::string& ps) :
  item(id)
{
  path p(ps);
}

file::~file()
{
}

}
}
