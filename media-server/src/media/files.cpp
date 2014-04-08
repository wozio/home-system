#include "files.h"
#include "../db.h"
#include "directory.h"
#include <boost/filesystem.hpp>
#include <string>
using namespace Poco::Data;
using namespace boost::filesystem;
using namespace std;

namespace home_system
{
  namespace media
  {
    files::files(id_t id) :
    container(id)
    {
      DB << "CREATE TABLE IF NOT EXISTS files(id INTEGER, path TEXT)", now;
      DB << "CREATE TABLE IF NOT EXISTS directories(id INTEGER, path TEXT)", now;
    }

    files::~files()
    {
    }
  }
}
