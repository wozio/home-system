#include "root.h"
#include "files.h"
#include "../db.h"

using namespace Poco::Data;
using namespace boost;

namespace home_system
{
namespace media
{

id_t check_container(size_t type)
{
  id_t id = 0;
  DB << "SELECT id FROM media WHERE type = :t", use(type), into(id), now;
  if (id == 0)
  {
    id = MEDIA.get_new_id(0, OBJECT_TYPE_FILES);
  }
  return id;
}

root::root() :
  container(0)
{
  size_t c = 0;
  DB << "SELECT COUNT(id) FROM media WHERE type = :t", use(OBJECT_TYPE_ROOT), into(c), now;
  if (c == 0)
  {
    // root's id = 0 parent = 0
    DB << "INSERT INTO media VALUES (0, -1, :t)", use(OBJECT_TYPE_ROOT), now;
  }
  // our standard containers
  add_child(object_ptr(new files(check_container(OBJECT_TYPE_FILES))));
}

root::~root()
{
}

}
}
