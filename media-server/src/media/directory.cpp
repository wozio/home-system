#include "directory.h"
#include "file.h"
#include <boost/filesystem.hpp>

using namespace Poco::Data;
using namespace boost::filesystem;
using namespace std;

namespace home_system
{
namespace media
{

  directory::directory(id_t id, const std::string& ps) :
  container(id)
{
  path p(ps);
  directory_iterator end;
  vector<string> files, directories;
  for (directory_iterator itr(p); itr != end; ++itr)
  {
    id_t subid = 0;
    string subp = itr->path().string();
    if (is_regular_file(itr->status()))
    {
      // if we don't have file in our database adding it to list of files to add
      // else just creating object
      DB << "SELECT id FROM files WHERE path = :path", into(subid), use(subp), now;
      if (subid == 0)
      {
        files.push_back(subp);
      }
      else
      {
        add_child(object_ptr(new file(subid, subp)));
      }
    }
    else if (is_directory(itr->status()))
    {
      DB << "SELECT id FROM directories WHERE path = :path", into(subid), use(subp), now;
      if (subid == 0)
      {
        directories.push_back(subp);
      }
      else
      {
        add_child(object_ptr(new directory(subid, subp)));
      }
    }
  }

  vector<object_type> types;
  vector<id_t> parents;
  vector<id_t> files_ids, directories_ids;

  DB.begin();

  if (files.size() > 0)
  {
    types.resize(files.size(), OBJECT_TYPE_FILE);
    parents.resize(files.size(), id);
    MEDIA.get_new_id(parents, types, files_ids);
    DB << "INSERT INTO files VALUES(:i, :p)", use(files_ids), use(files), now;
  }

  if (directories.size() > 0)
  {
    types.clear();
    types.resize(directories.size(), OBJECT_TYPE_DIRECTORY);
    parents.clear();
    parents.resize(directories.size(), id);
    MEDIA.get_new_id(parents, types, directories_ids);
    DB << "INSERT INTO directories VALUES(:i, :p)", use(directories_ids), use(directories), now;
  }

  DB.commit();

  for (size_t i = 0; i < files_ids.size(); ++i)
  {
    add_child(object_ptr(new file(files_ids[i], files[i])));
  }

  for (size_t i = 0; i < directories_ids.size(); ++i)
  {
    add_child(object_ptr(new directory(directories_ids[i], directories[i])));
  }
}

directory::~directory()
{
}

}
}
