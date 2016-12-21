#include "media.h"
#include "../db.h"
#include "root.h"
#include "files.h"
#include "directory.h"
#include "file.h"
#include <boost/lexical_cast.hpp>

using namespace Poco::Data;
using namespace boost;
using namespace std;

namespace home_system
{
  namespace media
  {

    // base classes for media objects
    object::object(id_t id)
      : m_id(id)
    {
      try
      {
        m_id_str = lexical_cast<string>(id);
      }
      catch (bad_lexical_cast &)
      {
      }
    }

    object::~object()
    {
    }

    id_t object::id() const
    {
      return m_id;
    }

    string object::id_str() const
    {
      
      return m_id_str;
    }

    id_t object::parent_id() const
    {
      id_t out = 0;
      DB << "SELECT parent FROM media WHERE id = :i", into(out), use(id()), now;
      return out;
    }

    string object::parent_id_str() const
    {
      string out;
      DB << "SELECT parent FROM media WHERE id = :i", into(out), use(id()), now;
      return out;
    }

    item::item(id_t id) :
    object(id)
    {
    }

    item::~item()
    {
    }

    container::container(id_t id) :
    object(id)
    {
    }

    container::~container()
    {
    }

    void container::get_children(vector<id_t> &objects)
    {
      objects = m_children;
    }

    void container::add_child(object_ptr o)
    {
      MEDIA.register_object(o);
      m_children.push_back(o->id());
    }

    size_t container::size() const
    {
      return m_children.size();
    }

    media& media::instance()
    {
      static media m;
      return m;
    }

    media::media()
    {
      DB << "CREATE TABLE IF NOT EXISTS media(id INTEGER, parent INTEGER, type INTEGER)", now;

      register_object(object_ptr(new root));
    }

    object_ptr media::get(id_t id)
    {
      map<id_t, object_ptr>::iterator i = m_objects.find(id);
      if (i != m_objects.end())
      {
        return i->second;
      }
      throw object_not_found(id);
    }

    void media::register_object(object_ptr object)
    {
      m_objects[object->id()] = object;
    }

    id_t media::get_new_id(id_t parent, object_type type)
    {
      id_t id;
      DB << "SELECT MAX(id) FROM media", into(id), now;
      ++id;
      DB << "INSERT INTO media VALUES (:i, :p, :t)", use(id), use(parent), use(type), now;
      return id;
    }

    void media::get_new_id(const std::vector<id_t>& parents, const std::vector<object_type>& types, std::vector<id_t>& ids)
    {
      size_t s = parents.size();
      ids.resize(s);
      id_t maxid;
      DB << "SELECT MAX(id) FROM media", into(maxid), now;
      for (size_t i = 0; i < s; ++i)
      {
        ids[i] = ++maxid;
      }
      DB << "INSERT INTO media VALUES (:i, :p, :t)", use(ids), use(parents), use(types), now;
    }

  }
}
