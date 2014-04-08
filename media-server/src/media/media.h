#ifndef MEDIA_H_
#define MEDIA_H_

#include <vector>
#include <exception>
#include <boost/shared_ptr.hpp>
#include "../db.h"
#include "object_types.h"
#include "id_t.h"

namespace home_system
{

namespace media
{

// base classes for media objects

class object
{

public:
  virtual ~object();
  id_t id() const;
  std::string id_str() const;
  id_t parent_id() const;
  std::string parent_id_str() const;

protected:
  object(id_t id);

private:
  id_t m_id;
  std::string m_id_str;
};

typedef boost::shared_ptr<object> object_ptr;

class item : public object
{
public:
  ~item();

protected:
  item(id_t id);
};

class container : public object
{

public:
  ~container();
  void get_children(std::vector<id_t> &objects);
  void add_child(object_ptr o);

  size_t size() const;

protected:
  container(id_t id);
  std::vector<id_t> m_children;
};

// exceptions

class object_not_found : public std::out_of_range
{

private:
  id_t m_id;

public:
  object_not_found(id_t id_) :
    std::out_of_range("Object not found"),
    m_id(id_)
  {
  }

  id_t id()
  {
    return m_id;
  }
};

class media
{
public:
    static media& instance();
    object_ptr get(id_t id);
    id_t get_new_id(id_t parent, object_type type);
    void get_new_id(const std::vector<id_t>& parents, const std::vector<object_type>& types, std::vector<id_t>& ids);

    void register_object(object_ptr object);
private:
    media();

    std::map<id_t, object_ptr> m_objects;
};

}
}

#define MEDIA home_system::media::media::instance()

#endif /* MEDIA_H_ */
