#ifndef SOURCES_H
#define	SOURCES_H

#include "source.h"
#include "db.h"


namespace home_system
{
namespace media
{

class sources
{
public:
  sources(db& db);
  ~sources();

  bool check_source(const std::string& service);
  
  void source_available(const std::string& service, const std::string& ye);
  void source_not_available(const std::string& service);
  
  // returns client session id
  int create_session(int channel, const std::string& destination, const std::string& endpoint);
  void delete_session(int client_session);
  
  source_t operator[](const std::string& source);
  
private:
  db& db_;
  
  // key is endpoint of the service
  typedef std::map<std::string, source_t> source_map;
  source_map sources_;
  source_map sources_names_;
};

}
}

#endif	/* SOURCES_H */
