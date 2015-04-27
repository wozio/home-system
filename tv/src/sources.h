#ifndef SOURCES_H
#define	SOURCES_H

#include "source.h"
#include "db.h"


namespace home_system
{
namespace media
{

class source_not_found
: public std::runtime_error
{
public:
  source_not_found()
    : std::runtime_error("Source not found or not available")
    {
    }
};

class sources
{
public:
  sources(db& db);
  ~sources();

  bool check_source(const std::string& service);
  
  void source_available(const std::string& service, const std::string& ye);
  void source_not_available(const std::string& service);
  
  int create_session(int channel, const std::string& client_endpoint, const std::string& client);
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
