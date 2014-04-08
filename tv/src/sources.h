#ifndef SOURCES_H
#define	SOURCES_H

#include "source.h"
#include "db.h"
#include <memory>
#include <map>

#define SOURCES home_system::media::sources::instance()

namespace home_system
{
namespace media
{

typedef std::shared_ptr<source> source_t;

class sources
{
public:
  sources(db& db);
  ~sources();

  void source_available(const std::string& service, const std::string& ye);
  source_t get_source_for_channel(int channel);
  void handle_stream_part(const std::string& source_endpoint, int source_session, const void* buf, size_t length);
  void handle_session_deleted(const std::string& source_endpoint, int source_session);
  
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
