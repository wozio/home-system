#include "sources.h"
#include "discovery.h"
#include "logger.h"

using namespace std;

namespace home_system
{
namespace media
{

sources::sources(db& db)
: db_(db)
{
  DISCOVERY.subscribe([&] (const std::string& service, bool available)
  {
    if (!available)
    {
      auto old_source = sources_.find(service);
      if (old_source != sources_.end())
      {
        LOG("Source not available: " << service);
        sources_.erase(old_source->second->endpoint());
        sources_names_.erase(service);
      }
    }
  });
}

sources::~sources()
{
}

void sources::source_available(const std::string& service, const std::string& ye)
{
  if (sources_.find(ye) == sources_.end())
  {
    source_t new_s = make_shared<source>(db_, service, ye);
    sources_[ye] = new_s;
    sources_names_[service] = new_s;
  }
}

source_t sources::get_source_for_channel(int channel)
{
  std::vector<std::string> dbsources;
  db_.get_sources_for_channel(channel, dbsources);
  
  LOG("sources found for channel " << channel << " " << dbsources.size());
  
  for (auto dbsource : dbsources)
  {
    auto sit = sources_names_.find(dbsource);
    if (sit != sources_names_.end())
    {
      LOG("Available source for channel: " << sit->first << "(" << sit->second->endpoint() << ")");
      return (*sit).second;
    }
  }
  
  throw runtime_error("source not found or not available");
}

void sources::handle_stream_part(const std::string& source_endpoint, int source_session, const void* buf, size_t length)
{
  auto s = sources_.find(source_endpoint);
  if (s != sources_.end())
  {
    (*s).second->handle_stream_part(source_session, buf, length);
  }
}

void sources::handle_session_deleted(const std::string& source_endpoint, int source_session)
{
  auto s = sources_.find(source_endpoint);
  if (s != sources_.end())
  {
    (*s).second->handle_session_deleted(source_session);
  }
}

}
}
