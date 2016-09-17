#include "pch.h"
#include "sources.h"
#include "logger.h"

using namespace std;

namespace home_system
{
namespace media
{

sources::sources(db& db)
: db_(db)
{
  LOG(DEBUG) << "Sending sources wakeup";
  // sending Wake On Lan message
  // TODO make it on separate function, maybe move to control-server
  string strmac("000C7620C5E0");
  unsigned int mac[6];
  for (size_t j = 0; j < 6; j++)
  {
    std::stringstream s;
    s << hex << strmac.substr(j * 2, 2);
    s >> mac[j];
  }
  
  uint8_t mp[108];
  size_t j = 0;
  for (; j < 6; j++)
    mp[j] = 0xFF;
  while (j < 102)
  {
    for (size_t i = 0; i < 6; ++i)
    {
      mp[j++] = mac[i];
    }
  }
  boost::asio::io_service srv;
  boost::asio::ip::udp::endpoint endpoint(boost::asio::ip::address::from_string("255.255.255.255"), 8);
  boost::asio::ip::udp::socket socket(srv, endpoint.protocol());
  socket.set_option(boost::asio::ip::udp::socket::broadcast(true));
  socket.send_to(boost::asio::buffer(mp), endpoint);
}

sources::~sources()
{
}

bool sources::check_source(const std::string& service)
{
  return sources_names_.find(service) != sources_names_.end();
}

void sources::source_available(const std::string& service, const std::string& ye)
{
  source_t new_s = make_shared<source>(db_, service, ye);
  sources_[ye] = new_s;
  sources_names_[service] = new_s;
}

void sources::source_not_available(const std::string& service)
{
  sources_names_[service]->not_available();
  sources_.erase(sources_names_[service]->endpoint());
  sources_names_.erase(service);
}

source_t sources::operator[](const std::string& source)
{
  auto s = sources_names_.find(source);
  if (s != sources_names_.end())
  {
    return s->second;
  }
  else
  {
    throw std::runtime_error(string("Source not found: ") + source);
  }
}

int sources::create_session(int channel, const std::string& client_endpoint, const std::string& client)
{
  std::vector<std::string> dbsources;
  db_.get_sources_for_channel(channel, dbsources);
  
  LOG(DEBUG) << "sources found for channel " << channel << " " << dbsources.size();
  
  for (auto dbsource : dbsources)
  {
    auto sit = sources_names_.find(dbsource);
    if (sit != sources_names_.end())
    {
      LOG(DEBUG) << "Available source for channel: " << sit->first << "(" << sit->second->endpoint() << ")";
      return sit->second->create_session(channel, client_endpoint, client);
    }
  }
  
  throw runtime_error("Source not found or not available");
}

void sources::delete_session(int client_session)
{
  source::source_for_session(client_session)->delete_session(client_session);
}

}
}
