#include "db.h"
#include "logger.h"
#include "yamicontainer.h"
#include "discovery.h"
#include <Poco/Data/SQLite/Connector.h>

using namespace Poco::Data;
using namespace Poco::Data::Keywords;
using namespace std;

namespace home_system
{
namespace media
{

db::db()
{
  lock_guard<mutex> lock(db_mutex_);

  LOG(DEBUG) << "DB created";
  SQLite::Connector::registerConnector();
  
  session().begin();
  session() << "CREATE TABLE IF NOT EXISTS channel_map (channel INTEGER PRIMARY KEY, local INTEGER, source TEXT)", now;
  session() << "CREATE TABLE IF NOT EXISTS names (channel INTEGER PRIMARY KEY, name TEXT)", now;
  session() << "CREATE TABLE IF NOT EXISTS recordings (id INTEGER PRIMARY KEY, channel INTEGER, start_time INTEGER, duration INTEGER)", now;
  session().commit();
  
  std::vector<string> sources;
  std::vector<long long> locals;
  std::vector<int> channels;
  session() << "SELECT channel,source,local FROM channel_map",
    into(channels), into(sources), into(locals), now;

  LOG(DEBUG) << "Defined channels number " << channels.size();
  
  for (size_t i = 0; i < sources.size(); ++i){
    source_local_to_global_[sources[i]][locals[i]] = channels[i];
  }
}

db::~db()
{
  lock_guard<mutex> lock(db_mutex_);
  SQLite::Connector::unregisterConnector();
}

Session& db::session()
{
  static Session s("SQLite", "tv.db");
  return s;
}

void db::check_and_add_local_channels(const std::string& source, size_t size, long long* local, const std::vector<std::string>& names)
{
  //LOG("Check and add local channels: source=" << source << ", size=" << size << ", size2=" << names.size());
  session().begin();
  for (size_t i = 0; i < size; ++i)
  {
    check_and_add_local_channel(source, local[i], names[i]);
  }
  session().commit();
}

void db::check_and_add_local_channel(const std::string& source, long long local, const std::string& name)
{
  lock_guard<mutex> lock(db_mutex_);
  //LOG("Check and add local channel: source=" << source << ", local=" << local << ", name=" << name);
  // TODO make try catch for out_of_range
  try
  {
    get_channel_from_source_local(source, local);
  }
  catch (const std::out_of_range&)
  {
    LOG(DEBUG) << "New channel from " << source << ": [" << local << "] " << name;
    add_channel(source, local, name);
  }
}

void db::add_channel(const std::string& source, long long local, const std::string& name)
{
  session() << "INSERT INTO channel_map (local, source) VALUES (:local, :source)", use(local), useRef(source), now;
  
  int channel = -1;
  session() << "SELECT channel FROM channel_map WHERE source = :s AND local = :l",
    into(channel), useRef(source), use(local), now;
  source_local_to_global_[source][local] = channel;
  
  session() << "INSERT INTO names (channel, name) VALUES (:channel, :name)",
    use(channel), useRef(name), now;
}

bool db::check_channel_existence(int channel)
{
  int count = -1;
  session() << "SELECT channel FROM channel_map WHERE channel = :channel",
    into(count), use(channel), now;
  return count == -1 ? false : true;
}

void db::get_channels(std::vector<int>& channels, std::vector<std::string>& names)
{
  session() << "SELECT channel FROM channel_map ORDER BY channel",
    into(channels), now;

  session() << "SELECT name FROM names ORDER BY channel",
    into(names), now;
}



int db::get_channel_from_source_local(const std::string& source, long long local)
{
  //LOG("get channel from source local: source= " << source << ", local=" << local);
  
  int channel = source_local_to_global_.at(source).at(local);

  //LOG("global channel=" << channel);

  return channel;
}

void db::get_sources_local_channels(int channel_id, std::vector<std::string>& sources,
  std::vector<long long>& local_channels)
{
  session() << "SELECT source,local FROM channel_map WHERE global = :global_channel",
    into(sources), into(local_channels), use(channel_id), now;
}

void db::get_sources_for_channel(int channel_id, std::vector<std::string>& sources)
{
  session() << "SELECT source FROM channel_map WHERE channel = :channel",
    into(sources), use(channel_id), now;
}

long long db::get_local_channel(int channel, const std::string& source)
{
  long long local;
  session() << "SELECT local FROM channel_map WHERE channel = :channel AND source = :source",
    into(local), use(channel), useRef(source), now;
  return local;
}

int db::get_channel_for_recording(int id)
{
  int channel = -1;
  session() << "SELECT channel FROM recordings WHERE id = :id",
    into(channel), use(id), now;
  return channel;
}

void db::get_recordings_to_start(std::vector<int>& recordings)
{
  time_t cur_time = time(NULL);
  session() <<
    "SELECT id FROM recordings WHERE start_time - 300 <= :cur_time AND (start_time + duration + 300 >= :cur_time OR duration = -1)",
    into(recordings), use(cur_time), use(cur_time), now;
}

void db::get_recordings_to_stop(std::vector<int>& recordings)
{
  time_t cur_time = time(NULL);
  session() <<
    "SELECT id FROM recordings WHERE start_time + duration + 300 <= :cur_time",
    into(recordings), use(cur_time), now;
}

int db::create_recording(int channel, time_t start_time, int duration)
{
  int id = 0;
  if (check_channel_existence(channel))
  {
    // getting first not used key
    session() <<
      "SELECT id+1 FROM recordings WHERE id+1 NOT IN (SELECT id FROM recordings)",
      into(id), now;
    session() <<
      "INSERT INTO recordings (id, channel, start_time, duration) VALUES (:id, :channel, :start_time, :duration)",
      use(id), use(channel), use(start_time), use(duration), now;
    LOG(DEBUG) << "created recording: id=" << id << " channel=" << channel;
  }
  return id;
}

std::string db::get_channel_name(int channel)
{
  string name;
  session() << "SELECT name FROM names WHERE channel = :channel",
    into(name), use(channel), now;
  return name;
}

}
}
