#include "db.h"
#include <Poco/Data/SQLite/Connector.h>

using namespace Poco::Data;
using namespace Poco;

namespace home_system
{
namespace media
{

db& db::instance()
{
  static db i;
  return i;
}

db::db()
{
  SQLite::Connector::registerConnector();

  // ensure that main table exist in database
  session() << "CREATE TABLE IF NOT EXISTS media (id INTEGER PRIMARY KEY, parent INTEGER, type INTEGER)", now;
}

db::~db()
{
  SQLite::Connector::unregisterConnector();
}

Session db::session()
{
  static Session session(SQLite::Connector::KEY, "media.db");
  return session;
}

}
}