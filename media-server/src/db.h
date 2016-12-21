#ifndef DB_H_
#define DB_H_

#include <Poco/Data/Common.h>

namespace home_system
{
namespace media
{

class db
{
public:
  static db& instance();
  
  Poco::Data::Session session();
  ~db();
private:
  db();
};

}
}

#define DB home_system::media::db::instance().session()

#endif /* DB_H_ */
