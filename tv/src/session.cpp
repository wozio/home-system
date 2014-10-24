#include "session.h"
#include "logger.h"
#include "yamicontainer.h"
#include <memory>

namespace home_system
{
namespace media
{

session::session(int id, std::string endpoint, std::string destination)
: id_(id),
  endpoint_(endpoint),
  destination_(destination)
{
  LOG("Create session id=" << id << " endpoint=" << endpoint << " destination=" << destination);
}

session::~session()
{
  LOG("Delete client session id=" << id_);
  
  try
  {
    yami::parameters params;
    params.set_integer("session", id_);
    YC.agent().send(endpoint_, destination_, "session_deleted", params);
  }
  catch (const std::exception& e)
  {
    LOGWARN("EXCEPTION: " << e.what());
  }
}

void session::stream_part(const void* buf, size_t length)
{
  try
  {
    yami::parameters params;
    params.set_binary("payload", buf, length);
    params.set_integer("session", id_);
    std::unique_ptr<yami::outgoing_message> msg(YC.agent().send(endpoint_, destination_, "stream_part", params));
    
    if (!msg->wait_for_completion(1000))
    {
      LOGERROR("ERROR: Timeout on stream part sending");
      throw session_error("Timeout on stream part sending", id_);
    }
    
    switch (msg->get_state())
    {
    case yami::replied:
      break;
    case yami::rejected:
      LOGERROR("ERROR: Stream part message rejected: " << msg->get_exception_msg());
      throw session_error("Stream part message rejected: " + msg->get_exception_msg(), id_);
      break;
    default:
      LOGERROR("ERROR: Stream part message not replied");
      throw session_error("Stream part message not replied", id_);
      break;
    }
  }
  catch (const std::exception& e)
  {
    LOGERROR("EXCEPTION: " << e.what());
    std::string s("Exception when sending stream part to client: ");
    s.append(e.what());
    throw session_error(s, id_);
  }
}

}
}
