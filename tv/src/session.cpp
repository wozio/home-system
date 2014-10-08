#include "session.h"
#include "logger.h"
#include "yamicontainer.h"
#include <memory>

namespace home_system
{
namespace media
{

session::session(sources& sources, int id, int channel, std::string endpoint, std::string destination)
: sources_(sources),
  id_(id),
  channel_(channel),
  endpoint_(endpoint),
  destination_(destination)
{
  LOG("Create session id=" << id << " channel=" << channel_ << " endpoint=" << endpoint << " destination=" << destination);
  
  // fetch source for channel and create session
  source_ = sources_.get_source_for_channel(channel);
  source_->connect_session(channel,
    [this] (const void* buf, size_t length) { handle_stream_part(buf, length); });
}

session::~session()
{
  LOG("Delete session id=" << id_);
  
  source_->disconnect_session();
  
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

void session::handle_stream_part(const void* buf, size_t length)
{
  try
  {
    yami::parameters params;
    params.set_binary("payload", buf, length);
    params.set_integer("session", id_);
    std::unique_ptr<yami::outgoing_message> msg(YC.agent().send(endpoint_, destination_, "stream_part", params));
    
    if (!msg->wait_for_transmission(1000))
    {
      LOGERROR("ERROR: Stream part not transmitted");
      throw session_error("Stream part not transmitted", id_);
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
