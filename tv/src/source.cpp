#include "source.h"
#include "yamicontainer.h"
#include "logger.h"
#include <memory>

using namespace std;

namespace home_system
{
namespace media
{

source::source(db& db, const std::string& name, const std::string& ye)
: db_(db),
  name_(name),
  ye_(ye)
{
}

source::~source()
{
  // TODO add session state callback to notify client about source problems
}

std::string source::endpoint()
{
  return ye_;
}

// TODO add session state callback to notify client about source problems
void source::connect_session(int channel, stream_part_callback_t stream_part_callback)
{
  stream_part_callback_ = stream_part_callback;
  
  // starting session on source
  long long local = db_.get_local_channel(channel, name_);
  
  yami::parameters params;
  
  params.set_long_long("channel", local);
  params.set_string("destination", "tv");
  params.set_string("endpoint", YC.endpoint());
  
  LOG("Create session " << " channel=" << channel << "(" << hex << local << ")");
  
  unique_ptr<yami::outgoing_message> message(YC.agent().send(ye_, name_, "create_streaming_session", params));
  
  message->wait_for_completion(1000);
  
  if (message->get_state() == yami::replied)
  {
    source_session_id_ = message->get_reply().get_integer("session");
    
    LOG("Got source session=" << source_session_id_);
  }
}

void source::disconnect_session()
{
  stream_part_callback_ = nullptr;
  
  LOG("Delete session " << source_session_id_);
  
  yami::parameters params;
  params.set_long_long("session", source_session_id_);
  
  YC.agent().send(ye_, name_, "delete_streaming_session", params);
  
  
}

void source::handle_stream_part(int server_session, const void* buf, size_t length)
{
  if (stream_part_callback_ != nullptr)
  {
    stream_part_callback_(buf, length);
  }
}

void source::handle_session_deleted(int server_session)
{
  LOG("Source session deleted " << server_session);
}

}
}
