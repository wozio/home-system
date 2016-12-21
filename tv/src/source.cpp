#include "pch.h"
#include "source.h"
#include "yamicontainer.h"
#include "logger.h"

using namespace std;

namespace home_system
{
namespace media
{

std::map<int, source_t> source::_client_session_ids;

source_t source::source_for_session(int client_session)
{
  return _client_session_ids[client_session];
}

source::source(db& db, const std::string& name, const std::string& ye)
: db_(db),
  name_(name),
  ye_(ye),
  source_session_id_(-1)
{
  LOG(DEBUG) << "Creating source " << name_ << " (" << ye_ << ")";
}

source::~source()
{
  LOG(DEBUG) << "Removing source " << name_ << " (" << ye_ << ")";
  delete_source_session();
}

void source::not_available()
{
  // TODO: more that one client session per source
  if (client_session_id_ != -1)
  {
    delete_session(client_session_id_);
  }
}

std::string source::endpoint()
{
  return ye_;
}

int source::create_session(int channel, const std::string& destination, const std::string& endpoint)
{
  // starting session on source
  long long local = db_.get_local_channel(channel, name_);

  source_session_.reset(new client_binary_session());
  
  yami::parameters params;
  
  params.set_long_long("channel", local);
  params.set_string("destination", "tv");
  params.set_string("endpoint", source_session_->get_endpoint());
  
  LOG(DEBUG) << "Create source session " << " channel=" << channel << "(" << hex << local << ") endpoint: " << source_session_->get_endpoint();
  
  unique_ptr<yami::outgoing_message> message(YC.agent().send(ye_, name_, "create_session", params));
  
  message->wait_for_completion(10000);
  
  if (message->get_state() != yami::replied)
  {
    throw runtime_error("Request to create session rejected by source: " + message->get_exception_msg());
  }

  source_session_id_ = message->get_reply().get_integer("id");

  if (source_session_id_ == -1)
  {
    throw runtime_error("Request to create session failed in source: " + message->get_reply().get_string("reason"));
  }
    
  LOG(DEBUG) << "Got source session=" << source_session_id_;
  
  // finding free client session id
  int client_session_id_ = 0;
  while (_client_session_ids.find(client_session_id_) != _client_session_ids.end())
  {
    client_session_id_++;
  }
  _client_session_ids[client_session_id_] = shared_from_this();
  
  LOG(DEBUG) << "Creating client session " << client_session_id_;
  
  client_session_.reset(new session(client_session_id_, endpoint));
  
  return client_session_id_;
}

void source::delete_session(int client_session)
{
  delete_source_session();
  _client_session_ids.erase(client_session);
  client_session_.reset();
}

void source::delete_source_session()
{
  if (source_session_id_ != -1)
  {
    LOG(DEBUG) << "Delete source session " << source_session_id_;

    try
    {
      yami::parameters params;
      params.set_integer("session", source_session_id_);

      YC.agent().send(ye_, name_, "delete_session", params);
    }
    catch(const std::exception& e)
    {
      LOG(WARNING) << "EXCEPTION: " << e.what();
    }
    
    source_session_id_ = -1;
  }
}

void source::stream_part(int source_session, const void* buf, size_t len)
{
  try
  {
    if (client_session_)
    {
      client_session_->stream_part(buf, len);
    }
  }
  catch (const session_error& e)
  {
    LOG(ERROR) << "Session error: " << e.what();
    
    delete_session(client_session_id_);
  }
}

session_t source::get_session(int s)
{
  if (client_session_)
  {
    return client_session_;
  }
  throw runtime_error("Unknown session");
}

}
}
