#include "channel.h"

#include <iostream>

using namespace std;

namespace home_system
{
namespace media
{

channel::channel(uint64_t id, const std::string& name, transponder_t t, uint16_t service_id)
: id_(id),
  name_(name),
  transponder_(t),
  service_id_(service_id)
{
}

channel::~channel()
{
}

void channel::tune(int fd)
{
  transponder_->tune(fd);
}

uint64_t channel::get_id()
{
  return id_;
}

std::string channel::get_name()
{
  return name_;
}

std::shared_ptr<transponder> channel::get_transponder()
{
  return transponder_;
}

uint16_t channel::service_id()
{
  return service_id_;
}

void channel::print()
{
  cout << hex << id_ << " " << name_ << " transponder: " << transponder_ << endl;
}

}
}

