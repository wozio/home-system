#include "ownetwork.h"
#include "utils.h"
#include "service.h"
#include "logger.h"

extern "C"
{
#include "ownet.h"
}

using namespace std;

namespace home_system
{
namespace input_output
{
namespace ow
{

net::net(const std::string &port)
: port_(port),
  opened_(false),
  open_fault_logged_(false),
  search_fault_logged_(false)
{
  open();
}

net::~net()
{
}

void net::get_inputs(std::vector<long long> ids)
{
  for (auto device : devices_)
  {
    ids.push_back(device.first);
  }
}

void net::get_input_value(uint64_t id, double& value)
{
  auto device = devices_.find(id);
  if (device != devices_.end())
  {
    device->second.get_value(value);
  }
  else
  {
    throw std::runtime_error("Device id not found: " + serial_num_to_string(id));
  }
}

void net::open()
{
  if (!open_fault_logged_)
    LOGINFO("Opening One Wire port " << port_);
  // attempt to acquire the 1-Wire Net
  if ((portnum_ = owAcquireEx(port_.c_str())) < 0)
  {
    if (!open_fault_logged_)
    {
      LOGWARN("Unable to open One Wire port " << port_ << ", keep trying...");
      open_fault_logged_ = true;
    }
    //OWERROR_DUMP(stdout);
    timer_.set_from_now(1000, [this](){ open(); });
  }
  else
  {
    opened_ = true;
    search();
  }
}

void net::search()
{
  if (!search_fault_logged_)
    LOG("Searching for devices");
  
  devices_.clear();
  
  // Find the device(s) on network
  while (true)
  {
    // perform the search
    if (!owNext(portnum_, TRUE, FALSE))
      break;
    uint64_t serial_num;
    owSerialNum(portnum_, (uchar*)&serial_num, TRUE);
    LOG("Found device: " << serial_num_to_string(serial_num));
    // TODO: add other types of devices when needed
    switch (serial_num & 0xFF) // family type is on first octet
    {
    case 0x10: // DS1920
      {
        temp dev(portnum_, serial_num);
        devices_.emplace(serial_num, dev);
      }
      break;
    default:
      break;
    }
  }
  
  if (devices_.size())
  {
    // start reading from devices
    send_request();
  }
  else
  {
    // try again later
    if (!search_fault_logged_)
    {
      LOGWARN("No devices found, keep trying...");
      search_fault_logged_ = true;
    }
    timer_.set_from_now(1000, [this](){ search(); });
  }
}

void net::close()
{
  if (portnum_ > -1)
  {
    owRelease(portnum_);
  }
}

void net::send_request()
{
  LOG("Sending requests");
  
  for (auto device : devices_)
  {
    device.second.send_convert();
  }
  
  timer_.set_from_now(2000, [this](){ read_temp(); });
}

void net::read_temp()
{
  LOG("Reading temperature");
  
  for (auto device : devices_)
  {
    device.second.read_temp();
  }
  
  timer_.set_from_now(13000, [this](){ send_request(); });
}

}
}
}
