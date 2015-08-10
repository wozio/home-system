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
    uchar serial_num[8];
    owSerialNum(portnum_, serial_num, TRUE);
    LOG("Found device: " << serial_num_to_string(serial_num));
    // TODO: add other types of devices when needed
    switch (serial_num[0])
    {
    case 0x10: // DS1920
      {
        temp dev(portnum_, serial_num);
        devices_.push_back(dev);
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
  
  for (size_t i = 0; i < devices_.size(); i++)
  {
    devices_[i].send_convert();
  }
  
  timer_.set_from_now(2000, [this](){ read_temp(); });
}

void net::read_temp()
{
  LOG("Reading temperature");
  
  for (size_t i = 0; i < devices_.size(); i++)
  {
    devices_[i].read_temp();
  }
  timer_.set_from_now(13000, [this](){ send_request(); });
}

void net::get_input_history(int input, std::vector<double>& history)
{
  devices_[0].get_history(history);
}

}
}
}
