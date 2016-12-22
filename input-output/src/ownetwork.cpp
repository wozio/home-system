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

net::net(const std::string &port, std::function<void(uint64_t)> state_change_callback)
: port_(port),
  state_change_callback_(state_change_callback),
  opened_(false),
  open_fault_logged_(false),
  search_fault_logged_(false)
{
  open();
}

net::~net()
{
}

void net::get_inputs(std::vector<long long>& ids)
{
  for (auto const & device : devices_)
  {
    ids.push_back(device.first);
  }
}

temp& net::get_input(uint64_t id)
{
  return devices_.at(id);
}

void net::open()
{
  if (!open_fault_logged_)
    LOG(INFO)  << "Opening One Wire port " << port_;
  // attempt to acquire the 1-Wire Net
  if ((portnum_ = owAcquireEx(port_.c_str())) < 0)
  {
    if (!open_fault_logged_)
    {
      LOG(WARNING) << "Unable to open One Wire port " << port_ << ", keep trying...";
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
    LOG(DEBUG) << "Searching for devices";
  
  devices_.clear();
  
  // Find the device(s) on network
  while (true)
  {
    // perform the search
    if (!owNext(portnum_, TRUE, FALSE))
      break;
    uint64_t serial_num;
    owSerialNum(portnum_, (uchar*)&serial_num, TRUE);
    LOG(DEBUG) << "Found device: " << serial_num_to_string(serial_num);
    // TODO: add other types of devices when needed
    switch (serial_num & 0xFF) // family type is on first octet
    {
    case 0x10: // DS1920
      {
        devices_.emplace(serial_num, temp(portnum_, serial_num, state_change_callback_));
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
      LOG(WARNING) << "No devices found, keep trying...";
      search_fault_logged_ = true;
    }
    timer_.set_from_now(1000, [this](){ search(); });
  }
}

void net::close()
{
  if (portnum_ > -1)
  {
    LOG(INFO) << "Closing One Wire port " << port_;
    owRelease(portnum_);
    open_fault_logged_ = false;
  }
}

void net::send_request()
{
  LOG(DEBUG) << "Sending requests";
  
  try
  {
    for (auto& device : devices_)
    {
      device.second.send_convert();
    }

    timer_.set_from_now(2000, [this](){ read_temp(); });
  }
  catch (const std::runtime_error& e)
  {
    LOG(ERROR) << "Error while sending requests: " << e.what();
    close();
    open();
  }
}

void net::read_temp()
{
  LOG(DEBUG) << "Reading temperature";
  
  try
  {
    for (auto& device : devices_)
    {
      device.second.read_temp();
    }
    
    timer_.set_from_now(13000, [this](){ send_request(); });
  }
  catch (const std::runtime_error& e)
  {
    LOG(ERROR) << "Error while reading temperature: " << e.what();
    close();
    open();
  }
}

}
}
}
