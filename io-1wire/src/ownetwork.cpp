#include "ownetwork.h"
#include "owtemp.h"
#include "utils.h"
#include "utils/logger.h"

extern "C"
{
#include "ownet.h"
}

using namespace std;

ownet::ownet(const std::string &port)
: port_(port),
  ioservice_("io.1wire"),
  opened_(false),
  open_fault_logged_(false),
  search_fault_logged_(false)
{
  open();
}

ownet::~ownet()
{
  close();
}

void ownet::open()
{
  if (!open_fault_logged_)
  {
    LOG(INFO)  << "Opening One Wire port " << port_;
  }
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

void ownet::search()
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
        std::shared_ptr<temp> d(new temp(portnum_, serial_num));
        ioservice_.add_device(d);
        devices_[serial_num] = d;
      }
      break;
    default:
      break;
    }
  }
  
  if (devices_.size())
  {
    // start processing loop
    process();
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

void ownet::close()
{
  if (portnum_ > -1)
  {
    LOG(INFO) << "Closing One Wire port " << port_;
    owRelease(portnum_);
    open_fault_logged_ = false;
  }
}

void ownet::process()
{
  try
  {
    for (auto& device : devices_)
    {
      device.second->process();
    }

    timer_.set_from_now(1000, [this]()
    {
      process();
    });
  }
  catch (const std::runtime_error& e)
  {
    LOG(ERROR) << "Error while processing: " << e.what();
    ioservice_.set_state_for_all(home_system::io::io_state_t::faulty);
    
    close();
    open();
  }
}
