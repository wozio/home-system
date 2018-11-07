#include "ownetwork.h"
#include "owtemp.h"
#include "utils/logger.h"
#include <boost/filesystem.hpp>

using namespace std;
using namespace boost::filesystem;

path sysfs_path("/sys/bus/w1/devices/");

ownet::ownet()
: ioservice_("io.1wire-sysfs"),
  timer_proc_(timer_.get_ios())
{
  timer_.set_from_now(0, [this](){ list(); });
  timer_proc_.set_from_now(0, [this](){ process(); });
}

ownet::~ownet()
{
}

void ownet::list()
{
  static bool list_logged = false;
  if (!list_logged)
  {
    LOG(INFO)  << "Listing One Wire sysfs devices, path: " << sysfs_path;
  }

  if (exists(sysfs_path))
  {
    if (is_directory(sysfs_path))
    {
      for (directory_entry& x : directory_iterator(sysfs_path))
      {
        auto devfile = x.path().filename().string();
        auto dashpos = devfile.find('-');
        if (dashpos != std::string::npos)
        {
          if (!list_logged)
          {
            LOG(DEBUG) << "Found 1 wire device with file name " << devfile;
          }
          auto dts = devfile.substr(0, dashpos);
          auto dids = devfile.substr(dashpos + 1);
          auto dt = std::stoull(dts, 0, 16);
          auto did = std::stoull(dids, 0, 16);

          auto ditr = devices_.find(did);
          if (ditr == devices_.end())
          {
            switch (dt)
            {
            case 0x10:
            {
              std::shared_ptr<temp> d(new temp(did, x.path().string()));
              ioservice_.add_device(d);
              devices_[did] = d;
              break;
            }
            }
          }
        }
      }
    }
  }
  list_logged = true;
  timer_.set_from_now(10000, [this](){ list(); });
}

void ownet::process()
{
  try
  {
    for (auto& device : devices_)
    {
      device.second->process();
    }
  }
  catch (const std::runtime_error& e)
  {
    LOG(ERROR) << "Error while processing: " << e.what();
    ioservice_.set_state_for_all(home_system::io::io_state_t::faulty);
  }
  timer_proc_.set_from_now(1000, [this](){ process(); });
}
