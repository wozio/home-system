#include "transponder_dvbt.h"
#include <logger.h>
#include <sys/ioctl.h>
#include "param_convert.h"
#include "transponder_configuration_exception.h"
#include <sstream>
#include <map>

namespace home_system
{
namespace media
{

using namespace std;

transponder_dvbt::transponder_dvbt(const std::vector<string>& fields)
{
  if (fields.size() < 9)
  {
    throw transponder_configuration_exception("It should be at least 9 parameters for DVB-T");
  }
  
  frequency_ = to_frequency(fields[1]);
  bandwidth_ = to_bandwidth(fields[2]);
  fec_hi_ = to_fec(fields[3]);
  fec_lo_ = to_fec(fields[4]);
  mod_ = to_modulation(fields[5]);
  tm_ = to_transmit_mode(fields[6]);
  guard_ = to_guard_interval(fields[7]);
  hi_ = to_hierarchy(fields[8]);
}

transponder_dvbt::~transponder_dvbt()
{
}

void transponder_dvbt::tune(int fd)
{
  dtv_property myproperties[10];
  myproperties[0].cmd = DTV_DELIVERY_SYSTEM;
  myproperties[0].u.data = SYS_DVBT;
  myproperties[1].cmd = DTV_FREQUENCY;
  myproperties[1].u.data = frequency_;
  myproperties[2].cmd = DTV_BANDWIDTH_HZ;
  myproperties[2].u.data = bandwidth_;
  myproperties[3].cmd = DTV_CODE_RATE_HP;
  myproperties[3].u.data = fec_hi_;
  myproperties[4].cmd = DTV_CODE_RATE_LP;
  myproperties[4].u.data = fec_lo_;
  myproperties[5].cmd = DTV_MODULATION;
  myproperties[5].u.data = mod_;
  myproperties[6].cmd = DTV_TRANSMISSION_MODE;
  myproperties[6].u.data = tm_;
  myproperties[7].cmd = DTV_GUARD_INTERVAL;
  myproperties[7].u.data = guard_;
  myproperties[8].cmd = DTV_HIERARCHY;
  myproperties[8].u.data = hi_;
  myproperties[9].cmd = DTV_TUNE;
  
  dtv_properties mydtvproperties = {
      .num = 10, /* The number of commands in the array */
      .props = myproperties /* Pointer to the array */
  };

  if ((ioctl(fd, FE_SET_PROPERTY, &mydtvproperties)) == -1)
  {
    LOG(WARNING) << "FE_SET_PROPERTY failed";
  }
}

void transponder_dvbt::print(std::ostream& str) const
{
  str << "Type: DVB-T, Frequency: " << frequency_;
}

bool transponder_dvbt::isless(const transponder* right)
{
  const transponder_dvbt* comp = dynamic_cast<const transponder_dvbt*>(right);
  if (comp == NULL)
    throw runtime_error("Comparing transponders of different type");
  
  if (frequency_ < comp->frequency_)
  {
    return true;
  }
  return false;
}

void transponder_dvbt::save(std::ostream& str) const
{
  
}

}
}
