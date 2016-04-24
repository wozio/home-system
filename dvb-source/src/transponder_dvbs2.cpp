#include "transponder_dvbs2.h"
#include "param_convert.h"
#include "transponder_configuration_exception.h"
#include <sys/ioctl.h>
#include <logger.h>
#include <sstream>

namespace home_system
{
namespace media
{

using namespace std;

transponder_dvbs2::transponder_dvbs2(const std::vector<string>& fields)
: transponder_dvbs(fields)
{
  if (fields.size() < 7)
  {
    throw transponder_configuration_exception("It should be at least 7 parameters for DVB-S2");
  }

  rolloff_ = str_to_rolloff(fields[5]);
  mod_ = str_to_modulation(fields[6]);
}

transponder_dvbs2::~transponder_dvbs2()
{
}

void transponder_dvbs2::tune(int fd)
{
  int ifreq, hiband;
  clear_prepare(fd, hiband, ifreq);
  diseqc(fd, hiband);

  dtv_property myproperties[8];

  myproperties[0].cmd = DTV_DELIVERY_SYSTEM;
  myproperties[0].u.data = SYS_DVBS2;
  myproperties[1].cmd = DTV_FREQUENCY;
  myproperties[1].u.data = ifreq;
  myproperties[2].cmd = DTV_SYMBOL_RATE;
  myproperties[2].u.data = symbol_rate_;
  myproperties[3].cmd = DTV_INNER_FEC;
  myproperties[3].u.data = fec_;
  myproperties[4].cmd = DTV_INVERSION;
  myproperties[4].u.data = INVERSION_AUTO;

  myproperties[5].cmd = DTV_ROLLOFF;
  myproperties[5].u.data = rolloff_;
  myproperties[6].cmd = DTV_MODULATION;
  myproperties[6].u.data = mod_;

  myproperties[7].cmd = DTV_TUNE;

  dtv_properties mydtvproperties = {
    .num = 8,
    .props = myproperties
  };

  if ((ioctl(fd, FE_SET_PROPERTY, &mydtvproperties)) == -1)
  {
    LOG(WARNING) << "FE_SET_PROPERTY failed";
  }
}

void transponder_dvbs2::print(std::ostream& str) const
{
  str << "S2";
  print_dvbs_params(str);
  str
    << " " << rolloff_to_str(rolloff_)
    << " " << modulation_to_str(mod_)
    ;
}

void transponder_dvbs2::save(std::ostream& str) const
{
  print(str);
}

}
}
