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

  rolloff_ = to_rolloff(fields[5]);
  mod_ = to_modulation(fields[6]);
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
  str << "DVB-S2, freq=" << frequency_ << " pol=" << polarization_;
}

void transponder_dvbs2::save(std::ostream& str) const
{
  str << "S2 " << dec << frequency_ << " ";
  str << (polarization_ == POLARIZATION_HORIZONTAL ? 'H' : 'V') << " ";
  str << dec << symbol_rate_ << " ";
  switch (fec_)
  {
    case FEC_NONE:
      str << "NONE";
      break;
    case FEC_1_2:
      str << "1/2";
      break;
    case FEC_2_3:
      str << "2/3";
      break;
    case FEC_3_4:
      str << "3/4";
      break;
    case FEC_4_5:
      str << "4/5";
      break;
    case FEC_5_6:
      str << "5/6";
      break;
    case FEC_6_7:
      str << "6/7";
      break;
    case FEC_7_8:
      str << "7/8";
      break;
    case FEC_8_9:
      str << "8/9";
      break;
    case FEC_9_10:
      str << "9/0";
      break;
    case FEC_AUTO:
      str << "AUTO";
      break;
    case FEC_3_5:
      str << "3/5";
      break;
    case FEC_2_5:
      str << "2/5";
      break;
  }
  str << " ";
  switch (rolloff_)
  {
    case ROLLOFF_35:
      str << "35";
      break;
    case ROLLOFF_20:
      str << "20";
      break;
    case ROLLOFF_25:
      str << "25";
      break;
    case ROLLOFF_AUTO:
      str << "AUTO";
      break;
  }

  switch (mod_)
  {
    case QPSK:
      str << "QPSK";
      break;
    case QAM_16:
      str << "QAM16";
      break;
    case QAM_32:
      str << "QAM32";
      break;
    case QAM_64:
      str << "QAM64";
      break;
    case QAM_128:
      str << "QAM128";
      break;
    case QAM_256:
      str << "QAM256";
      break;
    case QAM_AUTO:
      str << "AUTO";
      break;
    case VSB_8:
      str << "VSB8";
      break;
    case VSB_16:
      str << "VSB16";
      break;
    case PSK_8:
      str << "8PSK";
      break;
    case APSK_16:
      str << "16APSK";
      break;
    case APSK_32:
      str << "32APSK";
      break;
    case DQPSK:
      str << "DQPSK";
      break;
    case QAM_4_NR:
      str << "AUTO"; // i don't know how it is encoded in transponders file
      break;
  }
  
  str << endl;
}

}
}
