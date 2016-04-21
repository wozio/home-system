#include "transponder_dvbs.h"
#include "param_convert.h"
#include "transponder_configuration_exception.h"
#include <sys/ioctl.h>
#include <logger.h>
#include <sstream>
#include <thread>

namespace home_system
{
namespace media
{

using namespace std;

transponder_dvbs::transponder_dvbs(const std::vector<string>& fields)
: lnb_type_(lnbs_[0]) // TODO currently only LNB type UNIVERSAL is supported
{
  lnb_type_.high_val *= 1000; // convert to kilohertz
  lnb_type_.low_val *= 1000; // convert to kilohertz
  lnb_type_.switch_val *= 1000; // convert to kilohertz

  if (fields.size() < 5)
  {
    throw transponder_configuration_exception("It should be at least 5 parameters for DVB-S");
  }

  frequency_ = stoi(fields[1]);
  polarization_ = str_to_polarization(fields[2]);
  symbol_rate_ = stoi(fields[3]);
  fec_ = str_to_fec(fields[4]);
}

transponder_dvbs::~transponder_dvbs()
{
}

int set_property(int fd, int cmd, int data)
{
  dtv_property prop;
  prop.cmd = cmd;
  prop.u.data = data;
  dtv_properties props;
  props.num = 1;
  props.props = &prop;

  if (ioctl(fd, FE_SET_PROPERTY, &props) == -1)
  {
    LOG(WARNING) << "FE_SET_PROPERTY failed";
    return -1;
  }
  return 0;
}

void transponder_dvbs::clear_prepare(int fd, int& hiband, int& ifreq)
{
  set_property(fd, DTV_CLEAR, DTV_UNDEFINED);

  hiband = 0;

  if (lnb_type_.switch_val && lnb_type_.high_val &&
    frequency_ >= lnb_type_.switch_val)
  {
    hiband = 1;
  }

  if (hiband)
  {
    ifreq = frequency_ - lnb_type_.high_val;
  }
  else
  {
    if (frequency_ < lnb_type_.low_val)
    {
      ifreq = lnb_type_.low_val - frequency_;
    }
    else
    {
      ifreq = frequency_ - lnb_type_.low_val;
    }
  }
  LOG(DEBUG) << "Use high band: " << hiband << ", intermediate frequency: " << ifreq;
}

void transponder_dvbs::tune(int fd)
{
  int ifreq, hiband;
  clear_prepare(fd, hiband, ifreq);
  diseqc(fd, hiband);

  dvb_frontend_event ev;
  while (1)
  {
    if (ioctl(fd, FE_GET_EVENT, &ev) == -1)
      break;
  }

  set_property(fd, DTV_DELIVERY_SYSTEM, SYS_DVBS);
  set_property(fd, DTV_FREQUENCY, ifreq);
  set_property(fd, DTV_SYMBOL_RATE, symbol_rate_);
  set_property(fd, DTV_INNER_FEC, fec_);
  set_property(fd, DTV_INVERSION, INVERSION_AUTO);
  set_property(fd, DTV_ROLLOFF, ROLLOFF_35);
  set_property(fd, DTV_PILOT, PILOT_AUTO);
  set_property(fd, DTV_TUNE, DTV_UNDEFINED);
}

void diseqc_send_msg(int fd, fe_sec_voltage_t v, dvb_diseqc_master_cmd& cmd,
  fe_sec_tone_mode_t t)
{
  if (ioctl(fd, FE_SET_TONE, SEC_TONE_OFF) == -1)
  {
    LOG(WARNING) << "FE_SET_TONE failed";
  }
  if (ioctl(fd, FE_SET_VOLTAGE, v) == -1)
  {
    LOG(WARNING) << "FE_SET_VOLTAGE failed";
  }
  this_thread::sleep_for(chrono::milliseconds(5));

  if (ioctl(fd, FE_DISEQC_SEND_MASTER_CMD, &cmd) == -1)
  {
    LOG(WARNING) << "FE_DISEQC_SEND_MASTER_CMD failed";
  }
  this_thread::sleep_for(chrono::milliseconds(15));

  if (ioctl(fd, FE_DISEQC_SEND_BURST, SEC_MINI_A) == -1)
  {
    LOG(WARNING) << "FE_DISEQC_SEND_BURST failed";
  }
  this_thread::sleep_for(chrono::milliseconds(15));

  if (ioctl(fd, FE_SET_TONE, t) == -1)
  {
    LOG(WARNING) << "FE_SET_TONE failed";
  }
}

void transponder_dvbs::diseqc(int fd, int hiband)
{
  dvb_diseqc_master_cmd cmd = {
    {0xe0, 0x10, 0x38, 0xf0, 0x00, 0x00}, 4};

  /**
   * param: high nibble: reset bits, low nibble set bits,
   * bits are: option, position, polarizaion, band
   */
  cmd.msg[3] = 0xf0 | ((hiband ? 1 : 0) | (polarization_ == POLARIZATION_VERTICAL ? 0 : 2));

  diseqc_send_msg(fd, polarization_ == POLARIZATION_VERTICAL ? SEC_VOLTAGE_13 : SEC_VOLTAGE_18,
    cmd, hiband ? SEC_TONE_ON : SEC_TONE_OFF);
}

void transponder_dvbs::print(std::ostream& str) const
{
  str << "DVB-S, freq=" << frequency_ << " pol=" << polarization_;
}

transponder_dvbs::lnb_type_st transponder_dvbs::lnbs_[] ={
  {"UNIVERSAL", 9750, 10600, 11700},
  {"DBS", 11250, 0, 0},
  {"STANDARD", 10000, 0, 0},
  {"ENHANCED", 9750, 0, 0},
  {"C-BAND", 5150, 0, 0}
};

bool transponder_dvbs::isless(const transponder* right)
{
  const transponder_dvbs* comp = dynamic_cast<const transponder_dvbs*> (right);
  if (comp == NULL)
    throw runtime_error("Comparing transponders of different type");

  if (
    frequency_ < comp->frequency_ &&
    polarization_ < comp->polarization_)
  {
    return true;
  }
  return false;
}

void transponder_dvbs::save(std::ostream& str) const
{
  str << "S " << dec << frequency_ << " ";
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
  str << endl;
}

}
}
