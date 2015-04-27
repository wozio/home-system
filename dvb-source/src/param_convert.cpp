#include "param_convert.h"
#include "transponder_configuration_exception.h"
#include <linux/dvb/version.h>
#include <boost/lexical_cast.hpp>
#include <map>

using namespace std;

int to_frequency(const std::string& str)
{
  try
  {
    return boost::lexical_cast<int>(str);
  }
  catch (const boost::bad_lexical_cast& e)
  {
    throw home_system::media::transponder_configuration_exception("Incorrect frequency value: " + str);
  }
}

int to_bandwidth(const std::string& str)
{
  try
  {
    return boost::lexical_cast<float>(str.substr(0, str.find_first_of("MHz"))) * 1000000;
  }
  catch (const boost::bad_lexical_cast& e)
  {
    throw home_system::media::transponder_configuration_exception("Incorrect or unsupported bandwidth value: " + str);
  }
}

fe_code_rate to_fec(const std::string& str)
{
  map<string, fe_code_rate> fec_map;
  fec_map["NONE"] = FEC_NONE;
  fec_map["1/2"]  = FEC_1_2;
  fec_map["2/3"]  = FEC_2_3;
  fec_map["3/4"]  = FEC_3_4;
  fec_map["4/5"]  = FEC_4_5;
  fec_map["5/6"]  = FEC_5_6;
  fec_map["6/7"]  = FEC_6_7;
  fec_map["7/8"]  = FEC_7_8;
  fec_map["8/9"]  = FEC_8_9;
  fec_map["AUTO"] = FEC_AUTO;
  fec_map["3/5"]  = FEC_3_5;
  fec_map["2/5"]  = FEC_2_5;
  fec_map["9/10"] = FEC_9_10;
  auto i = fec_map.find(str);
  if (i != fec_map.end())
  {
    return i->second;
  }
  else
  {
    throw home_system::media::transponder_configuration_exception("Incorrect or unsupported FEC value: " + str);
  }
}

fe_modulation to_modulation(const std::string& str)
{
  map<string, fe_modulation> mod_map;
  mod_map["QPSK"]   = QPSK;
  mod_map["QAM16"]  = QAM_16;
  mod_map["QAM32"]  = QAM_32;
  mod_map["QAM64"]  = QAM_64;
  mod_map["QAM128"] = QAM_128;
  mod_map["QAM256"] = QAM_256;
  mod_map["AUTO"]   = QAM_AUTO;
  mod_map["VSB8"]   = VSB_8;
  mod_map["VSB16"]  = VSB_16;
  mod_map["8PSK"]   = PSK_8;
  mod_map["16APSK"] = APSK_16;
  mod_map["32APSK"] = APSK_32;
  mod_map["DQPSK"]  = DQPSK;
  auto i = mod_map.find(str);
  if (i != mod_map.end())
  {
    return i->second;
  }
  else
  {
    throw home_system::media::transponder_configuration_exception("Incorrect or unsupported modulation value: " + str);
  }
}

fe_transmit_mode to_transmit_mode(const std::string& str)
{
  map<string, fe_transmit_mode> tm_map;
  tm_map["2K"]   = TRANSMISSION_MODE_2K;
  tm_map["8K"]   = TRANSMISSION_MODE_8K;
  tm_map["AUTO"] = TRANSMISSION_MODE_AUTO;
  tm_map["4K"]   = TRANSMISSION_MODE_4K;
#if DVB_API_VERSION_MINOR > 2
  tm_map["1K"]   = TRANSMISSION_MODE_1K;
  tm_map["16K"]  = TRANSMISSION_MODE_16K;
  tm_map["32K"]  = TRANSMISSION_MODE_32K;
#endif
  auto i = tm_map.find(str);
  if (i != tm_map.end())
  {
    return i->second;
  }
  else
  {
    throw home_system::media::transponder_configuration_exception("Incorrect or unsupported transmit mode value: " + str);
  }
}

fe_guard_interval to_guard_interval(const std::string& str)
{
  map<string, fe_guard_interval> guard_map;
  guard_map["1/32"]   = GUARD_INTERVAL_1_32;
  guard_map["1/16"]   = GUARD_INTERVAL_1_16;
  guard_map["1/8"]    = GUARD_INTERVAL_1_8;
  guard_map["1/4"]    = GUARD_INTERVAL_1_4;
  guard_map["AUTO"]   = GUARD_INTERVAL_AUTO;
#if DVB_API_VERSION_MINOR > 2
  guard_map["1/128"]  = GUARD_INTERVAL_1_128;
  guard_map["19/128"] = GUARD_INTERVAL_19_128;
  guard_map["19/256"] = GUARD_INTERVAL_19_256;
#endif
  auto i = guard_map.find(str);
  if (i != guard_map.end())
  {
    return i->second;
  }
  else
  {
    throw home_system::media::transponder_configuration_exception("Incorrect or unsupported guard interval value: " + str);
  }
}

fe_hierarchy to_hierarchy(const std::string& str)
{
  map<string, fe_hierarchy> hi_map;
  hi_map["NONE"] = HIERARCHY_NONE;
  hi_map["1"] = HIERARCHY_1;
  hi_map["2"] = HIERARCHY_2;
  hi_map["4"] = HIERARCHY_4;
  hi_map["AUTO"] = HIERARCHY_AUTO;
  auto i = hi_map.find(str);
  if (i != hi_map.end())
  {
    return i->second;
  }
  else
  {
    throw home_system::media::transponder_configuration_exception("Incorrect or unsupported hierarchy value: " + str);
  }
}

polarization to_polarization(const std::string& str)
{
  map<string, polarization> pol_map;
  pol_map["H"] = POLARIZATION_HORIZONTAL;
  pol_map["V"] = POLARIZATION_VERTICAL;
  pol_map["L"] = POLARIZATION_CIRCULAR_LEFT;
  pol_map["R"] = POLARIZATION_CIRCULAR_RIGHT;
  auto i = pol_map.find(str);
  if (i != pol_map.end())
  {
    return i->second;
  }
  else
  {
    throw home_system::media::transponder_configuration_exception("Incorrect or unsupported polarization value: " + str);
  }
}

int to_symbol_rate(const std::string& str)
{
  try
  {
    return boost::lexical_cast<int>(str);
  }
  catch (const boost::bad_lexical_cast& e)
  {
    throw home_system::media::transponder_configuration_exception("Incorrect symbol rate value: " + str);
  }
}

fe_rolloff to_rolloff(const std::string& str)
{
  map<string, fe_rolloff> roll_map;
  roll_map["35"] = ROLLOFF_35;
  roll_map["20"] = ROLLOFF_20;
  roll_map["25"] = ROLLOFF_25;
  roll_map["AUTO"] = ROLLOFF_AUTO;
  auto i = roll_map.find(str);
  if (i != roll_map.end())
  {
    return i->second;
  }
  else
  {
    throw home_system::media::transponder_configuration_exception("Incorrect or unsupported rolloff value: " + str);
  }
}