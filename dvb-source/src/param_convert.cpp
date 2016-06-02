#include "param_convert.h"
#include "transponder_configuration_exception.h"
#include <linux/dvb/version.h>
#include <boost/lexical_cast.hpp>
#include <map>

using namespace std;

fe_code_rate str_to_fec(const std::string& str)
{
  static map<string, fe_code_rate> map
  {
    {"NONE", FEC_NONE},
    {"1/2",  FEC_1_2},
    {"2/3",  FEC_2_3},
    {"3/4",  FEC_3_4},
    {"4/5",  FEC_4_5},
    {"5/6",  FEC_5_6},
    {"6/7",  FEC_6_7},
    {"7/8",  FEC_7_8},
    {"8/9",  FEC_8_9},
    {"AUTO", FEC_AUTO},
    {"3/5",  FEC_3_5},
    {"2/5",  FEC_2_5},
    {"9/10", FEC_9_10},
  };
  return map.at(str);
}

const std::string& fec_to_str(fe_code_rate fec)
{
  static map<fe_code_rate, string> map
  {
    {FEC_NONE, "NONE"},
    {FEC_1_2,  "1/2"},
    {FEC_2_3,  "2/3"},
    {FEC_3_4,  "3/4"},
    {FEC_4_5,  "4/5"},
    {FEC_5_6,  "5/6"},
    {FEC_6_7,  "6/7"},
    {FEC_7_8,  "7/8"},
    {FEC_8_9,  "8/9"},
    {FEC_AUTO, "AUTO"},
    {FEC_3_5,  "3/5"},
    {FEC_2_5,  "2/5"},
    {FEC_9_10, "9/10"},
  };
  return map.at(fec);
}

fe_modulation str_to_modulation(const std::string& str)
{
  static map<string, fe_modulation> map
  {
    {"QPSK", QPSK},
    {"QAM16", QAM_16},
    {"QAM32", QAM_32},
    {"QAM64", QAM_64},
    {"QAM128", QAM_128},
    {"QAM256", QAM_256},
    {"AUTO", QAM_AUTO},
    {"VSB8", VSB_8},
    {"VSB16", VSB_16},
    {"8PSK", PSK_8},
    {"16APSK", APSK_16},
    {"32APSK", APSK_32},
    {"DQPSK", DQPSK},
  };
  return map.at(str);
}

const std::string& modulation_to_str(fe_modulation mod)
{
  static map<fe_modulation, string> map
  {
    {QPSK, "QPSK"},
    {QAM_16, "QAM16"},
    {QAM_32, "QAM32"},
    {QAM_64, "QAM64"},
    {QAM_128, "QAM128"},
    {QAM_256, "QAM256"},
    {QAM_AUTO, "AUTO"},
    {VSB_8, "VSB8"},
    {VSB_16, "VSB16"},
    {PSK_8, "8PSK"},
    {APSK_16, "16APSK"},
    {APSK_32, "32APSK"},
    {DQPSK, "DQPSK"},
    {QAM_4_NR, "AUTO"},
  };
  return map.at(mod);
}

fe_transmit_mode str_to_transmit_mode(const std::string& str)
{
  static map<string, fe_transmit_mode> map
  {
    {"2K", TRANSMISSION_MODE_2K},
    {"8K", TRANSMISSION_MODE_8K},
    {"AUTO", TRANSMISSION_MODE_AUTO},
    {"4K", TRANSMISSION_MODE_4K},
  #if DVB_API_VERSION_MINOR > 2
    {"1K", TRANSMISSION_MODE_1K},
    {"16K", TRANSMISSION_MODE_16K},
    {"32K", TRANSMISSION_MODE_32K},
  #endif
  };
  return map.at(str);
}

const std::string& transmit_mode_to_str(fe_transmit_mode tm)
{
  static map<fe_transmit_mode, string> map
  {
    {TRANSMISSION_MODE_2K,   "2K"},
    {TRANSMISSION_MODE_8K,   "8K"},
    {TRANSMISSION_MODE_AUTO, "AUTO"},
    {TRANSMISSION_MODE_4K,   "4K"},
  #if DVB_API_VERSION_MINOR > 2
    {TRANSMISSION_MODE_1K,   "1K"},
    {TRANSMISSION_MODE_16K,  "16K"},
    {TRANSMISSION_MODE_32K,  "32K"},
  #endif
  };
  return map.at(tm);
}

fe_guard_interval str_to_guard_interval(const std::string& str)
{
  static map<string, fe_guard_interval> map
  {
    {"1/32",   GUARD_INTERVAL_1_32},
    {"1/16",   GUARD_INTERVAL_1_16},
    {"1/8",    GUARD_INTERVAL_1_8},
    {"1/4",    GUARD_INTERVAL_1_4},
    {"AUTO",   GUARD_INTERVAL_AUTO},
#if DVB_API_VERSION_MINOR > 2
    {"1/128",  GUARD_INTERVAL_1_128},
    {"19/128", GUARD_INTERVAL_19_128},
    {"19/256", GUARD_INTERVAL_19_256},
#endif
  };
  return map.at(str);
}

const std::string& guard_interval_to_str(fe_guard_interval gi)
{
  static map<fe_guard_interval, string> map
  {
    {GUARD_INTERVAL_1_32,   "1/32"},
    {GUARD_INTERVAL_1_16,   "1/16"},
    {GUARD_INTERVAL_1_8,    "1/8"},
    {GUARD_INTERVAL_1_4,    "1/4"},
    {GUARD_INTERVAL_AUTO,   "AUTO"},
#if DVB_API_VERSION_MINOR > 2
    {GUARD_INTERVAL_1_128,  "1/128"},
    {GUARD_INTERVAL_19_128, "19/128"},
    {GUARD_INTERVAL_19_256, "19/256"},
#endif
  };
  return map.at(gi);
}

fe_hierarchy str_to_hierarchy(const std::string& str)
{
  static map<string, fe_hierarchy> map
  {
    {"NONE", HIERARCHY_NONE},
    {"1", HIERARCHY_1},
    {"2", HIERARCHY_2},
    {"4", HIERARCHY_4},
    {"AUTO", HIERARCHY_AUTO},
  };
  return map.at(str);
}

const std::string& hierarchy_to_str(fe_hierarchy hi)
{
  static map<fe_hierarchy, string> map
  {
    {HIERARCHY_NONE, "NONE"},
    {HIERARCHY_1,    "1"},
    {HIERARCHY_2,    "2"},
    {HIERARCHY_4,    "4"},
    {HIERARCHY_AUTO, "AUTO"},
  };
  return map.at(hi);
}

polarization str_to_polarization(const std::string& str)
{
  static map<string, polarization> map
  {
    {"H", POLARIZATION_HORIZONTAL},
    {"V", POLARIZATION_VERTICAL},
    {"L", POLARIZATION_CIRCULAR_LEFT},
    {"R", POLARIZATION_CIRCULAR_RIGHT},
  };
  return map.at(str);
}

const std::string& polarization_to_str(polarization p)
{
  static map<polarization, string> map
  {
    {POLARIZATION_HORIZONTAL,     "H"},
    {POLARIZATION_VERTICAL,       "V"},
    {POLARIZATION_CIRCULAR_LEFT,  "L"},
    {POLARIZATION_CIRCULAR_RIGHT, "R"},
  };
  return map.at(p);
}

fe_rolloff str_to_rolloff(const std::string& str)
{
  static map<string, fe_rolloff> map
  {
    {"35", ROLLOFF_35},
    {"20", ROLLOFF_20},
    {"25", ROLLOFF_25},
    {"AUTO", ROLLOFF_AUTO},
  };
  return map.at(str);
}

const std::string& rolloff_to_str(fe_rolloff r)
{
  static map<fe_rolloff, string> map
  {
    {ROLLOFF_35, "35"},
    {ROLLOFF_20, "20"},
    {ROLLOFF_25, "25"},
    {ROLLOFF_AUTO, "AUTO"},
  };
  return map.at(r);
}