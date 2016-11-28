#ifndef PARAM_CONVERT_H
#define	PARAM_CONVERT_H

#include "frontend_extended.h"
#include <linux/dvb/frontend.h>
#include <string>

fe_code_rate str_to_fec(const std::string& str);
const std::string& fec_to_str(fe_code_rate fec);

fe_modulation str_to_modulation(const std::string& str);
const std::string& modulation_to_str(fe_modulation mod);

fe_transmit_mode str_to_transmit_mode(const std::string& str);
const std::string& transmit_mode_to_str(fe_transmit_mode tm);

fe_guard_interval str_to_guard_interval(const std::string& str);
const std::string& guard_interval_to_str(fe_guard_interval gi);

fe_hierarchy str_to_hierarchy(const std::string& str);
const std::string& hierarchy_to_str(fe_hierarchy hi);

polarization str_to_polarization(const std::string& str);
const std::string& polarization_to_str(polarization p);

fe_rolloff str_to_rolloff(const std::string& str);
const std::string& rolloff_to_str(fe_rolloff r);

#endif	/* PARAM_CONVERT_H */

