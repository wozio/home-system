#ifndef PARAM_CONVERT_H
#define	PARAM_CONVERT_H

#include "frontend_extended.h"
#include <linux/dvb/frontend.h>
#include <string>

int to_frequency(const std::string& str);

int to_bandwidth(const std::string& str);

fe_code_rate to_fec(const std::string& str);

fe_modulation to_modulation(const std::string& str);

fe_transmit_mode to_transmit_mode(const std::string& str);

fe_guard_interval to_guard_interval(const std::string& str);

fe_hierarchy to_hierarchy(const std::string& str);

polarization to_polarization(const std::string& str);

int to_symbol_rate(const std::string& str);

fe_rolloff to_rolloff(const std::string& str);

#endif	/* PARAM_CONVERT_H */

