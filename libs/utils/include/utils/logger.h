#pragma once

#include <boost/log/trivial.hpp>

#define LOG(level) BOOST_LOG_TRIVIAL(level)

#define TRACE trace
#define DEBUG debug
#define INFO info
#define WARNING warning
#define ERROR error
#define FATAL fatal
