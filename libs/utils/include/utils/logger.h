#pragma once

#include <boost/log/trivial.hpp>
#include <boost/log/attributes.hpp>

#define LOG(level) \
  LOG_LOCATION;    \
  BOOST_LOG_TRIVIAL(level)

#define LOG_LOCATION \
  boost::log::attribute_cast<boost::log::attributes::mutable_constant<unsigned int>>(boost::log::core::get()->get_global_attributes()["Line"]).set(__LINE__); \
  boost::log::attribute_cast<boost::log::attributes::mutable_constant<std::string>>(boost::log::core::get()->get_global_attributes()["File"]).set(__FILENAME__);

#define TRACE trace
#define DEBUG debug
#define INFO info
#define WARNING warning
#define ERROR error
#define FATAL fatal
