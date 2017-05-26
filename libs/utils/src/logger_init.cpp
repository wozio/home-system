#include "logger_init.h"

#include <boost/log/utility/setup/file.hpp>
#include <boost/log/utility/setup/common_attributes.hpp>
#include <boost/filesystem.hpp>
#include <string>
#include <boost/smart_ptr/shared_ptr.hpp>
#include <boost/core/null_deleter.hpp>
#include <boost/lambda/lambda.hpp>
#include <boost/date_time/posix_time/posix_time.hpp>

#include <boost/log/common.hpp>
#include <boost/log/expressions.hpp>
#include <boost/log/sinks.hpp>
#include <boost/log/sources/logger.hpp>
#include <boost/log/support/date_time.hpp>

namespace logging = boost::log;
namespace attrs = boost::log::attributes;
namespace sinks = boost::log::sinks;
namespace expr = boost::log::expressions;
namespace keywords = boost::log::keywords;

namespace home_system
{
namespace utils
{

void init_log(const std::string& file, bool console_log)
{
  logging::add_common_attributes();

  logging::core::get()->add_global_attribute("Line", attrs::mutable_constant<unsigned int>(0));
  logging::core::get()->add_global_attribute("File", attrs::mutable_constant<std::string>(""));

  auto e = expr::stream
            << expr::format_date_time< boost::posix_time::ptime >("TimeStamp", "%Y-%m-%d %H:%M:%S.%f%Q")
            << " <" << logging::trivial::severity << "> ["
            << expr::attr<std::string>("File") << ":" << expr::attr<unsigned int>("Line") << "]: "
            << expr::smessage;

  if (console_log)
  {
    boost::shared_ptr< sinks::text_ostream_backend > backend =
        boost::make_shared< sinks::text_ostream_backend >();
    backend->add_stream(
        boost::shared_ptr< std::ostream >(&std::cout, boost::null_deleter()));

    backend->auto_flush(true);

    typedef sinks::synchronous_sink< sinks::text_ostream_backend > sink_t;
    boost::shared_ptr< sink_t > sink(new sink_t(backend));

    sink->set_formatter(e);

    logging::core::get()->add_sink(sink);
  }
  else
  {
    try
    {
#ifndef DEBUG
      boost::filesystem::path p("/var/log/home-system/");
      boost::filesystem::create_directories(p);
#else
      boost::filesystem::path p("");
#endif
      boost::shared_ptr< sinks::text_file_backend > backend =
        boost::make_shared< sinks::text_file_backend >(
          keywords::file_name = file + "_%5N.log",
          keywords::rotation_size = 1 * 1024 * 1024
        );

      backend->set_open_mode(std::ios_base::app | std::ios_base::out);

      //backend->auto_flush(true);

      typedef sinks::synchronous_sink< sinks::text_file_backend > sink_t;
      boost::shared_ptr< sink_t > sink(new sink_t(backend));

      sink->set_formatter(e);

      logging::core::get()->add_sink(sink);
      
    }
    catch (...)
    {
    }
  }
}

}
}
