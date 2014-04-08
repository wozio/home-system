#ifndef DRAWCHART_H_INCLUDED
#define DRAWCHART_H_INCLUDED

#include <boost/filesystem.hpp>
#include <boost/date_time/posix_time/posix_time.hpp>

boost::filesystem::path DrawTempChart(const std::string& server_address, const boost::posix_time::ptime& time_begin,
                                      const boost::posix_time::ptime& time_end, const std::vector<std::string>& sensors);

#endif // DRAWCHART_H_INCLUDED
