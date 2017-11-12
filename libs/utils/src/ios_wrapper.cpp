#include "ios_wrapper.h"

using namespace std;
using namespace boost::asio;

namespace home_system
{
namespace utils
{

ios_wrapper::ios_wrapper()
: ios_wrapper(1)
{
}

ios_wrapper::ios_wrapper(size_t num_of_threads)
{
    work_.reset(new io_service::work(io_service_));
    for (; num_of_threads--; )
    {
        io_threads_.push_back(thread([this]()
        {
            io_service_.run();
        }));
    }
}

ios_wrapper::~ios_wrapper()
{
  stop_ios();
}

void ios_wrapper::stop_ios()
{
    work_.reset();
    io_service_.stop();
    for (auto& i : io_threads_)
    {
        if (i.joinable())
        {
            i.join();
        }
    }
    io_threads_.clear();
}

boost::asio::io_service &ios_wrapper::io_service()
{
    return io_service_;
}

}
}
