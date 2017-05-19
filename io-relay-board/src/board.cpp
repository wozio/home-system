#include "board.h"
#include "utils/logger.h"
#include <boost/date_time/posix_time/posix_time.hpp>
#include <thread>
#include <iostream>

using namespace std;
using namespace boost;
using namespace boost::posix_time;
using namespace boost::asio;
using namespace home_system::io;

board::board(const std::string &name, const std::string &port)
    : port_(port),
      ioservice_(name),
      serial_port_(ios_.io_service()),
      timer_(ios_),
      write_timer_(ios_)
{
  for (size_t i = 0; i < 8; ++i)
  {
    relay_t r(new relay(i));
    ioservice_.add_device(r);
    relays_.push_back(r);
  }

  // all operations on port are done on io_service thread
  // to avoid locking
  ios_.io_service().post([this]() {
    open_port();
  });
}

void board::open_port()
{
  static bool logged = false;
  if (!logged)
  {
    LOG(INFO) << "Opening COM port " << port_;
  }

  try
  {
    serial_port_.open(port_);
    serial_port_.set_option(serial_port::baud_rate(2400));
    LOG(INFO) << "Opened COM port " << port_;
    logged = false;
    setup_read();
  }
  catch (const boost::system::system_error &e)
  {
    if (!logged)
    {
      LOG(WARNING) << "Unable to open COM port " << port_ << ": " << e.what() << ", keep trying...";
      logged = true;
    }
    timer_.set_from_now(1000, [this]() {
      open_port();
    });
  }
}

void board::setup_read()
{
  serial_port_.async_read_some(buffer(buf_, 10),
                               [&](const boost::system::error_code &error, std::size_t bytes_transferred) { read_handler(error, bytes_transferred); });

  timer_.cancel();
  timer_.set_from_now(2000, [this]() {
    LOG(WARNING) << "Timeout on port read";
    close_port();
    open_port();
  });
}

void board::read_handler(const boost::system::error_code &error,
                         std::size_t bytes_transferred)
{
  if (!error)
  {
    static bool crfound = false;
    static vector<char> value; // between cr(0xA) and endl(0xD);
    for (size_t i = 0; i < bytes_transferred; ++i)
    {
      switch (buf_[i])
      {
      case 0xA:
        crfound = true;
        break;
      case 0xD:
        if (crfound)
        {
          crfound = false;
          value.push_back(0);
          int bitmap = atoi(&value[0]);
          bitmap &= 0xFF;
          //LOG("BITMAP: " << bitmap);
          value.clear();
          set_values(bitmap);
          exec_value_change();
        }
      default:
        if ((buf_[i] > 0x1F) && crfound)
          value.push_back(buf_[i]);
      }
    }

    setup_read();
  }
  else if (error != error::operation_aborted)
  {
    LOG(WARNING) << "Error on port read";
    close_port();
    open_port();
  }
}

void board::write_handler(const boost::system::error_code &error,
                          std::size_t bytes_transferred)
{
  if (!error)
  {
    write_timer_.set_from_now(1000, [this]() {
      exec_value_change();
    });
  }
  else if (error != error::operation_aborted)
  {
    LOG(WARNING) << "Error on port write";
    close_port();
    open_port();
  }
}

void board::set_values(int bitmap)
{
  for (size_t i = 0; i < 8; ++i)
  {
    int value = (bitmap >> i) & 1;
    relays_[i]->set_value(value);
  }
}

void board::exec_value_change()
{
  write_timer_.cancel();
  try
  {
    for (size_t j = 0; j < 8; ++j)
    {
      if (relays_[j]->get_state() == io_state_t::ok)
      {
        auto wv = relays_[j]->get_wanted_value();
        auto v = relays_[j]->get_value();
        if (v != wv)
        {
          unsigned char buf;
          if (wv)
            // enable relay number is encoded from 'a' ASCII code
            buf = 97;
          else
            // disable relay number is encoded from 'i' ASCII code
            buf = 105;
          buf += j;
          LOG(DEBUG) << "executing relay " << j << " value change: " << v << "->" << wv;
          serial_port_.async_write_some(buffer(&buf, 1),
                                        [&](const boost::system::error_code &error, std::size_t bytes_transferred) { write_handler(error, bytes_transferred); });
          break;
        }
      }
    }
  }
  catch (const boost::system::system_error &e)
  {
    LOG(WARNING) << "Unable to write to COM port " << port_ << ": " << e.what();
    close_port();
    open_port();
  }
}

void board::close_port()
{
  if (serial_port_.is_open())
  {
    LOG(INFO) << "Closing COM port: " << port_;
    serial_port_.cancel();
    serial_port_.close();
  }
  timer_.cancel();
  ios_.stop_ios();
  for (size_t i = 0; i < 8; ++i)
  {
    relays_[i]->set_state(io_state_t::faulty);
  }
}

board::~board()
{
  if (serial_port_.is_open())
  {
    LOG(INFO) << "Closing COM port: " << port_;
    serial_port_.cancel();
    serial_port_.close();
  }
  timer_.cancel();
}
