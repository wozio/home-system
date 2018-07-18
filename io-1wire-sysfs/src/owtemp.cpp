#include "owtemp.h"
#include "utils/logger.h"

using namespace std;

temp::temp(uint64_t serial_num, boost::filesystem::path dev_path)
: owdevice(serial_num, dev_path),
  home_system::io::device_float(serial_num, "temperature_input"),
  process_cnt_(14)
{
  LOG(DEBUG) << "Created temperature device (DS1920): " << serial_num_;
}

temp::~temp()
{
  LOG(DEBUG) << "Destroyed temperature device (DS1920): " << serial_num_;
}

void temp::process()
{
  process_cnt_++;
  // read temperature every 15 s
  if (process_cnt_ == 15)
  {
    read_temp();

    process_cnt_ = 0;
  }
}

void temp::read_temp()
{
  std::string line;
  boost::filesystem::path fp(dev_path_);
  fp /= boost::filesystem::path("/w1_slave");
  std::ifstream myfile(fp.string());
  if (myfile.is_open())
  {
    while (std::getline(myfile,line))
    {
      auto sppos = line.find_last_of(" ");
      if (sppos == std::string::npos)
      {
        LOG(ERROR) << "Unable to find last space in line";
        return;
      }
      if (line.substr(sppos + 1) == "YES")
      {
        break;
      }
      else
      {
        LOG(ERROR) << "YES not found in file";
        return;
      }
    }
    while (std::getline(myfile,line))
    {
      auto tpos = line.find("t=");
      if (tpos == std::string::npos)
      {
        LOG(ERROR) << "t= not found in file";
        return;
      }
      double t;
      try
      {
        t = std::stod(line.substr(tpos + 2)) / 1000.0;
      }
      catch(const std::exception& e)
      {
        LOG(ERROR) << "Unable to convert from string to double: " << line.substr(tpos + 2) << "': " << e.what();
        return;
      }
      LOG(DEBUG) << "Temperature measured by device: " << serial_num_ << " is " << t;
      set_value(t);
    }
    myfile.close();
  }
  else
  {
    LOG(ERROR) << "Unable to open file: " << fp.string();
  }
}
