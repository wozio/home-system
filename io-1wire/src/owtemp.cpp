
#include "utils/logger.h"
#include "owtemp.h"
#include "utils.h"

extern "C"
{
#include "ownet.h"
}

using namespace std;

temp::temp(int port_num, uint64_t serial_num)
: owdevice(port_num, serial_num, home_system::io::io_data_type_t::double_float, "temperature_input"),
  process_cnt_(12)
{
  LOG(DEBUG) << "Created temperature device (DS1920): " << serial_num_to_string(serial_num_);
}

temp::~temp()
{
  LOG(DEBUG) << "Destroyed temperature device (DS1920): " << serial_num_to_string(serial_num_);
}

void temp::process()
{
  process_cnt_++;
  // every 15 s send convert
  if (process_cnt_ == 13)
  {
    send_convert();
  }
  else if (process_cnt_ == 15)
  {
    auto ret = read_temp();
    // if convertion is not ready yet, wait another second
    if (ret)
      process_cnt_ = 0;
  }
  else if (process_cnt_ == 16)
  {
    read_temp();
    process_cnt_ = 1; // since already waited 1 s more than 15 s
  }
}

void temp::send_convert()
{
  // set the device serial number to the counter device
  owSerialNum(port_num_, (uchar*)&serial_num_, FALSE);
  
  // access the device
  if (owAccess(port_num_))
  {
    // send the convert command
    if (!owWriteByte(port_num_, 0x44))
    {
      throw std::runtime_error("Problem writing convert command");
    }
  }
  else
  {
    throw std::runtime_error("Problem accessing device");
  }
}

bool temp::read_temp()
{
  owSerialNum(port_num_, (uchar*)&serial_num_, FALSE);
  
  // access the device
  if (owAccess(port_num_))
  {
    // create a block to send that reads the temperature
    // read scratchpad command
    uchar send_block[30];
    size_t send_cnt = 0;
    send_block[send_cnt++] = 0xBE;

    // now add the read bytes for data bytes and crc8
    for (size_t i = 0; i < 9; i++)
      send_block[send_cnt++] = 0xFF;

    // now send the block
    if (owBlock(port_num_, FALSE, send_block, send_cnt))
    {
      // initialize the CRC8 
      setcrc8(port_num_, 0);

      // perform the CRC8 on the last 8 bytes of packet
      uchar lastcrc8;
      for (size_t i = send_cnt - 9; i < send_cnt; i++)
        lastcrc8 = docrc8(port_num_, send_block[i]);

      // verify CRC8 is correct
      if (lastcrc8 == 0x00)
      {
        float tmp,cr,cpc;
        // calculate the high-res temperature
        int tsht;
        tsht = send_block[1]/2;
        if (send_block[2] & 0x01)
          tsht |= -128;
        tmp = (float)(tsht);
        cr = send_block[7];
        cpc = send_block[8];
        if ((cpc - cr) == 1)
        {
          LOG(WARNING) << "Conversion not ready yet";
          return false;
        }
        if (cpc == 0)
        {
          throw std::runtime_error("CPC == 0");
        }
        else
          tmp = tmp - (float)0.25 + (cpc - cr)/cpc;
        
        LOG(DEBUG) << serial_num_to_string(serial_num_) << ": " << tmp;
        boost::any v(tmp);
        set_value(v);
        return true;
      }
      else
      {
        throw std::runtime_error("Wrong CRC");
      }
    }
    throw std::runtime_error("Sending block failed");
  }
  throw std::runtime_error("Accessing device failed");
}
