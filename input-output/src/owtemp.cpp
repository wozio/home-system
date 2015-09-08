
#include "logger.h"
#include "owtemp.h"
#include "utils.h"

extern "C"
{
#include "ownet.h"
}

using namespace std;

namespace home_system
{
namespace input_output
{
namespace ow
{

temp::temp(int portnum, uint64_t serial_num,
  std::function<void(uint64_t)> state_change_callback)
: portnum_(portnum),
  time_(0),
  serial_num_(serial_num),
  state_change_callback_(state_change_callback)
{
  LOG("Created temperature device (DS1920): " << serial_num_to_string(serial_num_));
}

uint64_t temp::id()
{
  return serial_num_;
}

float temp::get_value()
{
  return value_;
}

long long temp::get_time()
{
  return time_;
}

void temp::send_convert()
{
  // set the device serial number to the counter device
  owSerialNum(portnum_, (uchar*)&serial_num_, FALSE);
  
  // access the device
  if (owAccess(portnum_))
  {
    // send the convert command
    if (!owWriteByte(portnum_, 0x44))
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
  owSerialNum(portnum_, (uchar*)&serial_num_, FALSE);
  
  // access the device
  if (owAccess(portnum_))
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
    if (owBlock(portnum_, FALSE, send_block, send_cnt))
    {
      // initialize the CRC8 
      setcrc8(portnum_, 0);

      // perform the CRC8 on the last 8 bytes of packet
      uchar lastcrc8;
      for (size_t i = send_cnt - 9; i < send_cnt; i++)
        lastcrc8 = docrc8(portnum_, send_block[i]);

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
          LOGWARN("Conversion not ready yet");
          return false;
        }
        if (cpc == 0)
        {
          throw std::runtime_error("CPC == 0");
        }
        else
          tmp = tmp - (float)0.25 + (cpc - cr)/cpc;
        
        LOG(serial_num_to_string(serial_num_) << ": " << tmp);
        float old_value = value_;
        value_ = tmp;
        if (tmp != old_value)
        {
          time_ = time(NULL);
          state_change_callback_(serial_num_);
        }
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

}
}
}
