
#include "logger.h"
#include "owtemp.h"
#include "utils.h"

extern "C"
{
#include "ownet.h"
}

#include <time.h>

using namespace std;

namespace home_system
{
namespace input_output
{
namespace ow
{

temp::temp(int portnum, uchar* serial_num)
: portnum_(portnum),
  serial_num_(serial_num, serial_num + 8),
  history_(8640) // for 24h more or less
{
  LOG("Created temperature device (DS1920): " << serial_num_to_string(serial_num_));
}

void temp::send_convert()
{
  // set the device serial number to the counter device
  owSerialNum(portnum_, &serial_num_[0], FALSE);
  
  // access the device
  if (owAccess(portnum_))
  {
    // send the convert command
    if (!owWriteByte(portnum_, 0x44))
      LOGWARN("Problem writing convert command");
  }
}

void temp::read_temp()
{
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
          LOGWARN("Conversion not ready yet"); // ???
          return;
        }
        if (cpc == 0)
        {
          LOGWARN("CPC == 0");
          return;
        }
        else
          tmp = tmp - (float)0.25 + (cpc - cr)/cpc;
        
        history_entry nhe;
        nhe.time_ = time(NULL);
        nhe.value_ = tmp;
        history_.push_back(nhe);
        LOG(serial_num_to_string(serial_num_) << ": " << tmp);
      }
      else
      {
        LOGWARN("Wrong CRC");
      }
    }
  }
}

void temp::get_history(std::vector<double>& history)
{
  history.clear();
  history.reserve(history_.size());
  
  for (auto he : history_)
  {
    history.push_back(he.value_);
  }
}

}
}
}
