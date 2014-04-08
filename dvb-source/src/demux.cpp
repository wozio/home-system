#include "demux.h"

#include "logger.h"
#include "channels.h"

#include <libdvbapi/dvbdemux.h>
#include <libucsi/dvb/section.h>
#include <libucsi/dvb/descriptor.h>

#include <iconv.h>

using namespace std;

namespace home_system
{
namespace media
{

demux::demux(int adapter, int demux, channels& channels, transponders& transponders)
: adapter_(adapter),
  demux_(demux),
  channels_(channels),
  transponders_(transponders),
  send_event_info_(false),
  state_(demux_state::idle)
{
}

demux::~demux()
{
  state_callback_ = nullptr;
  ei_callback_ = nullptr;
  reset_mux();
  timer_.cancel();
}

void demux::set_state_callback(state_callback_t callback)
{
  state_callback_ = callback;
}

void demux::set_ei_callback(ei_callback_t callback)
{
  ei_callback_ = callback;
}

int demux::set_channel(channel_t c, dvb::session_callback_t callback)
{
  session_callback_ = callback;
  channel_ = c;
  
  set_mux();
  return 0;
}

void demux::set_mux()
{
  LOG("Setting demux");
  
  lock_guard<mutex> lock(state_mutex_);
  
  LOG("Setting PAT filter");
  pat_version_number_ = 0xFF;
  if ((create_section_filter(TRANSPORT_PAT_PID, stag_mpeg_program_association, 0xFF,
    [this] (section* s){check_pat(s);})) < 0)
  {
    LOGWARN("Failed to set PAT filter")
  }
  
  LOG("Setting SDT filter");
  sdt_version_number_ = 0xFF;
  if ((create_section_filter(TRANSPORT_SDT_PID, stag_dvb_service_description_actual, 0xff,
    [this] (section* s){check_sdt(s);})) < 0)
  {
    LOGWARN("Failed to set SDT filter");
  }
  
  LOG("Setting EIT filter");
  
  if ((create_section_filter(TRANSPORT_EIT_PID, 0x6f, 0xc0,
    [this] (section* s){check_eit(s);})) < 0)
  {
    LOGWARN("Failed to set EIT filter");
  }
  
  /*LOG("Setting NIT filter");
  nit_version_number_ = 0xFF;
  if ((create_section_filter(TRANSPORT_NIT_PID, stag_dvb_network_information_actual, 0xff,
    [this] (section* s){check_nit(s);})) < 0)
  {
    LOGWARN("Failed to set NIT filter");
  }
    
  if ((rst_fd_ = create_section_filter(TRANSPORT_RST_PID, stag_dvb_running_status, 0xff)) < 0)
  {
    LOGWARN("Failed to set RST filter");
    rst_fd_ = -1;
  }
  if ((tot_fd_ = create_section_filter(TRANSPORT_TOT_PID, stag_dvb_time_offset, 0xff)) < 0)
  {
    LOGWARN("Failed to set TOT filter");
    tot_fd_ = -1;
  }
  if ((tdt_fd_ = create_section_filter(TRANSPORT_TDT_PID, stag_dvb_time_date, 0xff)) < 0)
  {
    LOGWARN("Failed to set TDT filter");
    tdt_fd_ = -1;
  }*/

  
  change_state(demux_state::mux_set);
  set_timer(10);
}

void demux::reset_mux()
{
  lock_guard<mutex> lock(state_mutex_);
  
  if (state_ != demux_state::idle)
  {
    LOG("Resetting demux");
    
    for (auto i : pollfds_)
    {
      close(i.fd);
    }
    
    file_reader_.reset();
    
    for (auto i : pid_fds_)
    {
      close(i);
    }

    section_callbacks_.clear();
    pollfds_.clear();

    channel_ = nullptr;

    change_state(demux_state::idle);
  }
}

void demux::change_state(demux_state new_state)
{
  if (new_state != state_)
  {
    state_ = new_state;
    if (state_callback_ != nullptr)
    {
      state_callback_(state_);
    }
  }
}

void demux::set_timer(int duration)
{
  timer_.set_from_now(duration, [&] () {check();});
}

int demux::create_section_filter(uint16_t pid, uint8_t table, uint8_t bitmask, section_callback_t section_callback)
{
  int demux_fd = -1;
  uint8_t filter[18];
  uint8_t mask[18];

  // open the demuxer
  if ((demux_fd = dvbdemux_open_demux(adapter_, demux_, 1)) < 0)
  {
    LOGWARN("Failed to open demux");
    return -1;
  }

  // create a section filter
  memset(filter, 0, sizeof(filter));
  memset(mask, 0, sizeof(mask));
  filter[0] = table;
  mask[0] = bitmask;
  if (dvbdemux_set_section_filter(demux_fd, pid, filter, mask, 1, 1))
  {
    LOGWARN("Failed to set section filter");
    close(demux_fd);
    return -1;
  }
  
  pollfd newpollfd;
  newpollfd.fd = demux_fd;
  newpollfd.events = POLLIN|POLLPRI|POLLERR;
  pollfds_.push_back(newpollfd);
  section_callbacks_[demux_fd] = section_callback;
  
  return demux_fd;
}

void demux::remove_section_filter(int fd)
{
  section_callbacks_.erase(fd);
  for (size_t i = 0; i < pollfds_.size(); ++i)
  {
    if (pollfds_[i].fd == fd)
    {
      pollfds_.erase(pollfds_.begin() + i);
      break;
    }
  }
  close(fd);
}

int demux::create_pid_filter(uint16_t pid)
{
  int demux_fd = -1;

  if ((demux_fd = dvbdemux_open_demux(adapter_, demux_, 1)) < 0)
  {
    return -1;
  }

  if (dvbdemux_set_pid_filter(demux_fd, pid, DVBDEMUX_INPUT_FRONTEND, DVBDEMUX_OUTPUT_DVR, 1))
  {
    close(demux_fd);
    return -1;
  }

  return demux_fd;
}

void demux::check()
{
  lock_guard<mutex> lock(state_mutex_);
  
  switch (state_)
  {
  case demux_state::idle:
    break;
  case demux_state::mux_set:
  case demux_state::sdt:
  case demux_state::pat:
  case demux_state::pmts:
    poll_filters();
    break;
  }
}

void demux::poll_filters()
{
  static uint8_t buf[4096];
  
  int count = poll(&pollfds_[0], pollfds_.size(), 1000);
  
  if (count < 0)
  {
    LOGWARN("Poll error");
    return;
  }

  if (count == 0)
    return;
  
  for (size_t i = 0; i < pollfds_.size(); ++i)
  {
    if (!(pollfds_[i].revents & (POLLIN|POLLPRI)))
      continue;

    // read the sections
    int size = ::read(pollfds_[i].fd, buf, 4096);
    if (size > 0)
    {
      section* s = section_codec(buf, size);
      if (!s)
        continue;
      section_callbacks_[pollfds_[i].fd](s);
    }
  }
  set_timer(10);
}

void demux::check_sdt(section* s)
{
  section_ext* section_ext = section_ext_decode(s, 0);
  if (section_ext == NULL)
    return;
  
  if (section_ext->current_next_indicator == 0)
    return;
  
  if (section_ext->version_number == sdt_version_number_)
    return;
  
  sdt_version_number_ = section_ext->version_number;
  
  LOG("We have a SDT, parsing...");
   
  dvb_sdt_section* sdtsec = dvb_sdt_section_codec(section_ext);
  if (sdtsec != NULL)
  {
    
    uint64_t onid = sdtsec->original_network_id;
    uint64_t tsid = dvb_sdt_section_transport_stream_id(sdtsec);
    dvb_sdt_service* curser;
    dvb_sdt_section_services_for_each(sdtsec, curser)
    {
      // TODO fix me, should be configurable when CA support will be added
      if (curser->free_ca_mode == 0)
      {
        descriptor* cd;
        dvb_sdt_service_descriptors_for_each(curser, cd)
        {
          switch (cd->tag)
          {
          case dtag_dvb_service:
            dvb_service_descriptor* sd = dvb_service_descriptor_codec(cd);
            if (sd != NULL)
            {
              switch (sd->service_type)
              {
              case DVB_SERVICE_TYPE_DIGITAL_TV:
              case DVB_SERVICE_TYPE_DIGITAL_RADIO:
              case DVB_SERVICE_TYPE_MPEG2_HD_DIGITAL_TV:
              case DVB_SERVICE_TYPE_ADVANCED_CODEC_SD_DIGITAL_TV:
              case DVB_SERVICE_TYPE_ADVANCED_CODEC_HD_DIGITAL_TV:
                struct dvb_service_descriptor_part2* sd2 = dvb_service_descriptor_part2(sd);
                if (sd2 != NULL)
                {
                  int offset;
                  const char* charset = dvb_charset((char*)dvb_service_descriptor_service_name(sd2), sd2->service_name_length, &offset);
                  iconv_t cd = iconv_open("UTF-8", charset);
                  if (cd != (iconv_t)(-1))
                  {
                    size_t outbytesleft = 1024;
                    size_t inbytesleft =  sd2->service_name_length - offset;
                    char outbuf[1024];
                    char* inbufp = (char*)dvb_service_descriptor_service_name(sd2) + offset;
                    char* outbufp = outbuf;
                    if (iconv(cd, &inbufp, &inbytesleft, &outbufp, &outbytesleft) != (size_t)(-1))
                    {
                      uint64_t nid = (onid << 32) + (tsid << 16) + curser->service_id;
                      
                      string name(outbuf, outbufp - outbuf);

                      LOG(hex << "Local channel ID: " << nid << ", name: " << name);
                      channels_.add(nid, name, curser->service_id);
                      //if (channels_.count(nid))
                      //{
                        //channels_[nid].frequency_ = current_transponder_;
                        //channels_[nid].name_ = string(outbuf, outbufp - outbuf);
                        //channels_[nid].service_id_ = curser->service_id;
                      //}
                      //else
                      //{
                        //channel new_ch;
                        //new_ch.frequency_ = current_transponder_;
                        //new_ch.name_ = string(outbuf, outbufp - outbuf);
                        //new_ch.service_id_ = curser->service_id;
                        //new_ch.id_ = (onid << 32) + (tsid << 16) + curser->service_id;
                        //channels_[new_ch.id_] = new_ch;
                        //send_new_channel(new_ch.id_);
                      //}
                    }
                  }
                  iconv_close(cd);
                }
              }
            }
            break;
          }
        }
      }
    }
  }
  change_state(demux_state::sdt);
}

//struct mysection
//{
//  int version;
//};
//
//struct myservice
//{
//  map<int, mysection> sections;
//};
//
//struct table
//{
//  map<uint64_t, myservice> services;
//};

void demux::check_eit(section* s)
{
  if (ei_callback_ == nullptr)
    return;

  //static map<int, table> tables;
  //ofstream f("eit.txt");

  if (s->table_id < 0x4e || s->table_id > 0x6f)
    return;

  // parse section_ext
  section_ext* se = section_ext_decode(s, 0);
  if (se == NULL)
    return;

  if (se->current_next_indicator == 0)
    return;
  
  dvb_eit_section* eit = dvb_eit_section_codec(se);
  if (eit == NULL)
    return;

  event_info ei;
  
  //int tab = section_ext->table_id;
  uint64_t onid = eit->original_network_id; // original network id
  uint64_t tsid = eit->transport_stream_id; // transport stream id
  ei.channel_ = (onid << 32) + (tsid << 16) + se->table_id_ext;
  
  // just for checking if such channel exists
  try
  {
    channels_.get(ei.channel_);
  }
  catch (const runtime_error&)
  {
    return;
  }
  
  //LOG(hex << ser);
  //int sec = section_ext->section_number;
  //int ver = section_ext->version_number;
  
//  if (tables.find(tab) == tables.end())
//  {
//    table t;
//    tables[tab] = t;
//  }
//  if (tables[tab].services.find(ser) == tables[tab].services.end())
//  {
//    myservice s;
//    tables[tab].services[ser] = s;
//  }
//  if (tables[tab].services[ser].sections.find(sec) == tables[tab].services[ser].sections.end())
//  {
//    mysection s;
//    s.version = -1;
//    tables[tab].services[ser].sections[sec] = s;
//  }
  
  //if (tables[tab].services[ser].sections[sec].version == ver)
  //  return;

  //tables[tab].services[ser].sections[sec].version = ver;

//  LOG("table_id: " << (int)section->table_id
//    << " service (table_id_ext): " << ser
//    << " section_number: " << sec
//    << " version_number: " << ver);
  
  dvb_eit_event* cur_event;
  dvb_eit_section_events_for_each(eit, cur_event)
  {
    ei.event_ = cur_event->event_id;
//    ei.start_time_ = dvbdate_to_unixtime(cur_event->start_time);
    {
      /* check for the undefined value */
      if ((cur_event->start_time[0] == 0xff) &&
          (cur_event->start_time[1] == 0xff) &&
          (cur_event->start_time[2] == 0xff) &&
          (cur_event->start_time[3] == 0xff) &&
          (cur_event->start_time[4] == 0xff))
      {
        ei.start_time_ = -1;
      }
      else
      {
        int k = 0;
        struct tm tm;
        double mjd;

        memset(&tm, 0, sizeof(tm));
        mjd = (cur_event->start_time[0] << 8) | cur_event->start_time[1];

        tm.tm_year = (int) ((mjd - 15078.2) / 365.25);
        tm.tm_mon = (int) (((mjd - 14956.1) - (int) (tm.tm_year * 365.25)) / 30.6001);
        tm.tm_mday = (int) mjd - 14956 - (int) (tm.tm_year * 365.25) - (int) (tm.tm_mon * 30.6001);
        if ((tm.tm_mon == 14) || (tm.tm_mon == 15))
          k = 1;
        tm.tm_year += k;
        tm.tm_mon = tm.tm_mon - 2 - k * 12;
        tm.tm_sec = bcd_to_integer(cur_event->start_time[4]);
        tm.tm_min = bcd_to_integer(cur_event->start_time[3]);
        tm.tm_hour = bcd_to_integer(cur_event->start_time[2]);
        tm.tm_isdst = -1;
        // gmtoff added since mktime treats tm structure as in local time but it is in UTC from DVB
        ei.start_time_ = mktime(&tm) + tm.tm_gmtoff;
      }
    }
    ei.duration_ = dvbduration_to_seconds(cur_event->duration);
//    char buff[20];
//    strftime(buff, 20, "%Y-%m-%d %H:%M:%S", localtime(&t));
//    LOG("Service: " << dvb_eit_section_service_id(eit) << 
//      " ID:" << cur_event->event_id <<
//      " [" << buff <<
//      "(" << dvbduration_to_seconds(cur_event->duration) << ")]");
    
    descriptor* desc;
    dvb_eit_event_descriptors_for_each(cur_event, desc)
    {
      switch (desc->tag)
      {
      case dtag_dvb_short_event:
        {
          dvb_short_event_descriptor* sed = dvb_short_event_descriptor_codec(desc);
          if (sed != NULL)
          {
            int offset;
            const char* charset = dvb_charset((char*)dvb_short_event_descriptor_event_name(sed), sed->event_name_length, &offset);
            iconv_t cd = iconv_open("UTF-8", charset);
            if (cd != (iconv_t)(-1))
            {
              size_t outbytesleft = 1024;
              size_t inbytesleft =  sed->event_name_length - offset;
              char outbuf[1024];
              char* inbufp = (char*)dvb_short_event_descriptor_event_name(sed) + offset;
              char* outbufp = outbuf;
              if (iconv(cd, &inbufp, &inbytesleft, &outbufp, &outbytesleft) != (size_t)(-1))
              {
                ei.title_.assign(outbuf, outbufp - outbuf);
//                if (ser == 1)
                  //LOG(string(outbuf, outbufp - outbuf));
              }
            }
            iconv_close(cd);
            
            struct dvb_short_event_descriptor_part2* sedp2 = dvb_short_event_descriptor_part2(sed);
            
            if (sedp2 != NULL)
            {
              int offset;
              const char* charset = dvb_charset((char*)dvb_short_event_descriptor_text(sedp2), sedp2->text_length, &offset);
              iconv_t cd = iconv_open("UTF-8", charset);
              if (cd != (iconv_t)(-1))
              {
                size_t outbytesleft = 1024;
                size_t inbytesleft =  sedp2->text_length - offset;
                char outbuf[1024];
                char* inbufp = (char*)dvb_short_event_descriptor_text(sedp2) + offset;
                char* outbufp = outbuf;
                if (iconv(cd, &inbufp, &inbytesleft, &outbufp, &outbytesleft) != (size_t)(-1))
                {
                  ei.description_.assign(outbuf, outbufp - outbuf);
                  //LOG(string(outbuf, outbufp - outbuf));
                }
              }
              iconv_close(cd);
            }
          }
        }
        break;
      case dtag_dvb_extended_event:
        break;
      }
    }
    //channels_[ser].events_[ei.event_id_] = ei;
    
    if (ei_callback_ != nullptr)
    {
      ei_callback_(ei);
    }
  }
}

void demux::check_pat(section* s)
{
  // we don't need PMT from PAT when there is no channel set
  if (channel_ == nullptr)
  {
    return;
  }
  
  // parse section_ext
  section_ext* se = section_ext_decode(s, 0);
  if (se == NULL)
    return;
  
  if (se->current_next_indicator == 0)
    return;
  
  if (se->version_number == pat_version_number_)
    return;
  
  pat_version_number_ = se->version_number;

  mpeg_pat_section* pat = mpeg_pat_section_codec(se);
  if (pat == NULL)
    return;
  
  LOG("We have a PAT, parsing...");

  // try and find the requested program
  mpeg_pat_program* cur_program;
  mpeg_pat_section_programs_for_each(pat, cur_program)
  {
    if (cur_program->program_number == channel_->service_id())
    {
      LOG("Program found, setting PMT filter");
      
      pmt_version_number_ = 0xFF;

      // create PMT filter
      if ((create_section_filter(cur_program->pid, stag_mpeg_program_map, 0xFF,
       [this] (section* s){check_pmt(s);})) < 0)
      {
        LOGWARN("Failed to set PMT filter")
      }
       
       pmt_pid_ = cur_program->pid;

      // close PAT filter
      //close(pat_fd_);
      
      //fe_state_ = fe_waiting_pmt;
    }
  }
  change_state(demux_state::pat);
}

void demux::check_pmt(section* s)
{
  // parse section_ext
  section_ext* se = section_ext_decode(s, 0);
  if (se == NULL)
    return;

  //if (se->table_id_ext != channels_[current_channel_].service_id_)
    //return;

  if (pmt_version_number_ == se->version_number)
    return;
  
  pmt_version_number_ = se->version_number;
  
  LOG("We have PMT, parsing...");
  
  mpeg_pmt_section* pmt = mpeg_pmt_section_codec(se);
  if (pmt == NULL)
    return;
  
  // adding PIDs which are always needed
  // PAT
  int pid_fd = create_pid_filter(0);
  pid_fds_.push_back(pid_fd);

  // PMT
  pid_fd = create_pid_filter(pmt_pid_);
  pid_fds_.push_back(pid_fd);

  // PCR
  pid_fd = create_pid_filter(pmt->pcr_pid);
  pid_fds_.push_back(pid_fd);

/*  descriptor* desc;
  mpeg_pmt_section_descriptors_for_each(pmt, desc)
  {
  }*/

  // loop on streams in PMT
  mpeg_pmt_stream* stream;
  mpeg_pmt_section_streams_for_each(pmt, stream)
  {
    /*printf("Stream type: %x PID: %x\n", stream->stream_type, stream->pid);

    descriptor* stream_desc;
    mpeg_pmt_stream_descriptors_for_each(stream, stream_desc)
    {
      printf("[%x](%x) ", stream_desc->tag, stream_desc->len);
      for (size_t i = 0; i < stream_desc->len; ++i)
      {
        printf("%x ", *((uint8_t*)stream_desc + 2 + i));
      }
    }
    cout << endl;*/
    
    // creating PID filters for each stream
    pid_fd = create_pid_filter(stream->pid);
    if (pid_fd != -1)
    {
      LOG("Found stream (pid: " << stream->pid << "), creating PID filter");
      pid_fds_.push_back(pid_fd);
    }
  }
  
  // creating file_reader
  file_reader_.reset(new file_reader(adapter_, demux_, session_callback_));
  
  change_state(demux_state::pmts);
}

//void dvb_service::check_tdt(section* section)
//{
//  LOG("TDT");
  
//  dvb_tdt_section *tdtsec = dvb_tdt_section_codec(section);
//  if (tdtsec != NULL)
//  {
    /* check for the undefined value */
/*    if ((tdtsec->utc_time[0] == 0xff) &&
        (tdtsec->utc_time[1] == 0xff) &&
        (tdtsec->utc_time[2] == 0xff) &&
        (tdtsec->utc_time[3] == 0xff) &&
        (tdtsec->utc_time[4] == 0xff))
    {
    }
    else
    {
      int k = 0;
      struct tm tm;
      double mjd;

      memset(&tm, 0, sizeof(tm));
      mjd = (tdtsec->utc_time[0] << 8) | tdtsec->utc_time[1];

      tm.tm_year = (int) ((mjd - 15078.2) / 365.25);
      tm.tm_mon = (int) (((mjd - 14956.1) - (int) (tm.tm_year * 365.25)) / 30.6001);
      tm.tm_mday = (int) mjd - 14956 - (int) (tm.tm_year * 365.25) - (int) (tm.tm_mon * 30.6001);
      if ((tm.tm_mon == 14) || (tm.tm_mon == 15))
        k = 1;
      tm.tm_year += k;
      tm.tm_mon = tm.tm_mon - 2 - k * 12;
      tm.tm_sec = bcd_to_integer(tdtsec->utc_time[4]);
      tm.tm_min = bcd_to_integer(tdtsec->utc_time[3]);
      tm.tm_hour = bcd_to_integer(tdtsec->utc_time[2]);
      LOG(tm.tm_hour << ":" << tm.tm_min << ":" << tm.tm_sec);
      tm.tm_isdst = -1;
      time_t st = mktime(&tm) + tm.tm_gmtoff;

      LOG(st << " t = " << tm.tm_hour << ":" << tm.tm_min << " processed: " << asctime(&tm));

      time_t ct;
      time(&ct);
      LOG(ct << " processed: " << asctime(&tm));
    }
  }*/
//}

void demux::check_nit(section* s)
{
  // parse section_ext
  section_ext* se = section_ext_decode(s, 0);
  if (se == NULL)
    return;
  
  if (se->current_next_indicator == 0)
    return;
  
  if (se->version_number == nit_version_number_)
    return;
  
  nit_version_number_ = se->version_number;

  LOG("We have a NIT, parsing...");
}

}
}
