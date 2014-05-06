#include "discovery.h"
#include "yamicontainer.h"
#include "logger.h"
#include "service.h"
#include "app.h"
#include "discovery.h"
#include "yamicontainer.h"
#include <boost/lexical_cast.hpp>
#include <fstream>
#include <iostream>
#include <memory>

home_system::yami_container _yc;
home_system::discovery _discovery;

using namespace std;

class test_service
  : public home_system::service
{
private:
  ofstream f_;
  size_t received_;

public:
  test_service()
    : f_("test.ts", ofstream::binary),
    service("test"),
    received_(0)
  {
  }
  ~test_service(){};

  int start(int channel)
  {
    yami::parameters params;

    params.set_integer("channel", channel);
    params.set_string("destination", "test");
    params.set_string("endpoint", YC.endpoint());

    unique_ptr<yami::outgoing_message> message(AGENT.send(DISCOVERY.get("tv"), "tv", "create_client_session", params));

    message->wait_for_completion(1000);

    if (message->get_state() == yami::replied)
    {
      return message->get_reply().get_integer("session");
    }
    return -1;
  }
  
  void stop(int session)
  {
    yami::parameters params;

    params.set_integer("session", session);

    unique_ptr<yami::outgoing_message> message(AGENT.send(DISCOVERY.get("tv"), "tv", "delete_client_session", params));
    
    message->wait_for_completion(1000);
  }
  
  void change(int session, int channel)
  {
    stop(session);
    start(channel);
  }

  void on_msg(yami::incoming_message & im)
  {
    if (im.get_message_name() == "stream_part")
    {
      try
      {
        size_t len = 0;
        const char* buf = (const char*)im.get_parameters().get_binary("payload", len);

        f_.write(buf, len);

        received_ += len;
        
        static int m = 0;
        if (++m == 10)
        {
          m = 0;
          LOG("Received " << received_ << " bytes");
        }
      }
      catch (const std::exception& e)
      {
        LOG("Exception: " << e.what());
      }
    }
    else
    {
      home_system::service::on_msg(im);
    }
  }
};

test_service* s;

void cmd_handler(const std::vector<string>& fields)
{
  if (fields.size() >= 1)
  {
    if (fields[0] == "on")
    {
      try
      {
        string ye = DISCOVERY.get("dhwrec");
        yami::parameters params;
        params.set_boolean("state", true);
        AGENT.send(ye, "dhwrec", "set_state", params);
      }
      catch (const home_system::service_not_found&)
      {
        LOG("dhwrec service not found");
      }
    }
    else if (fields[0] == "off")
    {
      try
      {
        string ye = DISCOVERY.get("dhwrec");
        yami::parameters params;
        params.set_boolean("state", false);
        AGENT.send(ye, "dhwrec", "set_state", params);
      }
      catch (const home_system::service_not_found&)
      {
        LOG("dhwrec service not found");
      }
    }
    else if (fields[0] == "ls")
    {
      try
      {
        std::map<std::string,std::string> services;
        DISCOVERY.get_all(services);
        int j = 0;
        for (std::map<std::string,std::string>::iterator i = services.begin(); i != services.end(); ++i)
        {
          j++;
          cout << j << ". " << i->first << " (" << i->second << ")" << endl;
        }
      }
      catch (const home_system::service_not_found&)
      {
      }
    }
    else if (fields[0] == "start")
    {
      if (fields.size() > 1)
      {
        try
        {
          int channel = boost::lexical_cast<int>(fields[1]);
          cout << "Session: " << s->start(channel) << endl;
        }
        catch (const std::exception& e)
        {
          LOGWARN("EXCEPTION: " << e.what());
        }
      }
    }
    else if (fields[0] == "stop")
    {
      if (fields.size() > 1)
      {
        try
        {
          int session = boost::lexical_cast<int>(fields[1]);
          s->stop(session);
        }
        catch (const std::exception& e)
        {
          LOGWARN("EXCEPTION: " << e.what());
        }
      }
    }
    else if (fields[0] == "change")
    {
      if (fields.size() > 2)
      {
        try
        {
          int session = boost::lexical_cast<int>(fields[1]);
          int channel = boost::lexical_cast<int>(fields[2]);
          s->change(session, channel);
        }
        catch (const std::exception& e)
        {
          LOGWARN("EXCEPTION: " << e.what());
        }
      }
    }
    else if (fields[0] == "list")
    {
      if (fields.size() >= 2)
      {
        if (fields[1] == "channels")
        {
          try
          {
            string ye = DISCOVERY.get("tv");
            unique_ptr<yami::outgoing_message> message(AGENT.send(ye, "tv", "get_channels"));
            message->wait_for_completion(1000);
            if (message->get_state() == yami::replied)
            {
              size_t size = 0;
              int* ids = message->get_reply().get_integer_array("channel", size);
              cout << "Number of channels: " << size << endl;
              for (size_t i = 0; i < size; ++i)
              {
                cout << ids[i] << ". " <<  message->get_reply().get_string_in_array("name", i) << endl;
              }
            }
            else
            {
              LOG("Message not replied");
            }
          }
          catch (const home_system::service_not_found& e)
          {
            LOG("TV service not found");
          }
        }
        else if (fields[1] == "records")
        {
          try
          {
            string ye = DISCOVERY.get("tv");
            unique_ptr<yami::outgoing_message> message(AGENT.send(ye, "tv", "get_records"));
            message->wait_for_completion(1000);
            if (message->get_state() == yami::replied)
            {
              size_t size = 0;
              int* ids = message->get_reply().get_integer_array("record_ids", size);
              int* states = message->get_reply().get_integer_array("states", size);
              cout << "Number of records: " << size << endl;
              for (size_t i = 0; i < size; ++i)
              {
                cout << ids[i] << ". " <<  message->get_reply().get_string_in_array("name", i) << " state=" << states[i] << endl;
              }
            }
            else
            {
              LOG("Message not replied");
            }
          }
          catch (const home_system::service_not_found& e)
          {
            LOG("TV service not found");
          }
        }
        else if (fields[1] == "epg")
        {
          if (fields.size() >= 3)
          {
            try
            {
              int channel = boost::lexical_cast<int>(fields[2]);
              yami::parameters param;
              param.set_integer("channel", channel);
              unique_ptr<yami::outgoing_message> message(AGENT.send(DISCOVERY.get("tv"), "tv", "get_epg_data", param));
              message->wait_for_completion(1000);
              if (message->get_state() == yami::replied)
              {
                size_t size = message->get_reply().get_integer("event_num");
                cout << "Number of events: " << size << endl;
                if (size > 0)
                {
                  int* ids = message->get_reply().get_integer_array("id", size);
                  int* durations = message->get_reply().get_integer_array("duration", size);
                  long long* start_times = message->get_reply().get_long_long_array("start_time", size);

                  for (size_t i = 0; i < size; ++i)
                  {
                    cout << ids[i] << ". " <<  message->get_reply().get_string_in_array("name", i)
                      << " (start_time=" << start_times[i] << ", duration=" << durations[i] << ")" << endl;
                  }
                }
              }
              else
              {
                LOG("Message not replied");
              }
            }
            catch (const home_system::service_not_found& e)
            {
              LOG("TV service not found");
            }
          }
        }
      }
    }
    // cmd: record start <channel_num>
    // cmd: record stop <record_id>
    else if (fields[0] == "recording" || fields[0] == "r")
    {
      if (fields.size() >= 3)
      {
        if (fields[1] == "start")
        {
          try
          {
            int channel = boost::lexical_cast<int>(fields[2]);
            yami::parameters param;
            param.set_integer("channel_id", channel);
            unique_ptr<yami::outgoing_message> message(AGENT.send(DISCOVERY.get("tv"), "tv", "start_recording", param));
            message->wait_for_completion(1000);
            if (message->get_state() == yami::replied)
            {
              cout << "Recording id: " << message->get_reply().get_integer("recording_id") << endl;
            }
            else if (message->get_state() == yami::rejected)
            {
              cout << "Message rejected: " << message->get_exception_msg() << endl;
            }
          }
          catch (const std::exception& e)
          {
            cout << "EXCEPTION: " << e.what() << endl;
          }
        }
        else if (fields[1] == "stop")
        {
          try
          {
            int record_id = boost::lexical_cast<int>(fields[2]);
            yami::parameters param;
            param.set_integer("recording_id", record_id);
            AGENT.send(DISCOVERY.get("tv"), "tv", "stop_recording", param);
          }
          catch (const std::exception& e)
          {
            cout << "EXCEPTION: " << e.what() << endl;
          }
        }
        else if (fields[1] == "schedule")
        {
          if (fields.size() >= 4)
          {
            try
            {
              int channel = boost::lexical_cast<int>(fields[2]);
              int event = boost::lexical_cast<int>(fields[3]);
              yami::parameters param;
              param.set_integer("channel", channel);
              param.set_integer("id", event);
              unique_ptr<yami::outgoing_message> message(AGENT.send(DISCOVERY.get("tv"), "tv", "schedule_event_record", param));
              message->wait_for_completion(1000);
              if (message->get_state() == yami::replied)
              {
                cout << "Recording id: " << message->get_reply().get_integer("recording_id") << endl;
              }
              else if (message->get_state() == yami::rejected)
              {
                cout << "Message rejected: " << message->get_exception_msg() << endl;
              }
            }
            catch (const std::exception& e)
            {
              cout << "EXCEPTION: " << e.what() << endl;
            }
          }
        }
      }
    }
  }
}

int main(int argc, char** argv)
{
  home_system::logger::configure("test.log", "debug", true);

  LOGINFO("Started");

  DISCOVERY;

  s = new test_service;

  home_system::app app(false, cmd_handler);

  app.run();

  LOGINFO("Quitting");

  return 0;
}

