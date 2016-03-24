#include "service.h"
#include "app.h"
#include "discovery.h"
#include "yamicontainer.h"
#include "logger.h"
#include <boost/lexical_cast.hpp>
#include <fstream>
#include <iostream>
#include <memory>

INITIALIZE_EASYLOGGINGPP

home_system::yc_t _yc;
home_system::discovery_t _discovery;

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
    DISCOVERY.subscribe([&] (const string& service, bool available)
    {
      if (available && service == "tv")
      {
        LOG(DEBUG) << "TV service available, registering as client";
        
        AGENT.send(DISCOVERY.get("tv"), "tv", "hello");
      }
    });
  }
  ~test_service(){};

  int start(int channel)
  {
    yami::parameters params;

    params.set_integer("channel", channel);
    params.set_string("destination", "test");
    params.set_string("endpoint", YC.endpoint());
    
    LOG(DEBUG) << "[CLIENT] Create session for channel: " << channel;
    
    unique_ptr<yami::outgoing_message> message(AGENT.send(DISCOVERY.get("tv"), "tv", "create_session", params));
    
    message->wait_for_completion(1000);
    if (message->get_state() == yami::replied)
    {
      LOG(DEBUG) << "[CLIENT] Session created: " << message->get_reply().get_integer("session");
      return message->get_reply().get_integer("session");
    }
    return -1;
  }
  
  void stop(int session)
  {
    LOG(DEBUG) << "[CLIENT] Delete session: " << session;
    
    yami::parameters params;

    params.set_integer("session", session);

    AGENT.send(DISCOVERY.get("tv"), "tv", "delete_session", params);
  }
  
  void pause(int session)
  {
    LOG(DEBUG) << "[CLIENT] Pause session: " << session;
    
    yami::parameters params;

    params.set_integer("session", session);

    AGENT.send(DISCOVERY.get("tv"), "tv", "pause_session", params);
  }
  
  void play(int session)
  {
    LOG(DEBUG) << "[CLIENT] Play session: " << session;
    
    yami::parameters params;

    params.set_integer("session", session);

    AGENT.send(DISCOVERY.get("tv"), "tv", "play_session", params);
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
        
        LOG(DEBUG) << "Received " << len << " bytes, total: " << received_;
      }
      catch (const std::exception& e)
      {
        LOG(DEBUG) << "Exception: " << e.what();
      }
    }
    else if (im.get_message_name() == "session_deleted")
    {
      LOG(DEBUG) << "[CLIENT] session deleted";
    }
    else
    {
      home_system::service::on_msg(im);
    }
  }
};

test_service* s;

class source_service
  : public home_system::service
{
private:
  string destination;
  string endpoint;
  
public:
  source_service()
  : service("test-source")
  {
    DISCOVERY.subscribe([&] (const string& service, bool available)
    {
      if (available && service == "tv")
      {
        LOG(DEBUG) << "TV service available, registering as source";
        
        yami::parameters params;

        params.set_string("name", service::name());

        params.create_string_array("channel_names", 1);
        params.set_string_in_array("channel_names", 0, "test channel");

        vector<long long> ids;
        ids.push_back(12345);
        params.set_long_long_array("channel_ids", &ids[0], ids.size());
        
        AGENT.send(DISCOVERY.get("tv"), "tv", "source_available", params);
      }
    });
  }
  ~source_service(){};

  void on_msg(yami::incoming_message & im)
  {
    if (im.get_message_name() == "create_session")
    {
      long long channel = im.get_parameters().get_long_long("channel");
      destination = im.get_parameters().get_string("destination");
      endpoint = im.get_parameters().get_string("endpoint");
      
      LOG(DEBUG) << "[SOURCE] Create source session: " << channel << " " << destination << " " << endpoint;
      
      yami::parameters params;
      params.set_integer("session", 4321);
      im.reply(params);
    }
    else if (im.get_message_name() == "delete_session")
    {
      int session = im.get_parameters().get_integer("session");
      
      LOG(DEBUG) << "[SOURCE] Delete source session: " << session;
      
      yami::parameters params;

      params.set_integer("session", session);

      AGENT.send(endpoint, destination, "session_deleted", params);
    }
    else
    {
      home_system::service::on_msg(im);
    }
  }
};

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
        LOG(DEBUG) << "dhwrec service not found";
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
        LOG(DEBUG) << "dhwrec service not found";
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
          LOG(DEBUG) << "EXCEPTION: " << e.what();
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
          LOG(DEBUG) << "EXCEPTION: " << e.what();
        }
      }
    }
    else if (fields[0] == "pause")
    {
      if (fields.size() > 1)
      {
        try
        {
          int session = boost::lexical_cast<int>(fields[1]);
          s->pause(session);
        }
        catch (const std::exception& e)
        {
          LOG(DEBUG) << "EXCEPTION: " << e.what();
        }
      }
    }
    else if (fields[0] == "play")
    {
      if (fields.size() > 1)
      {
        try
        {
          int session = boost::lexical_cast<int>(fields[1]);
          s->play(session);
        }
        catch (const std::exception& e)
        {
          LOG(DEBUG) << "EXCEPTION: " << e.what();
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
          LOG(DEBUG) << "EXCEPTION: " << e.what();
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
              LOG(DEBUG) << "Message not replied";
            }
          }
          catch (const home_system::service_not_found&)
          {
            LOG(DEBUG) << "TV service not found";
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
              LOG(DEBUG) << "Message not replied";
            }
          }
          catch (const home_system::service_not_found&)
          {
            LOG(DEBUG) << "TV service not found";
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
                LOG(DEBUG) << "Message not replied";
              }
            }
            catch (const home_system::service_not_found&)
            {
              LOG(DEBUG) << "TV service not found";
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
  LOG(DEBUG) << "Started";

  _yc = home_system::yami_container::create();
  _discovery = home_system::discovery::create();

  unique_ptr<test_service> s(new test_service());
  unique_ptr<source_service> ss(new source_service());

  cout << "Enter q to quit..." << endl;
  std::string input_line;
  while (std::getline(std::cin, input_line))
  {
    if (input_line == "q" || input_line == "quit")
    {
      break;
    }
  }

  LOG(DEBUG) << "Quitting";
  
  s.reset();
  ss.reset();

  _discovery.reset();
  _yc.reset();

  return 0;
}

