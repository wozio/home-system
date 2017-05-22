#include "app.h"

#include "logger.h"
#include <boost/property_tree/json_parser.hpp>
#ifdef __linux__
#include <signal.h>
#include <sys/stat.h>
#endif
#include <cstdlib>
#include <cstdio>
#include <iostream>

using namespace std;

namespace home_system
{
namespace utils
{

boost::property_tree::ptree app::config_;

boost::property_tree::ptree &app::config()
{
  return config_;
}

app::app(const char *conf_file, bool daemonize)
    : app(daemonize)
{
  boost::property_tree::read_json(conf_file, config_);
}

app::app(bool daemonize)
    : daemonize_(daemonize)
{
#ifdef __linux__
  if (daemonize_)
  {
    cout << "Running as daemon" << endl;

    pid_t pid = fork();
    if (pid < 0)
    {
      LOG(ERROR) << "Cannot fork";
      exit(EXIT_FAILURE);
    }
    else if (pid > 0)
    {
      exit(EXIT_SUCCESS);
    }

    umask(0);

    pid_t sid = setsid();
    if (sid < 0)
    {
      LOG(ERROR) << "Cannot setsid";
      exit(EXIT_FAILURE);
    }

    fclose(stdin);
    fclose(stdout);
    fclose(stderr);
  }

#endif
}

app::~app()
{
}

int app::run()
{
#ifdef __linux__
  if (daemonize_)
  {
    sigset_t sset;
    sigemptyset(&sset);
    sigaddset(&sset, SIGQUIT);
    sigaddset(&sset, SIGTERM);
    sigprocmask(SIG_BLOCK, &sset, NULL);
    int sig;
    sigwait(&sset, &sig);
  }
  else
#endif
  {
    cout << "Enter q to quit..." << endl;
    std::string input_line;
    while (std::getline(std::cin, input_line))
    {
      if (input_line == "q" || input_line == "quit")
      {
        break;
      }
    }
  }
  return 0;
}

int app::run(std::function<void()> init_callback, std::function<void()> cleanup_callback)
{
  bool exit_init_loop = false;
  int init_try = 0;
  do
  {
    try
    {
      init_callback();

      exit_init_loop = true;

      run();
    }
    catch (const std::exception & e)
    {
      LOG(ERROR) << "Exception: " << e.what();
    }
    catch (...)
    {
      LOG(ERROR) << "Unknown Exception";
    }

    cleanup_callback();

    if (!exit_init_loop)
    {
      if (++init_try < 60)
      {
        LOG(ERROR) << "Initialization not done, waiting 1 second before next try...";
        this_thread::sleep_for(chrono::seconds(1));
      }
      else
      {
        exit_init_loop = true;
      }
    }
  }
  while (!exit_init_loop);
}

}
}
