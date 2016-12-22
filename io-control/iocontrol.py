#!/usr/bin/python

import os
import sys
import getopt
import time
import logging

def app_exit(arg1, arg2):
  app.exit()
  sys.exit(0)

# fetch command line arguments
daemonize = False
try:
  if os.name == "posix":
    opt = "hd"
    helpmsg = "iocontrol.py [-h][-d]"
  else:
    opt = "h"
    helpmsg = "iocontrol.py [-h]"
  opts, args = getopt.getopt(sys.argv[1:], opt)
except getopt.GetoptError:
  print(helpmsg)
  sys.exit(2)
for opt, arg in opts:
  if opt == '-h':
    print(helpmsg)
    sys.exit()
  elif opt == "-d":
    daemonize = True

if daemonize:
  print("Running as a daemon")
  import daemon
  import signal

  context = daemon.DaemonContext(working_directory=os.getcwd())

  context.signal_map = {
    signal.SIGTERM: app_exit,
  }

  with context:
    import app
    app.init(daemonize)
    while 1:
      time.sleep(1)
else:
  print("Enter q to quit...")
  import app
  app.init(daemonize)
  try:
    while 1:
      if raw_input() == "q":
        break
  except KeyboardInterrupt:
    pass

app_exit(None, None)
