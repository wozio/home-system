#!/usr/bin/python

import threading
import logging
import os
import sys, getopt

# fetch command line arguments
daemonize = False
try:
  if os.name == "posix":
    opt = "hd"
    helpmsg = "io-control.py [-h][-d]"
  else:
    opt = "h"
    helpmsg = "io-control.py [-h]"
  opts, args = getopt.getopt(sys.argv[1:], opt)
except getopt.GetoptError:
  print helpmsg
  sys.exit(2)
for opt, arg in opts:
  if opt == '-h':
    print helpmsg
    sys.exit()
  elif opt == "-d":
    daemonize = True
    
logger = logging.getLogger()
logger.setLevel(logging.DEBUG)
formatter = logging.Formatter('[%(levelname)s] %(asctime)s %(filename)s: %(lineno)d: %(message)s')

fh = logging.FileHandler('io-control.log')
fh.setFormatter(formatter)
logger.addHandler(fh)

if not daemonize:
  ch = logging.StreamHandler()
  ch.setFormatter(formatter)
  logger.addHandler(ch)

logging.info("Starting Home System IO Control")

import discovery
import services
    
if daemonize:
  import daemon
  with daemon.DaemonContext():
    while 1:
      threading.sleep(1)
else:
  print "Enter q to quit..."
  try:
    while 1:
      if raw_input() == "q":
        break
  except KeyboardInterrupt:
    pass
    
logging.info("Home System IO Control quitting")

services.exit()
discovery.exit()
