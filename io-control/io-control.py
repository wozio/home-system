#!/usr/bin/python

import threading
import logging
import os
import sys, getopt
import discovery
import service

if os.name == "posix":
  import daemon

discovery_ = None

def init(daemonize):
  logger = logging.getLogger()
  logger.setLevel(logging.DEBUG)
  formatter = logging.Formatter('%(asctime)s %(message)s')

  fh = logging.FileHandler('io-control.log')
  fh.setFormatter(formatter)
  logger.addHandler(fh)

  if not daemonize:
    ch = logging.StreamHandler()
    ch.setFormatter(formatter)
    logger.addHandler(ch)

  logging.info("Starting Home System IO Control")

  global discovery_
  discovery_ = discovery.Discovery()

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

if daemonize:
  with daemon.DaemonContext():
    init(True)
    while 1:
      threading.sleep(1)
else:
  init(False)
  service = service.service("jakas usluga")
  print "Enter q to quit..."
  while 1:
    if raw_input() == "q":
      break

discovery_.exit()

logging.info("Home System IO Control quitting")
