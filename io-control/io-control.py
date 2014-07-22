#!/usr/bin/python

import threading
import logging
import os
import sys, getopt
import discovery

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
  opts, args = getopt.getopt(sys.argv[1:], "hd")
except getopt.GetoptError:
  print 'io-control.py [-h][-d]'
  sys.exit(2)
for opt, arg in opts:
  if opt == '-h':
    print 'io-control.py [-h][-d]'
    sys.exit()
  elif opt in ("-d"):
    daemonize = True

if daemonize:
  with daemon.DaemonContext():
    init(True)
    while 1:
      threading.sleep(1)
else:
  init(False)
  print "Enter q to quit..."
  while 1:
    if raw_input() == "q":
      break

discovery_.exit()

logging.info("Home System IO Control quitting")
