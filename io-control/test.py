#!/usr/bin/python

import socket
import struct
import threading
import logging
import os
import sys, getopt

if os.name == "posix":
  import daemon

class Discovery(threading.Thread):

  def __init__(self):
    logging.debug("initiating discovery")

    self.known_services = {}
    self.notify_received = {}
    self.callbacks = []
    
    self.on_timeout()

    threading.Thread.__init__(self)
    self.start();

  def on_timeout(self):
    self.timer = threading.Timer(10, self.on_timeout)
    self.timer.start()
    
    for s, r in self.notify_received.copy().iteritems():
      if r == False:
        self.erase_service(s)
        
    for s in self.notify_received.iterkeys():
      self.notify_received[s] = False

  def run(self):
    self.cont = True;

    MCAST_GRP = '239.255.255.255'
    MCAST_PORT = 10001

    sock = socket.socket(socket.AF_INET, socket.SOCK_DGRAM, socket.IPPROTO_UDP)
    sock.setsockopt(socket.SOL_SOCKET, socket.SO_REUSEADDR, 1)
    sock.bind(('', MCAST_PORT))
    mreq = struct.pack("4sl", socket.inet_aton(MCAST_GRP), socket.INADDR_ANY)

    sock.setsockopt(socket.IPPROTO_IP, socket.IP_ADD_MEMBERSHIP, mreq)
    sock.settimeout(0.1)

    while self.cont:
      try:
        msg = sock.recv(10240).split('\n')
        if (msg[0] == "notify"): self.handle_notify(msg)
        elif (msg[0] == "hello"): self.handle_hello(msg)
        elif (msg[0] == "bye"): self.handle_bye(msg)
        else: logging.warning("Unknown message: " + msg)
      except socket.timeout:
        pass

  def store_service(self, service, endpoint):
    logging.debug("Storing service: " + service + " (" + endpoint + ")")
    self.known_services[service] = endpoint
    for c in self.callbacks:
      c(service, True)

    
  def erase_service(self, service):
    logging.debug("Erasing service: " + service)
    del self.known_services[service]
    del self.notify_received[service]
    
  def check_service(self, service, endpoint):
    if service not in self.known_services:
      self.store_service(service, endpoint)    
    elif self.known_services[service] != endpoint:
      self.erase_service(service)
      self.store_service(service, endpoint)
    self.notify_received[service] = True

  def handle_notify(self, msg):
    if (len(msg) >= 3):
      self.check_service(msg[1], msg[2])
    
  def handle_bye(self, msg):
    if msg[1] in self.known_services:
      self.erase_service(msg[1])
    
  def handle_hello(self, msg):
    if (len(msg) >= 3):
      self.check_service(msg[1], msg[2])
    
  def exit(self):
    self.cont = False
    self.join()
    self.timer.cancel()
    logging.debug("Discovery exit")

  def register(self, callback):
    self.callbacks.append(callback)

  def get(self, service):
    if service in self.known_services:
      return self.known_services[service]
    else:
      raise Exception("Service not found " + service)

discovery = None

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

  global discovery
  discovery = Discovery()

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

discovery.exit()

logging.info("Home System IO Control quitting")
