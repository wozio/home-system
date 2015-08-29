#!/usr/bin/python

import socket
import struct
import threading
import logging

known_services = {}
notify_received = {}
callbacks = []
timer = None

def init():
  logging.debug("initiating discovery")

  on_timeout()

  global cont, thread
  cont = True
  thread = threading.Thread(target=run)
  thread.start();

def on_timeout():
  global timer, notify_received

  timer = threading.Timer(10, on_timeout)
  timer.start()

  global notify_received
  for s, r in notify_received.copy().iteritems():
    if r == False:
      erase_service(s)

  for s in notify_received.iterkeys():
    notify_received[s] = False

def run():
  global cont
  cont = True;

  MCAST_GRP = '239.255.255.255'
  MCAST_PORT = 10001

  sock = socket.socket(socket.AF_INET, socket.SOCK_DGRAM, socket.IPPROTO_UDP)
  sock.setsockopt(socket.SOL_SOCKET, socket.SO_REUSEADDR, 1)
  sock.bind(('', MCAST_PORT))
  mreq = struct.pack("4sl", socket.inet_aton(MCAST_GRP), socket.INADDR_ANY)

  sock.setsockopt(socket.IPPROTO_IP, socket.IP_ADD_MEMBERSHIP, mreq)
  sock.settimeout(0.1)

  while cont:
    try:
      msg = sock.recv(10240).split('\n')
#      logging.debug("Message: " + '||'.join(msg))
      if (len(msg) > 0):
        if (msg[0] == "notify"): handle_notify(msg)
        elif (msg[0] == "hello"): handle_hello(msg)
        elif (msg[0] == "bye"): handle_bye(msg)
        else: logging.warning("Unknown message: " + '||'.join(msg))
    except:
      pass

def store_service(service, endpoint):
  #logging.debug("Storing service: " + service + " (" + endpoint + ")")
  global known_services, callbacks
  known_services[service] = endpoint
  for c in callbacks:
    c(service, True)


def erase_service( service):
  #logging.debug("Erasing service: " + service)
  global known_services, callbacks, notify_received
  del known_services[service]
  del notify_received[service]
  for c in callbacks:
    c(service, False)

def check_service(service, endpoint):
  global known_services, notify_received
  if service not in known_services:
    store_service(service, endpoint)
  elif known_services[service] != endpoint:
    erase_service(service)
    store_service(service, endpoint)
  notify_received[service] = True

def handle_notify(msg):
  if (len(msg) >= 3):
    check_service(msg[1], msg[2])

def handle_bye(msg):
  if msg[1] in known_services:
    erase_service(msg[1])

def handle_hello(msg):
  if (len(msg) >= 3):
    check_service(msg[1], msg[2])

def exit():
  global cont, timer, thread
  cont = False
  thread.join()
  timer.cancel()
  logging.debug("Discovery exit")

def register(callback):
  callbacks.append(callback)
  for s in known_services.copy():
    callback(s, True)

def get(service):
  if service in known_services:
    return known_services[service]
  else:
    raise Exception("Service not found " + service)
