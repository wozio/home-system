#!/usr/bin/python

import socket
import struct
import threading
import logging

known_services = {}
callbacks = []
cont = False

def init():
  logging.debug("initiating discovery")

  global cont, thread
  cont = True
  thread = threading.Thread(target=run)
  thread.start()

def run():
  global cont
  cont = True

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
      #logging.debug("Message: " + '||'.join(msg))
      if (len(msg) > 0):
        if (msg[0] == "notify"): handle_notify(msg)
        elif (msg[0] == "hello"): handle_hello(msg)
        elif (msg[0] == "bye"): handle_bye(msg)
        elif (msg[0] == "search"): handle_search()
        else: logging.warning("Unknown message: " + '||'.join(msg))
    except:
      pass

def store_service(service, endpoint):
  logging.debug("Storing service: " + service + " (" + endpoint + ")")
  global known_services, callbacks
  known_services[service] = endpoint
  for c in callbacks:
    c(service, True)


def erase_service( service):
  logging.debug("Erasing service: " + service)
  global known_services, callbacks
  del known_services[service]
  for c in callbacks:
    c(service, False)

def check_service(service, endpoint):
  global known_services
  if service not in known_services:
    store_service(service, endpoint)
  elif known_services[service] != endpoint:
    erase_service(service)
    store_service(service, endpoint)

def handle_notify(msg):
  if (len(msg) >= 3):
    check_service(msg[1], msg[2])

def handle_bye(msg):
  if msg[1] in known_services:
    erase_service(msg[1])

def handle_hello(msg):
  if (len(msg) >= 3):
    check_service(msg[1], msg[2])
    
def handle_search():
  MCAST_GRP = '239.255.255.255'
  MCAST_PORT = 10001

  sock = socket.socket(socket.AF_INET, socket.SOCK_DGRAM, socket.IPPROTO_UDP)
  for s, e in known_services.iteritems():
    msg = "notify\n" + s + "\n" + e
    sock.sendto(msg, (MCAST_GRP, MCAST_PORT))

def exit():
  global cont, thread
  cont = False
  thread.join()
  logging.debug("Discovery exit")

def register(callback):
  callbacks.append(callback)
  for s in known_services.copy():
    callback(s, True)

def get(service):
  if service in known_services:
    return known_services[service]
  else:
    raise RuntimeError("Service not found " + service)
