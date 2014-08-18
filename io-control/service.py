#!/usr/bin/env python

import socket
import struct
import threading
import logging
import yami

class service(object):
  def __init__(self, name):
    self.name = name

    # get ip address
    ip = [(s.connect(('8.8.8.8', 80)), s.getsockname()[0], s.close()) for s in [socket.socket(socket.AF_INET, socket.SOCK_DGRAM)]][0][1]

    self.agent = yami.Agent()
    self.ye = self.agent.add_listener("tcp://" + ip + ":*")
    self.agent.register_object(self.name, self.on_msg)
    
    self.timer = threading.Timer(10, self.on_timeout)
    self.timer.start()
    
    self.sock = socket.socket(socket.AF_INET, socket.SOCK_DGRAM, socket.IPPROTO_UDP)
    
    self.send_hello()
    
    logging.info("Created service: %s (%s)", self.name, self.ye)
    
  def exit(self):
    logging.info("Deleting service with name=%s", self.name)
    
    self.timer.cancel()
    
    self.send_bye()
    
    logging.info("Deleted service with name=%s", self.name)

  def on_msg(self, message):
    logging.warning("Unknown message: " + message.get_message_name())
    message.reject("Unknown message: " + message.get_message_name())
    
  def send_hello(self):
    self.send("hello\n" + self.name + "\n" + self.ye)
    
  def send_notify(self):
    self.send("notify\n" + self.name + "\n" + self.ye)
    
  def send_bye(self):
    self.send("bye\n" + self.name)

  def on_timeout(self):
    self.timer = threading.Timer(5, self.on_timeout)
    self.timer.start()
    
    self.send_notify()
    
  def send(self, msg):
    MCAST_GRP = '239.255.255.255'
    MCAST_PORT = 10001
    
    self.sock.sendto(msg, (MCAST_GRP, MCAST_PORT))
