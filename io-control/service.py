#!/usr/bin/env python

import socket
import struct
import threading
import logging

class service:
  def __init__(self, name):
    self.name = name
    
    self.timer = threading.Timer(10, self.on_timeout)
    self.timer.start()
    
    self.sock = socket.socket(socket.AF_INET, socket.SOCK_DGRAM, socket.IPPROTO_UDP)
    
    self.send_hello()
    
    logging.info("Created service with name=%s", name)
    
  def exit(self):
    self.timer.cancel()
    
    self.send_bye()
    
    logging.info("Deleted service with name=%s", self.name)
    
  def send_hello(self):
    self.send("hello\n" + self.name + "\n" + "dupa")
    
  def send_notify(self):
    self.send("notify\n" + self.name + "\n" + "dupa")
    
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