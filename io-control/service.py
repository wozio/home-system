#!/usr/bin/env python

import socket
import threading
import logging
import yagent
from random import randint

class service(object):
  def __init__(self, name, on_msg_callback):
    self.name = name
    self.on_msg_callback = on_msg_callback

    yagent.agent.register_object(self.name, self.on_msg_callback)

    self.timer = threading.Timer(randint(1, 5), self.on_timeout)
    self.timer.start()

    self.sock = socket.socket(socket.AF_INET, socket.SOCK_DGRAM, socket.IPPROTO_UDP)

    self.send_hello()

    logging.info("Created service: %s (%s)", self.name, yagent.endpoint)
  
  def __del__(self):
    self.exit()

  def exit(self):
    logging.info("Deleting service with name=%s", self.name)

    self.timer.cancel()

    self.send_bye()

    logging.info("Deleted service with name=%s", self.name)

  def on_msg(self, message):
    logging.warning("[%s] Unknown message: %s", self.name, message.get_message_name())
    message.reject("Unknown message (" + message.get_message_name() + ") received by service " + self.name)

  def send_hello(self):
    self.send("hello\n" + self.name + "\n" + yagent.endpoint)

  def send_notify(self):
    global ye
    self.send("notify\n" + self.name + "\n" + yagent.endpoint)

  def send_bye(self):
    self.send("bye\n" + self.name)

  def on_timeout(self):
    self.timer = threading.Timer(randint(1, 5), self.on_timeout)
    self.timer.start()

    self.send_notify()

  def send(self, msg):
    MCAST_GRP = '239.255.255.255'
    MCAST_PORT = 10001
    
    self.sock.sendto(msg, (MCAST_GRP, MCAST_PORT))
