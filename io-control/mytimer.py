#!/usr/bin/env python

import struct
import logging
import threading
import datetime

class timer:

  def __init__(self):
    self.callbacks = []
    self.ready = False
    self.name = "Timer"

    logging.info("Created input '%s'", "Timer")

    self.timer = threading.Timer(1, self.on_timeout)
    self.timer.start()

    self.ready = True

  def exit(self):
    self.timer.cancel()

  def on_timeout(self):

    # restart the timer
    self.timer = threading.Timer(1, self.on_timeout)
    self.timer.start()
    for c in self.callbacks:
      c()

  def get(self):
    return datetime.datetime.now()

  def subscribe(self, callback):
    self.callbacks.append(callback)
    callback()
