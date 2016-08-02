#!/usr/bin/env python

import logging
import iomap

class Display:

  def __init__(self, name, data):
    logging.info("Created display '%s'", name)

    self.name = name
    self.type = data["type"]
    self.fromio = data["from"]
    self.change_callback = None
    iomap.Iomap[self.fromio].subscribe(self.on_change)

  def get(self):
    try:
      return iomap.Iomap[self.fromio].get()
    except KeyError:
      logging.error("Attempt to read from not known io (%s %s)",
        self.name, self.fromio)
      return output.state_unknown, 0
    
  def subscribe(self, callback):
    self.change_callback = callback
  
  def on_change(self):
    if self.change_callback != None:
      self.change_callback(self)
